package ammonite.compiler

import java.util.function.Function

import ammonite.compiler.iface.{CodeSource, CodeWrapper, Imports, Preprocessor => IPreprocessor}
import ammonite.util._
import ammonite.util.InterfaceExtensions._

import scala.reflect.internal.Flags
import scala.tools.nsc.{Global => G}

object DefaultPreprocessor {
  case class Expanded(code: String, printer: Seq[String])

  private def isPrivate(tree: G#Tree): Boolean =
    tree match {
      case m: G#MemberDef => m.mods.isPrivate
      case _ => false
    }

  def wrapCode(codeSource: CodeSource,
               indexedWrapperName: Name,
               code: String,
               printCode: String,
               imports: Imports,
               extraCode: String,
               markScript: Boolean,
               codeWrapper: CodeWrapper) = {

    //we need to normalize topWrapper and bottomWrapper in order to ensure
    //the snippets always use the platform-specific newLine
    val extraCode0 =
      if (markScript) extraCode + "/*</generated>*/"
      else extraCode
    val (topWrapper, bottomWrapper, userCodeNestingLevel) =
      codeWrapper(code, codeSource, imports, printCode, indexedWrapperName, extraCode0)
    val (topWrapper0, bottomWrapper0) =
      if (markScript) (topWrapper + "/*<script>*/", "/*</script>*/ /*<generated>*/" + bottomWrapper)
      else (topWrapper, bottomWrapper)
    val importsLen = topWrapper0.length

    (topWrapper0 + code + bottomWrapper0, importsLen, userCodeNestingLevel)
  }
}

class DefaultPreprocessor(parse: => String => Either[String, Seq[G#Tree]],
                          markGeneratedSections: Boolean = false) extends IPreprocessor{

  import DefaultPreprocessor._

  def transformOrNull(stmts: Array[String],
                      resultIndex: String,
                      leadingSpaces: String,
                      codeSource: CodeSource,
                      indexedWrapper: String,
                      imports: Imports,
                      printerTemplate: Function[String, String],
                      extraCode: String,
                      skipEmpty: Boolean,
                      markScript: Boolean,
                      codeWrapper: CodeWrapper) = {
    // All code Ammonite compiles must be rooted in some package within
    // the `ammonite` top-level package
    assert(codeSource.pkgName.head == Name("ammonite"))
    expandStatements(stmts, resultIndex, skipEmpty) match {
      case None => null
      case Some(Left(error)) => throw new IPreprocessor.PreprocessorError(error)
      case Some(Right(Expanded(code, printer))) =>
        val (wrappedCode, importsLength, userCodeNestingLevel) = wrapCode(
          codeSource, Name(indexedWrapper), leadingSpaces + code,
          printerTemplate(printer.mkString(", ")),
          imports, extraCode, markScript, codeWrapper
        )
        new IPreprocessor.Output(wrappedCode, importsLength, userCodeNestingLevel)
    }
  }

  def Processor(cond: PartialFunction[(String, String, G#Tree), Expanded]) = {
    (code: String, name: String, tree: G#Tree) => cond.lift(name, code, tree)
  }

  def pprintSignature(ident: String, customMsg: Option[String]) = {
    val customCode = customMsg.fold("_root_.scala.None")(x => s"""_root_.scala.Some("$x")""")
    s"""
    _root_.ammonite
          .repl
          .ReplExtras
          .print($ident, ${fastparse.internal.Util.literalize(ident)}, $customCode)
    """
  }
  def definedStr(definitionLabel: String, name: String) =
    s"""
    _root_.ammonite
          .repl
          .ReplExtras
          .printDef("$definitionLabel", ${fastparse.internal.Util.literalize(name)})
    """

  def pprint(ident: String) = pprintSignature(ident, None)


  /**
   * Processors for declarations which all have the same shape
   */
  def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]) =
    (code: String, name: String, tree: G#Tree) =>
      cond.lift(tree).map{ name =>
        val printer =
          if (isPrivate(tree)) Nil
          else Seq(definedStr(definitionLabel, Name.backtickWrap(name.decoded)))
        Expanded(
          code,
          printer
        )
      }

  val ObjectDef = DefProc("object"){case m: G#ModuleDef => m.name}
  val ClassDef = DefProc("class"){ case m: G#ClassDef if !m.mods.isTrait => m.name }
  val TraitDef =  DefProc("trait"){ case m: G#ClassDef if m.mods.isTrait => m.name }
  val DefDef = DefProc("function"){ case m: G#DefDef => m.name }
  val TypeDef = DefProc("type"){ case m: G#TypeDef => m.name }

  val PatVarDef = Processor { case (name, code, t: G#ValDef) =>
    Expanded(
      //Only wrap rhs in function if it is not a function
      //Wrapping functions causes type inference errors.
      code,
      // Try to leave out all synthetics; we don't actually have proper
      // synthetic flags right now, because we're dumb-parsing it and not putting
      // it through a full compilation
      if (isPrivate(t) || t.name.decoded.contains("$")) Nil
      else if (!t.mods.hasFlag(Flags.LAZY)) Seq(pprint(Name.backtickWrap(t.name.decoded)))
      else Seq(s"""${pprintSignature(Name.backtickWrap(t.name.decoded), Some("<lazy>"))}""")
    )
  }

  val Import = Processor{
    case (name, code, tree: G#Import) =>
      val Array(keyword, body) = code.split(" ", 2)
      val tq = "\"\"\""
      Expanded(code, Seq(
        s"""
        _root_.ammonite
              .repl
              .ReplExtras
              .printImport(${fastparse.internal.Util.literalize(body)})
        """
      ))
  }

  val Expr = Processor{
    //Expressions are lifted to anon function applications so they will be JITed
    case (name, code, tree) =>
      val expandedCode =
        if (markGeneratedSections)
          s"/*<amm>*/val $name = /*</amm>*/$code"
        else
          s"val $name = $code"
      Expanded(
        expandedCode,
        if (isPrivate(tree)) Nil else Seq(pprint(name))
      )
  }

  val decls = Seq[(String, String, G#Tree) => Option[Expanded]](
    ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
  )

  def expandStatements(stmts: Seq[String],
                       wrapperIndex: String,
                       skipEmpty: Boolean): Option[Either[String, Expanded]] = {
    stmts match{
      // In the REPL, we do not process empty inputs at all, to avoid
      // unnecessarily incrementing the command counter
      //
      // But in scripts, we process empty inputs and create an empty object,
      // to ensure that when the time comes to cache/load the class it exists
      case Nil if skipEmpty => None
      case postSplit =>
        Some(complete(stmts.mkString(""), wrapperIndex, postSplit))

    }
  }

  def complete(code: String, resultIndex: String, postSplit: Seq[String]) = {
    val reParsed = postSplit.map(p => (parse(p), p))
    val errors = reParsed.collect{case (Left(e), _) => e }
    if (errors.length != 0) Left(errors.mkString(System.lineSeparator()))
    else {
      val allDecls = for {
        ((Right(trees), code), i) <- reParsed.zipWithIndex if trees.nonEmpty
      } yield {
        // Suffix the name of the result variable with the index of
        // the tree if there is more than one statement in this command
        val suffix = if (reParsed.length > 1) "_" + i else ""
        def handleTree(t: G#Tree) = {
          decls.iterator.flatMap(_.apply(code, "res" + resultIndex + suffix, t)).next()
        }
        trees match {
          case Seq(tree) => handleTree(tree)

          // This handles the multi-import case `import a.b, c.d`
          case trees if trees.forall(_.isInstanceOf[G#Import]) => handleTree(trees(0))

          // AFAIK this can only happen for pattern-matching multi-assignment,
          // which for some reason parse into a list of statements. In such a
          // scenario, aggregate all their printers, but only output the code once
          case trees =>
            val printers = for {
              tree <- trees
              if tree.isInstanceOf[G#ValDef]
              Expanded(_, printers) = handleTree(tree)
              printer <- printers
            } yield printer

            Expanded(code, printers)
        }
      }

      val expanded = allDecls match{
        case Seq(first, rest@_*) =>
          val allDeclsWithComments = Expanded(first.code, first.printer) +: rest
          allDeclsWithComments.reduce { (a, b) =>
            Expanded(
              // We do not need to separate the code with our own semi-colons
              // or newlines, as each expanded code snippet itself comes with
              // it's own trailing newline/semicolons as a result of the
              // initial split
              a.code + b.code,
              a.printer ++ b.printer
            )
          }
        case Nil => Expanded("", Nil)
      }

      Right(expanded)
    }
  }
}
