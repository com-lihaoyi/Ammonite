package ammonite.compiler

import ammonite.compiler.iface.{Preprocessor as IPreprocessor, Compiler as _, Parser as _, *}
import ammonite.util.Util.CodeSource
import ammonite.util.{Imports, Name, Res}
import dotty.tools.dotc
import dotty.tools.dotc.ast.{desugar, untpd}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.{Flags, Names}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.util.SourceFile
import pprint.Util

import java.util.function.Function as JFunction

class Preprocessor(
  ctx: Context,
  markGeneratedSections: Boolean
) extends IPreprocessor {

  // FIXME Quite some duplication with DefaultProcessor for Scala 2.x

  private case class Expanded(code: String, printer: Seq[String])

  private def parse(source: String): Either[Seq[String], List[untpd.Tree]] = {
    val reporter = Compiler.newStoreReporter()
    val sourceFile = SourceFile.virtual("foo", source)
    val parseCtx = ctx.fresh.setReporter(reporter).withSource(sourceFile)
    val parser = new DottyParser(sourceFile)(using parseCtx)
    val stats = parser.blockStatSeq()
    parser.accept(Tokens.EOF)
    if (reporter.hasErrors) {
      val errorsStr = reporter
        .allErrors
        // .map(rendering.formatError)
        .map(e => scala.util.Try(e.msg.toString).toOption.getOrElse("???"))
      Left(errorsStr)
    } else
      Right(stats)
  }

  def transform(
    stmts: Seq[String],
    resultIndex: String,
    leadingSpaces: String,
    codeSource: CodeSource,
    indexedWrapper: Name,
    imports: Imports,
    printerTemplate: String => String,
    extraCode: String,
    skipEmpty: Boolean,
    markScript: Boolean,
    codeWrapper: CodeWrapper
  ): Res[IPreprocessor.Output] = {

    // println(s"transformOrNull(${stmts.toSeq})")

    // All code Ammonite compiles must be rooted in some package within
    // the `ammonite` top-level package
    assert(codeSource.pkgName.head == Name("ammonite"))

    expandStatements(stmts, resultIndex, skipEmpty).map {
      case Expanded(code, printer) =>
        val (wrappedCode, importsLength, userCodeNestingLevel) = wrapCode(
          codeSource, indexedWrapper, leadingSpaces + code,
          printerTemplate(printer.mkString(", ")),
          imports, extraCode, markScript, codeWrapper
        )
        IPreprocessor.Output(wrappedCode, importsLength, userCodeNestingLevel)
    }
  }

  private def expandStatements(
    stmts: Seq[String],
    wrapperIndex: String,
    skipEmpty: Boolean
  ): Res[Expanded] =
    stmts match{
      // In the REPL, we do not process empty inputs at all, to avoid
      // unnecessarily incrementing the command counter
      //
      // But in scripts, we process empty inputs and create an empty object,
      // to ensure that when the time comes to cache/load the class it exists
      case Nil if skipEmpty => Res.Skip
      case postSplit =>
        Res(complete(stmts.mkString(""), wrapperIndex, postSplit))

    }

  private def wrapCode(
    codeSource: CodeSource,
    indexedWrapperName: Name,
    code: String,
    printCode: String,
    imports: Imports,
    extraCode: String,
    markScript: Boolean,
    codeWrapper: CodeWrapper
  ) = {

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

  // Large parts of the logic below is adapted from DefaultProcessor,
  // the Scala 2 counterpart of this file.

  private def isPrivate(tree: untpd.Tree): Boolean =
    tree match {
      case m: untpd.MemberDef => m.mods.is(Flags.Private)
      case _ => false
    }

  private def Processor(cond: PartialFunction[(String, String, untpd.Tree), Expanded]) =
    (code: String, name: String, tree: untpd.Tree) => cond.lift(name, code, tree)

  private def pprintSignature(ident: String, customMsg: Option[String]): String =
    val customCode = customMsg.fold("_root_.scala.None")(x => s"""_root_.scala.Some("$x")""")
    s"""
    _root_.ammonite
          .repl
          .ReplBridge
          .value
          .Internal
          .print($ident, ${Util.literalize(ident)}, $customCode)
    """
  private def definedStr(definitionLabel: String, name: String) =
    s"""
    _root_.ammonite
          .repl
          .ReplBridge
          .value
          .Internal
          .printDef("$definitionLabel", ${Util.literalize(name)})
    """
  private def pprint(ident: String) = pprintSignature(ident, None)

  /**
   * Processors for declarations which all have the same shape
   */
  private def DefProc(definitionLabel: String)(cond: PartialFunction[untpd.Tree, Names.Name]) =
    (code: String, name: String, tree: untpd.Tree) =>
      cond.lift(tree).map{ name =>
        val printer =
          if (isPrivate(tree)) Nil
          else
            val definedName =
              if name.isEmpty then ""
              else Name.backtickWrap(name.decode.toString)
            Seq(definedStr(definitionLabel, definedName))
        Expanded(
          code,
          printer
        )
      }

  private val ObjectDef = DefProc("object"){case m: untpd.ModuleDef => m.name}
  private val ClassDef = DefProc("class"){
    case m: untpd.TypeDef if m.isClassDef && !m.mods.flags.is(Flags.Trait) =>
      m.name
  }
  private val TraitDef = DefProc("trait"){
    case m: untpd.TypeDef if m.isClassDef && m.mods.flags.is(Flags.Trait) =>
      m.name
  }
  private val DefDef = DefProc("function"){
    case m: untpd.DefDef if m.mods.flags.is(Flags.Given) && m.name.isEmpty =>
      given Context = ctx
      desugar.inventGivenOrExtensionName(m.tpt)
    case m: untpd.DefDef =>
      m.name
  }

  private val ExtDef = DefProc("extension methods") {
    case ext: untpd.ExtMethods => Names.EmptyTermName
  }
  private val TypeDef = DefProc("type"){ case m: untpd.TypeDef => m.name }

  private val VarDef = Processor { case (name, code, t: untpd.ValDef) =>
    Expanded(
      //Only wrap rhs in function if it is not a function
      //Wrapping functions causes type inference errors.
      code,
      // Try to leave out all synthetics; we don't actually have proper
      // synthetic flags right now, because we're dumb-parsing it and not putting
      // it through a full compilation
      if (isPrivate(t) || t.name.decode.toString.contains("$")) Nil
      else if (t.mods.flags.is(Flags.Given)) {
        given Context = ctx
        val name0 = if (t.name.isEmpty) desugar.inventGivenOrExtensionName(t.tpt) else t.name
        Seq(pprintSignature(Name.backtickWrap(name0.decode.toString), Some("<given>")))
      }
      else if (t.mods.flags.is(Flags.Lazy))
        Seq(pprintSignature(Name.backtickWrap(t.name.decode.toString), Some("<lazy>")))
      else Seq(pprint(Name.backtickWrap(t.name.decode.toString)))
    )
  }

  private val PatDef = Processor { case (name, code, t: untpd.PatDef) =>
    val isLazy = t.mods.flags.is(Flags.Lazy)
    val printers =
      if (isPrivate(t)) Nil
      else
        t.pats
          .flatMap {
            case untpd.Tuple(trees) => trees
            case elem => List(elem)
          }
          .flatMap {
            case untpd.Ident(name) =>
              val decoded = name.decode.toString
              if (decoded.contains("$")) Nil
              else if (isLazy) Seq(pprintSignature(Name.backtickWrap(decoded), Some("<lazy>")))
              else Seq(pprint(Name.backtickWrap(decoded)))
            case _ => Nil // can this happen?
          }
    Expanded(code, printers)
  }

  private val Import = Processor {
    case (name, code, tree: untpd.Import) =>
      val Array(keyword, body) = code.split(" ", 2)
      val tq = "\"\"\""
      Expanded(code, Seq(
        s"""
        _root_.ammonite
              .repl
              .ReplBridge
              .value
              .Internal
              .printImport(${Util.literalize(body)})
        """
      ))
  }

  private val Expr = Processor {
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

  private val decls = Seq[(String, String, untpd.Tree) => Option[Expanded]](
    ObjectDef, ClassDef, TraitDef, DefDef, ExtDef, TypeDef, VarDef, PatDef, Import, Expr
  )

  private def complete(
    code: String,
    resultIndex: String,
    postSplit: Seq[String]
  ): Either[String, Expanded] = {
    val reParsed = postSplit.map(p => (parse(p), p))
    val errors = reParsed.collect{case (Left(e), _) => e }.flatten
    if (errors.length != 0) Left(errors.mkString(System.lineSeparator()))
    else {
      val allDecls = for {
        case ((Right(trees), code), i) <- reParsed.zipWithIndex if trees.nonEmpty
      } yield {
        // Suffix the name of the result variable with the index of
        // the tree if there is more than one statement in this command
        val suffix = if (reParsed.length > 1) "_" + i else ""
        def handleTree(t: untpd.Tree) = {
          // println(s"handleTree($t)")
          val it = decls.iterator.flatMap(_.apply(code, "res" + resultIndex + suffix, t))
          if (it.hasNext)
            it.next()
          else {
            sys.error(s"Don't know how to handle ${t.getClass}: $t")
          }
        }
        trees match {
          case Seq(tree) => handleTree(tree)

          // This handles the multi-import case `import a.b, c.d`
          case trees if trees.forall(_.isInstanceOf[untpd.Import]) => handleTree(trees(0))

          // AFAIK this can only happen for pattern-matching multi-assignment,
          // which for some reason parse into a list of statements. In such a
          // scenario, aggregate all their printers, but only output the code once
          case trees =>
            val printers = for {
              tree <- trees
              if tree.isInstanceOf[untpd.ValDef]
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
