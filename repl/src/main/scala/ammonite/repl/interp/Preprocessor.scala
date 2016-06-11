package ammonite.repl.interp
import acyclic.file
import ammonite.repl.{ImportData, Parsers, Res, Timer}

import scala.reflect.internal.Flags
import scala.tools.nsc.{Global => G}
import collection.mutable
/**
  * Responsible for all scala-source-code-munging that happens within the
  * Ammonite REPL.
  *
  * Performs several tasks:
  *
  * - Takes top-level Scala expressions and assigns them to `res{1, 2, 3, ...}`
  *   values so they can be accessed later in the REPL
  *
  * - Wraps the code snippet with an wrapper `object` since Scala doesn't allow
  *   top-level expressions
  *
  * - Mangles imports from our [[ImportData]] data structure into a source
  *   String
  *
  * - Combines all of these into a complete compilation unit ready to feed into
  *   the Scala compiler
  */
trait Preprocessor{
  def transform(stmts: Seq[String],
                wrapperId: String,
                comment: String,
                pkgName: String,
                indexedWrapperName: String,
                imports: Seq[ImportData],
                printerTemplate: String => String): Res[Preprocessor.Output]
}
object Preprocessor{
  private case class Expanded(code: String, printer: Seq[String])
  case class Output(code: String, prefixCharLength: Int)
  def apply(parse: => String => Either[String, Seq[G#Tree]]): Preprocessor = new Preprocessor{

    def transform(stmts: Seq[String],
                  wrapperId: String,
                  comment: String,
                  pkgName: String,
                  indexedWrapperName: String,
                  imports: Seq[ImportData],
                  printerTemplate: String => String) = for{
      Preprocessor.Expanded(code, printer) <- expandStatements(stmts, wrapperId, comment)
      (wrappedCode, importsLength) = wrapCode(
        pkgName, indexedWrapperName, code,
        printerTemplate(printer.mkString(", ")),
        imports)
    } yield Preprocessor.Output(wrappedCode, importsLength)
    def Processor(cond: PartialFunction[(String, String, G#Tree), Preprocessor.Expanded]) = {
      (code: String, name: String, tree: G#Tree) => cond.lift(name, code, tree)
    }

    def pprintSignature(ident: String, customMsg: Option[String]) = {
      val customCode = customMsg.fold("_root_.scala.None")(x => s"""_root_.scala.Some("$x")""")
      s"""
      _root_.ammonite
            .repl
            .frontend
            .ReplBridge
            .repl
            .Internal
            .print($ident, $ident, "$ident", $customCode)
      """
    }
    def definedStr(definitionLabel: String, name: String) =
      s"""
      _root_.ammonite
            .repl
            .frontend
            .ReplBridge
            .repl
            .Internal
            .printDef("$definitionLabel", "$name")
      """

    def pprint(ident: String) = pprintSignature(ident, None)


    /**
     * Processors for declarations which all have the same shape
     */
    def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]) =
      (code: String, name: String, tree: G#Tree) =>
        cond.lift(tree).map{ name =>
          Preprocessor.Expanded(
            code,
            Seq(definedStr(definitionLabel, Parsers.backtickWrap(name.decoded)))
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
        if (t.name.decoded.contains("$")) Nil
        else if (!t.mods.hasFlag(Flags.LAZY)) Seq(pprint(Parsers.backtickWrap(t.name.decoded)))
        else Seq(s"""${pprintSignature(Parsers.backtickWrap(t.name.decoded), Some("<lazy>"))}""")
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
                .frontend
                .ReplBridge
                .repl
                .Internal
                .printImport($tq$body$tq)
          """
        ))
    }

    val Expr = Processor{
      //Expressions are lifted to anon function applications so they will be JITed
      case (name, code, tree) => Expanded(s"val $name = $code", Seq(pprint(name)))
    }

    val decls = Seq[(String, String, G#Tree) => Option[Preprocessor.Expanded]](
      ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
    )

    def expandStatements(stmts: Seq[String],
                         wrapperId: String,
                         comment: String = ""): Res[Preprocessor.Expanded] = {
      val unwrapped = stmts.flatMap{x => Parsers.unwrapBlock(x) match {
        case Some(contents) =>
          Parsers.split(contents).get.get.value

        case None => Seq(x)
      }}
      unwrapped match{
        case Nil => Res.Skip
        case postSplit =>
          complete(stmts.mkString(""), wrapperId, postSplit, comment)

      }
    }

    def complete(code: String, wrapperId: String, postSplit: Seq[String], comment: String) = {
      val reParsed = postSplit.map(p => (parse(p), p))
      val errors = reParsed.collect{case (Left(e), _) => e }
      if (errors.length != 0) Res.Failure(None, errors.mkString("\n"))
      else {
        val allDecls = for {
          ((Right(trees), code), i) <- reParsed.zipWithIndex if (trees.nonEmpty)
        } yield {
          // Suffix the name of the result variable with the index of
          // the tree if there is more than one statement in this command
          val suffix = if (reParsed.length > 1) "_" + i else ""
          def handleTree(t: G#Tree) = {
            decls.iterator.flatMap(_.apply(code, "res" + wrapperId + suffix, t)).next()
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
                Preprocessor.Expanded(_, printers) = handleTree(tree)
                printer <- printers
              } yield printer

              Preprocessor.Expanded(code, printers)
          }
        }

        val Seq(first, rest@_*) = allDecls
        val allDeclsWithComments = Expanded(comment + first.code , first.printer) +: rest
        Res(
          allDeclsWithComments.reduceOption { (a, b) =>
            Expanded(
              a.code + ";" + b.code,
              a.printer ++ b.printer
            )
          },
          "Don't know how to handle " + code
        )

      }
    }
  }


  def importBlock(importData: Seq[ImportData]) = {
    Timer("importBlock 0")
    // Group the remaining imports into sliding groups according to their
    // prefix, while still maintaining their ordering
    val grouped = mutable.Buffer[mutable.Buffer[ImportData]]()
    for(data <- importData){
      if (grouped.isEmpty) grouped.append(mutable.Buffer(data))
      else {
        val last = grouped.last.last

        // Start a new import if we're importing from somewhere else, or
        // we're importing the same thing from the same place but aliasing
        // it to a different name, since you can't import the same thing
        // twice in a single import statement
        val startNewImport =
          last.prefix != data.prefix || grouped.last.exists(_.fromName == data.fromName)

        if (startNewImport) grouped.append(mutable.Buffer(data))
        else grouped.last.append(data)
      }
    }
    // Stringify everything
    val out = for(group <- grouped) yield {
      val printedGroup = for(item <- group) yield{
        if (item.fromName == item.toName) Parsers.backtickWrap(item.fromName)
        else s"${Parsers.backtickWrap(item.fromName)} => ${Parsers.backtickWrap(item.toName)}"
      }
      "import " + group.head.prefix + ".{\n  " + printedGroup.mkString(",\n  ") + "\n}\n"
    }
    val res = out.mkString

    Timer("importBlock 1")
    res
  }

  def wrapCode(pkgName: String,
               indexedWrapperName: String,
               code: String,
               printCode: String,
               imports: Seq[ImportData]) = {

    val topWrapper = s"""
package $pkgName
${importBlock(imports)}

object $indexedWrapperName{\n"""

    val bottomWrapper = s"""\ndef $$main() = { $printCode }
  override def toString = "$indexedWrapperName"
}
"""
    val importsLen = topWrapper.length

    (topWrapper + code + bottomWrapper, importsLen)
  }

}

