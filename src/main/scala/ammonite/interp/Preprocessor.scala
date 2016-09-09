package ammonite.runtime

import ammonite.util._
import ammonite.util.Util.{windowsPlatform, newLine, normalizeNewlines}
//import fastparse.all._

import scala.reflect.internal.Flags
import scala.tools.nsc.{Global => G}
import collection.mutable
import ammonite.kernel.LogError

import scalaz.{Name => _, _}
import Scalaz._
import Validation.FlatMap._

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
  * - Mangles imports from our [[ammonite.util.ImportData]] data structure into a source
  *   String
  *
  * - Combines all of these into a complete compilation unit ready to feed into
  *   the Scala compiler
  */
// trait Preprocessor {

//   def transform(stmts: Seq[String],
//                 resultIndex: String,
//                 leadingSpaces: String,
//                 pkgName: Seq[Name],
//                 indexedWrapperName: Name,
//                 imports: Imports,
//                 printerTemplate: String => String,
//                 extraCode: String): Res[Preprocessor.Output]
// }

object Preprocessor {

  private case class Expanded(code: String, printer: Seq[String])

  case class Output(code: String, prefixCharLength: Int)

  // private def errMsg(msg: String, code: String, expected: String, idx: Int): String = {
  //   val locationString = {
  //     val (first, last) = code.splitAt(idx)
  //     val lastSnippet = last.split(newLine).headOption.getOrElse("")
  //     val firstSnippet =
  //       first.reverse.split(newLine.reverse).lift(0).getOrElse("").reverse
  //     firstSnippet + lastSnippet + newLine + (" " * firstSnippet.length) + "^"
  //   }

  //   s"Syntax Error: $msg${newLine}$locationString"
  // }

  // /**
  //   * Splits up a script file into its constituent blocks, each of which
  //   * is a tuple of (leading-whitespace, statements). Leading whitespace
  //   * is returned separately so we can later manipulate the statements e.g.
  //   * by adding `val res2 = ` without the whitespace getting in the way
  //   */
  // def splitScript(rawCode: String): Res[Seq[(String, Seq[String])]] = {
  //   Parsers.splitScript(rawCode) match {
  //     case f: Parsed.Failure =>
  //       Res.Failure(None, errMsg(f.msg, rawCode, f.extra.traced.expected, f.index))
  //     case s: Parsed.Success[Seq[(String, Seq[String])]] =>
  //       var offset = 0
  //       val blocks = mutable.Buffer[(String, Seq[String])]()

  //       // comment holds comments or empty lines above the code which is not caught along with code
  //       for ((comment, code) <- s.value) {

  //         //ncomment has required number of newLines appended based on OS and offset
  //         //since fastparse has hardcoded `\n`s, while parsing strings with `\r\n`s it
  //         //gives out one extra `\r` after '@' i.e. block change
  //         //which needs to be removed to get correct line number (It adds up one extra line)
  //         //thats why the `comment.substring(1)` thing is necessary
  //         val ncomment =
  //           if (windowsPlatform && !blocks.isEmpty && !comment.isEmpty) {
  //             comment.substring(1) + newLine * offset
  //           } else {
  //             comment + newLine * offset
  //           }

  //         // 1 is added as Separator parser eats up the newLine char following @
  //         offset = offset + (comment.split(newLine, -1).length - 1) +
  //             code.map(_.split(newLine, -1).length - 1).sum + 1
  //         blocks.append((ncomment, code))
  //       }

  //       Res.Success(blocks)
  //   }
  // }

  def apply(parse: => String => ValidationNel[LogError, Seq[G#Tree]],
            stmts: NonEmptyList[String],
            resultIndex: String,
            leadingSpaces: String,
            pkgName: Seq[Name],
            indexedWrapperName: Name,
            imports: Imports,
            printerTemplate: String => String,
            extraCode: String): ValidationNel[LogError, Output] = {

    def Processor(cond: PartialFunction[(String, String, G#Tree), Preprocessor.Expanded]) = {
      (code: String, name: String, tree: G#Tree) =>
        cond.lift((name, code, tree))
    }

    def pprintSignature(ident: String, customMsg: Option[String]) = {
      val customCode = customMsg.fold("_root_.scala.None")(x => s"""_root_.scala.Some("$x")""")
      s"""
      _root_.ammonite
            .repl
            .ReplBridge
            .value
            .Internal
            .print($ident, $ident, "$ident", $customCode)
      """
    }
    def definedStr(definitionLabel: String, name: String) =
      s"""
      _root_.ammonite
            .repl
            .ReplBridge
            .value
            .Internal
            .printDef("$definitionLabel", "$name")
      """

    def pprint(ident: String) = pprintSignature(ident, None)

    val decls: List[(String, String, G#Tree) => Option[Preprocessor.Expanded]] = {

      /**
        * Processors for declarations which all have the same shape
        */
      def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]) =
        (code: String, name: String, tree: G#Tree) =>
          cond.lift(tree).map { name =>
            Preprocessor.Expanded(
              code,
              Seq(definedStr(definitionLabel, Name.backtickWrap(name.decoded)))
            )
        }

      val ObjectDef = DefProc("object") {
        case m: G#ModuleDef => m.name
      }

      val ClassDef = DefProc("class") {
        case m: G#ClassDef if !m.mods.isTrait => m.name
      }

      val TraitDef = DefProc("trait") {
        case m: G#ClassDef if m.mods.isTrait => m.name
      }

      val DefDef = DefProc("function") {
        case m: G#DefDef => m.name
      }

      val TypeDef = DefProc("type") {
        case m: G#TypeDef => m.name
      }

      val PatVarDef = Processor {
        case (name, code, t: G#ValDef) =>
          Expanded(
            //Only wrap rhs in function if it is not a function
            //Wrapping functions causes type inference errors.
            code,
            // Try to leave out all synthetics; we don't actually have proper
            // synthetic flags right now, because we're dumb-parsing it and not putting
            // it through a full compilation
            if (t.name.decoded.contains("$")) Nil
            else if (!t.mods.hasFlag(Flags.LAZY))
              Seq(pprint(Name.backtickWrap(t.name.decoded)))
            else
              Seq(s"""${pprintSignature(Name.backtickWrap(t.name.decoded), Some("<lazy>"))}""")
          )
      }

      val Import = Processor {
        case (name, code, tree: G#Import) =>
          val Array(_, body) = code.split(" ", 2)
          val tq = "\"\"\""
          Expanded(code,
                   Seq(
                     s"""
          _root_.ammonite
                .repl
                .ReplBridge
                .value
                .Internal
                .printImport($tq$body$tq)
          """
                   ))
      }

      val Expr = Processor {
        //Expressions are lifted to anon function applications so they will be JITed
        case (name, code, tree) => Expanded(s"val $name = $code", Seq(pprint(name)))
      }

      List(
        ObjectDef,
        ClassDef,
        TraitDef,
        DefDef,
        TypeDef,
        PatVarDef,
        Import,
        Expr
      )
    }

    // type signatures are added below for documentation

    val composed: String => ValidationNel[LogError, (Seq[G#Tree], String)] = x => parse(x) map (y => (y, x))

    val parsed: ValidationNel[LogError, NonEmptyList[(Seq[G#Tree], String)]] = stmts.traverseU(composed)

    def declParser(inp: ((Seq[G#Tree], String), Int)): ValidationNel[LogError, Expanded] = inp match {
      case ((trees, code), i) =>
        def handleTree(t: G#Tree): ValidationNel[LogError, Expanded] = {
          val parsedDecls: List[Expanded] = decls flatMap (x => x(code, "res" + resultIndex + "_" + i, t))
          parsedDecls match {
            case h :: t => Success(h)
            case Nil => Failure(NonEmptyList(LogError(s"Dont know how to handle $code")))
          }
        }
        trees match {
          case Seq(h) => handleTree(h)
          case _ if trees.nonEmpty && trees.forall(_.isInstanceOf[G#Import]) => handleTree(trees.head)
          case _ =>
            val filteredSeq = trees filter (_.isInstanceOf[G#ValDef])
            val handleTreeComposed: G#Tree => ValidationNel[LogError, Seq[String]] = handleTree(_) map (_.printer)
            val cumulativePrinter: ValidationNel[LogError, List[String]] =
              filteredSeq.toList.traverseU(handleTreeComposed).map(_.flatten)
            cumulativePrinter map (printer => Expanded(code, printer))
        }
    }

    val declTraversed: ValidationNel[LogError, NonEmptyList[Expanded]] =
      parsed.map(_.zipWithIndex).flatMap(_.traverseU(declParser))

    val expanded = declTraversed map {
      case NonEmptyList(h, tl) =>
        tl.foldLeft(h) {
          case (acc, v) => Expanded(acc.code ++ v.code, acc.printer ++ v.printer)
        }
    }

    expanded map {
      case Expanded(code, printer) =>
        wrapCode(pkgName,
                 indexedWrapperName,
                 leadingSpaces + code,
                 printerTemplate(printer.mkString(", ")),
                 imports,
                 extraCode)
    }

  }

  def importBlock(importData: Imports) = {
    // Group the remaining imports into sliding groups according to their
    // prefix, while still maintaining their ordering
    val grouped = mutable.Buffer[mutable.Buffer[ImportData]]()
    for (data <- importData.value) {
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
    val out = for (group <- grouped) yield {
      val printedGroup = for (item <- group) yield {
        if (item.fromName == item.toName) item.fromName.backticked
        else s"${item.fromName.backticked} => ${item.toName.backticked}"
      }
      val pkgString = group.head.prefix.map(_.backticked).mkString(".")
      "import " + pkgString + s".{$newLine  " +
        printedGroup.mkString(s",$newLine  ") + s"$newLine}$newLine"
    }
    val res = out.mkString

    res
  }

  private def wrapCode(pkgName: Seq[Name],
                       indexedWrapperName: Name,
                       code: String,
                       printCode: String,
                       imports: Imports,
                       extraCode: String): Output = {

    //we need to normalize topWrapper and bottomWrapper in order to ensure
    //the snippets always use the platform-specific newLine
    val topWrapper = normalizeNewlines(s"""
package ${pkgName.map(_.backticked).mkString(".")}
${importBlock(imports)}

object ${indexedWrapperName.backticked}{\n""")

    val bottomWrapper = normalizeNewlines(s"""\ndef $$main() = { $printCode }
  override def toString = "${indexedWrapperName.raw}"
  $extraCode
}
""")
    val importsLen = topWrapper.length

    Output(topWrapper + code + bottomWrapper, importsLen)
  }
}
