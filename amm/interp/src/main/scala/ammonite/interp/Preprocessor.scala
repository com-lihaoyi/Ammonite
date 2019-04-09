package ammonite.interp

import ammonite.util._
import ammonite.util.Util.{CodeSource, newLine, normalizeNewlines, windowsPlatform}
import fastparse._

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
  * - Mangles imports from our [[ammonite.util.ImportData]] data structure into a source
  *   String
  *
  * - Combines all of these into a complete compilation unit ready to feed into
  *   the Scala compiler
  */
trait Preprocessor{
  def transform(stmts: Seq[String],
                resultIndex: String,
                leadingSpaces: String,
                codeSource: CodeSource,
                indexedWrapperName: Name,
                imports: Imports,
                printerTemplate: String => String,
                extraCode: String,
                skipEmpty: Boolean,
                codeWrapper: CodeWrapper): Res[Preprocessor.Output]
}

object Preprocessor{
  case class Output(code: String,
                    prefixCharLength: Int,
                    userCodeNestingLevel: Int)


  def formatFastparseError(fileName: String, rawCode: String, f: Parsed.Failure) = {

    val lineColIndex = f.extra.input.prettyIndex(f.index)
    val expected = f.trace().failure.label
      val locationString = {
        val (first, last) = rawCode.splitAt(f.index)
        val lastSnippet = last.split(newLine).headOption.getOrElse("")
        val firstSnippet = first.reverse
          .split(newLine.reverse)
          .lift(0).getOrElse("").reverse
        firstSnippet + lastSnippet + newLine + (" " * firstSnippet.length) + "^"
      }
    s"$fileName:$lineColIndex expected $expected$newLine$locationString"
  }


  /**
    * Splits up a script file into its constituent blocks, each of which
    * is a tuple of (leading-whitespace, statements). Leading whitespace
    * is returned separately so we can later manipulate the statements e.g.
    * by adding `val res2 = ` without the whitespace getting in the way
    */
  def splitScript(rawCode: String, fileName: String): Res[IndexedSeq[(String, Seq[String])]] = {
    Parsers.splitScript(rawCode) match {
      case f: Parsed.Failure =>
        Res.Failure(formatFastparseError(fileName, rawCode, f))

      case s: Parsed.Success[Seq[(String, Seq[String])]] =>

        var offset = 0
        val blocks = mutable.ArrayBuffer[(String, Seq[String])]()

        // comment holds comments or empty lines above the code which is not caught along with code
        for( (comment, code) <- s.value){

          //ncomment has required number of newLines appended based on OS and offset
          //since fastparse has hardcoded `\n`s, while parsing strings with `\r\n`s it
          //gives out one extra `\r` after '@' i.e. block change
          //which needs to be removed to get correct line number (It adds up one extra line)
          //thats why the `comment.substring(1)` thing is necessary
          val ncomment =
            if(windowsPlatform && blocks.nonEmpty && !comment.isEmpty){
              comment.substring(1) + newLine * offset
            }else{
              comment + newLine * offset
            }

          // 1 is added as Separator parser eats up the newLine char following @
          offset = offset + (comment.split(newLine, -1).length - 1) +
            code.map(_.split(newLine, -1).length - 1).sum + 1
          blocks.append((ncomment, code))
        }

        Res.Success(blocks.toIndexedSeq)
    }
  }


  def wrapCode(codeSource: CodeSource,
               indexedWrapperName: Name,
               code: String,
               printCode: String,
               imports: Imports,
               extraCode: String,
               codeWrapper: CodeWrapper) = {

    //we need to normalize topWrapper and bottomWrapper in order to ensure
    //the snippets always use the platform-specific newLine
    val (topWrapper, bottomWrapper, userCodeNestingLevel) =
     codeWrapper(code, codeSource, imports, printCode, indexedWrapperName, extraCode)
    val importsLen = topWrapper.length

    (topWrapper + code + bottomWrapper, importsLen, userCodeNestingLevel)
  }


}

