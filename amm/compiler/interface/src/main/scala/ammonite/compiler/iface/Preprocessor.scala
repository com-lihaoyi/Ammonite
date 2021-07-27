package ammonite.compiler.iface

import ammonite.util.{Imports, Name, Res}
import ammonite.util.Util.CodeSource

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
abstract class Preprocessor {

  def transform(
    stmts: Seq[String],
    resultIndex: String,
    leadingSpaces: String,
    codeSource: CodeSource,
    indexedWrapperName: Name,
    imports: Imports,
    printerTemplate: String => String,
    extraCode: String,
    skipEmpty: Boolean,
    markScript: Boolean,
    codeWrapper: CodeWrapper
  ): Res[Preprocessor.Output]

}

object Preprocessor {

  case class Output(
    code: String,
    prefixCharLength: Int,
    userCodeNestingLevel: Int
  )

}
