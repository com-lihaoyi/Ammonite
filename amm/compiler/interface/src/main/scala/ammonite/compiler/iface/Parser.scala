package ammonite.compiler.iface

import ammonite.util.ImportTree
import ammonite.util.Util.CodeSource

abstract class Parser {

  def split(
    code: String,
    ignoreIncomplete: Boolean = true,
    fileName: String = "(console)"
  ): Option[Either[String, Seq[String]]]

  final def parseImportHooks(
    source: CodeSource,
    stmts: Seq[String]
  ): (Seq[String], Seq[ImportTree]) =
    parseImportHooksWithIndices(source, stmts.map((0, _)))
  def parseImportHooksWithIndices(
    source: CodeSource,
    stmts: Seq[(Int, String)]
  ): (Seq[String], Seq[ImportTree])

  /**
    * Splits up a script file into its constituent blocks, each of which
    * is a tuple of (leading-whitespace, statements). Leading whitespace
    * is returned separately so we can later manipulate the statements e.g.
    * by adding `val res2 = ` without the whitespace getting in the way
    */
  def splitScript(
    rawCode: String,
    fileName: String
  ): Either[String, IndexedSeq[(String, Seq[String])]]

  def scriptBlocksWithStartIndices(
    rawCode: String,
    fileName: String
  ): Either[Parser.ScriptSplittingError, Seq[Parser.ScriptBlock]]

  def defaultHighlight(buffer: Vector[Char],
                       comment: fansi.Attrs,
                       `type`: fansi.Attrs,
                       literal: fansi.Attrs,
                       keyword: fansi.Attrs,
                       reset: fansi.Attrs): Vector[Char]

  def isObjDef(code: String): Boolean
}

object Parser {

  case class ParsedImportHooks(
    hookStatements: Seq[String],
    importTrees: Seq[ImportTree]
  )

  case class ScriptBlock(
    startIndex: Int,
    ncomment: String,
    codeWithStartIndices: Seq[(Int, String)]
  )

  object ScriptBlock {
    def apply(
      ncomment: String,
      codeWithStartIndices: Seq[(Int, String)]
    ): ScriptBlock =
      ScriptBlock(0, ncomment, codeWithStartIndices)
  }

  class ScriptSplittingError(
    message: String,
    val index: Int = -1,
    val expected: String = ""
  ) extends Exception(message)
}
