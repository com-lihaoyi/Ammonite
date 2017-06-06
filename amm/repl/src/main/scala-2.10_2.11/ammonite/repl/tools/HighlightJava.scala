package ammonite.repl.tools

import ammonite.repl.Highlighter.flattenIndices
import ammonite.util.CodeColors
import com.github.javaparser.GeneratedJavaParserConstants._
import com.github.javaparser.{GeneratedJavaParserConstants, ParseStart, StringProvider}
import sourcecode.Compat.Context

import scala.collection.mutable

object HighlightJava{
  def highlightJavaCode(sourceCode: String, colors: CodeColors) = fansi.Str(sourceCode)
}