package utils

import ammonite.ops._
import ba.sake.hepek.prismjs.PrismCodeHighlightComponents
import ba.sake.hepek.Resources

object Imports {

  object chl extends PrismCodeHighlightComponents

  object resources extends Resources

  // TODO move to Hepek...
  def extractFileSnippet(filePath: ammonite.ops.BasePath,
                         startQueries: List[String] = Nil,
                         endQueries: List[String] = Nil): (Int, Int, String) = {
    val absPath = filePath match {
      case p: Path    => p
      case p: RelPath => pwd / p
    }
    extractSnippet(read ! absPath, startQueries, endQueries)
  }

  /**
    * Searches for startQueries one by one and takes the lines until it finds endQueries. <br>
    *   Trims trailing whitespace. Trims indentation.
    *
    * @param fullText Text of file.
    * @param startQueries Queries to start from.
    * @param endQueries Queries to end with.
    * @return Snippet of text, usually code snippet.
    */
  def extractSnippet(fullText: String,
                     startQueries: List[String] = Nil,
                     endQueries: List[String] = Nil): (Int, Int, String) = {
    val allLines = fullText.lines.toList
    def walk(queries: List[String], start: Int): Int = {
      var startIndex = start
      // finds the first query, then second etc.
      for (query <- queries) {
        startIndex = allLines.indexWhere(_.contains(query), startIndex + 1)
        if (startIndex == -1)
          throw new IllegalArgumentException(
            s"Missing query '$query' in queries $queries"
          )
      }
      startIndex
    }

    // if no queries, start from 0-th line, else find the index
    val startIndex  = if (startQueries.isEmpty) 0 else walk(startQueries, -1)
    val startIndent = allLines(startIndex).takeWhile(_.isWhitespace).length
    val endIndex = if (endQueries.isEmpty) {
      // take while indentation is the same or bigger :)
      val next = allLines.drop(startIndex).takeWhile { line =>
        line.trim.isEmpty || line
          .takeWhile(_.isWhitespace)
          .length >= startIndent
      }
      startIndex + next.length
    } else {
      walk(endQueries, startIndex)
    }

    val lines = allLines
      .slice(startIndex, endIndex)
      .map(_.drop(startIndent))
      .reverse
      .dropWhile(_.trim.isEmpty) // trim end whitespace
      .reverse

    (startIndex, endIndex, lines.mkString("\n"))
  }
}
