package ammonite.repl.frontend

import scala.annotation.tailrec

/**
 * Created by haoyi on 8/29/15.
 */
object FrontEndUtils {
  def width = ammonite.terminal.TTY.consoleDim("cols")
  def height = ammonite.terminal.TTY.consoleDim("lines")
  def tabulate(snippets: Seq[String], width: Int) = {
    val gap = 2
    val maxLength = snippets.maxBy(_.replaceAll("\u001B\\[[;\\d]*m", "").length).length + gap
    val columns = math.max(1, width / maxLength)

    snippets.grouped(columns).flatMap{
      case first :+ last => first.map(_.padTo(width / columns, ' ')) :+ last :+ "\n"
    }
  }

  @tailrec def findPrefix(strings: Seq[String], i: Int = 0): String = {
    if (strings.count(_.length > i) == 0) strings(0).take(i)
    else if(strings.map(_(i)).distinct.length > 1) strings(0).take(i)
    else findPrefix(strings, i + 1)
  }
}
