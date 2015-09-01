package ammonite.repl.frontend

import java.io.OutputStreamWriter

import ammonite.terminal.LazyList

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

    val grouped =
      snippets.toList
              .grouped(math.ceil(snippets.length * 1.0 / columns).toInt)
              .toList

    ammonite.repl.Util.transpose(grouped).flatMap{
      case first :+ last => first.map(_.padTo(width / columns, ' ')) :+ last :+ "\n"
    }
  }

  @tailrec def findPrefix(strings: Seq[String], i: Int = 0): String = {
    if (strings.count(_.length > i) == 0) strings(0).take(i)
    else if(strings.collect{ case x if x.length > i => x(i)}.distinct.length > 1)
      strings(0).take(i)
    else findPrefix(strings, i + 1)
  }

  def printCompletions(completions: Seq[String],
                       details: Seq[String]): List[String] = {

    val prelude =
      if (details.length != 0 || completions.length != 0) List("\n")
      else Nil

    val detailsText =
      if (details.length == 0) Nil
      else FrontEndUtils.tabulate(details, FrontEndUtils.width)


    val completionText =
      if (completions.length == 0) Nil
      else FrontEndUtils.tabulate(completions, FrontEndUtils.width)

    prelude ++ detailsText ++ completionText
  }

}
