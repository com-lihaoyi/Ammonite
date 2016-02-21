package ammonite.repl.frontend

import java.io.{OutputStreamWriter, OutputStream, InputStream}

import ammonite.repl._
import ammonite.terminal.GUILikeFilters.SelectionFilter
import ammonite.terminal.LazyList.~:
import ammonite.terminal._
import fastparse.core.Parsed
import scala.annotation.tailrec

case class AmmoniteFrontEnd(extraFilters: TermCore.Filter = PartialFunction.empty) extends FrontEnd{

  def width = FrontEndUtils.width
  def height = FrontEndUtils.height

  def action(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             colors: Colors,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: IndexedSeq[String],
             addHistory: String => Unit) = {
    Timer("AmmoniteFrontEnd.action start")
    val res = readLine(reader, output, prompt, colors, compilerComplete, history) match{
      case None => Res.Exit(())
      case Some(code) =>

        addHistory(code)
        Parsers.Splitter.parse(code) match{
          case Parsed.Success(value, idx) => Res.Success((code, value))
          case Parsed.Failure(_, index, extra) => Res.Failure(
            None,
            fastparse.core.ParseError.msg(extra.input, extra.traced.expected, index)
          )
        }
    }
    Timer("AmmoniteFrontEnd.action end")
    res
  }


  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: IndexedSeq[String]) = {
    Timer("AmmoniteFrontEnd.readLine start")
    val writer = new OutputStreamWriter(output)

    val autocompleteFilter: TermCore.Filter = {
      case TermInfo(TermState(9 ~: rest, b, c, _), width) =>
        val (newCursor, completions, details) = compilerComplete(c, b.mkString)
        val details2 = for (d <- details) yield {
          Highlighter.defaultHighlight(
            d.toVector,
            colors.comment(),
            colors.`type`(),
            colors.literal(),
            colors.keyword(),
            colors.reset()
          ).mkString
        }

        lazy val common = FrontEndUtils.findPrefix(completions, 0)
        val completions2 = for(comp <- completions) yield {

          val (left, right) = comp.splitAt(common.length)
          colors.comment() + left + colors.reset() + right
        }
        val stdout =
          FrontEndUtils.printCompletions(completions2, details2)
                       .mkString

        if (details.nonEmpty || completions.isEmpty)
          Printing(TermState(rest, b, c), stdout)
        else{
          val newBuffer = b.take(newCursor) ++ common ++ b.drop(c)
          Printing(TermState(rest, newBuffer, newCursor + common.length), stdout)
        }

    }

    val multilineFilter: TermCore.Filter = {
      case TermState(lb ~: rest, b, c, _)
        if (lb == 10 || lb == 13)
        && ammonite.repl.Parsers.split(b.mkString).isEmpty => // Enter

        BasicFilters.injectNewLine(b, c, rest)
    }

    val historyFilter = new HistoryFilter(
      () => history.reverse, colors.comment(), colors.reset()
    )
    val cutPasteFilter = ReadlineFilters.CutPasteFilter()
    val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)

    val allFilters =
      historyFilter.filter orElse
      extraFilters orElse
      selectionFilter orElse
      GUILikeFilters.altFilter orElse
      GUILikeFilters.fnFilter orElse
      ReadlineFilters.navFilter orElse
      autocompleteFilter orElse
      cutPasteFilter orElse
      multilineFilter orElse
      BasicFilters.all


    Timer("AmmoniteFrontEnd.readLine 1")
    val res = TermCore.readLine(
      prompt,
      reader,
      writer,
      allFilters,
      displayTransform = { (buffer, cursor) =>
        val resetColor = "\u001b[39m"

        val indices = Highlighter.defaultHighlightIndices(
          buffer,
          colors.comment(),
          colors.`type`(),
          colors.literal(),
          colors.keyword(),
          resetColor
        )
        val highlighted = Ansi.Str.parse(Highlighter.flattenIndices(indices, buffer).mkString)
        val (newBuffer, offset) = SelectionFilter.mangleBuffer(
          selectionFilter, highlighted, cursor, Ansi.Color.ParseMap(colors.selected())
        )

        val newNewBuffer = HistoryFilter.mangleBuffer(
          historyFilter, newBuffer, cursor, Ansi.Underlined
        )
        (newNewBuffer, offset)
      }
    )
    Timer("TermCore.readLine")
    res
  }
}
