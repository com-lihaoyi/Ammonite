package ammonite.repl.frontend

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import ammonite.repl._
import ammonite.terminal.LazyList.~:
import ammonite.terminal._
import fastparse.core.Result

case class AmmoniteFrontEnd(extraFilters: TermCore.Filter = PartialFunction.empty) extends FrontEnd{

  def width = FrontEndUtils.width
  def height = FrontEndUtils.height

  def action(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             colors: Colors,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: Seq[String],
             addHistory: String => Unit) = {
    Timer("AmmoniteFrontEnd.action start")
    val res = readLine(reader, output, prompt, colors, compilerComplete, history) match{
      case None => Res.Exit(())
      case Some(code) =>
        addHistory(code)
        Parsers.Splitter.parse(code) match{
          case Result.Success(value, idx) => Res.Success((code, value))
          case f: Result.Failure => Res.Failure(
            fastparse.core.ParseError.msg(f.input, f.traced.expected, f.index)
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
               history: Seq[String]) = {
    Timer("AmmoniteFrontEnd.readLine start")
    val writer = new OutputStreamWriter(output)

    val autocompleteFilter: TermCore.Filter = {
      case TermInfo(TermState(9 ~: rest, b, c), width) =>
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

        if (details.length != 0 || completions.length == 0)
          Printing(TermState(rest, b, c), stdout)
        else{
          val newBuffer = b.take(newCursor) ++ common ++ b.drop(c)
          Printing(TermState(rest, newBuffer, newCursor + common.length), stdout)
        }

    }

    val multilineFilter: TermCore.Filter = {
      case TermState(lb ~: rest, b, c)
        if (lb == 10 || lb == 13)
        && ammonite.repl.Parsers.split(b.mkString).isEmpty => // Enter

        BasicFilters.injectNewLine(b, c, rest)
    }

    val historyFilter = ReadlineFilters.HistoryFilter(() => history.reverse)
    val viHistoryFilter = VIFilters.VIHistoryFilter(history)
    val cutPasteFilter = ReadlineFilters.CutPasteFilter()
    val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)

    val allFilters =
      extraFilters orElse
      selectionFilter orElse
      VIFilters.viFilter orElse
      viHistoryFilter orElse
      GUILikeFilters.altFilter orElse
      GUILikeFilters.fnFilter orElse
      ReadlineFilters.navFilter orElse
      autocompleteFilter orElse
      historyFilter.filter orElse
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
        val indices = Highlighter.defaultHighlightIndices(
          buffer,
          colors.comment(),
          colors.`type`(),
          colors.literal(),
          colors.keyword(),
          colors.reset()
        )
        selectionFilter.mark match{
          case Some(mark) if mark != cursor =>
            val Seq(min, max) = Seq(cursor, mark).sorted
            val before = indices.filter(_._1 <= min)
            val after = indices.filter(_._1 > max)
            val lastBeforeAfter =
              indices.filter(_._1 <= max)
                .lastOption
                .map(_._2)
                .getOrElse("")
            val middle = Seq(
              (min, colors.reset() + colors.selected(), true),
              (max, colors.reset() + lastBeforeAfter, true)
            )
            val newIndices = before ++ middle ++ after
            val displayOffset = if (cursor < mark) 0 else -1
            // Add Console.RESET to the end of the buffer to hack around bug
            // in TermCore, to be fixed later when we clean up the crazy
            // TermCore.readLine logic
            (
              Highlighter.flattenIndices(newIndices, buffer) ++ Console.RESET,
              displayOffset
            )
          case _ => (Highlighter.flattenIndices(indices, buffer) ++ Console.RESET, 0)
        }
      }
    )
    Timer("TermCore.readLine")
    res
  }
}
