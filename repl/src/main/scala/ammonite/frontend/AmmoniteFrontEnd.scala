package ammonite.frontend

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import ammonite.terminal.filters._
import GUILikeFilters.SelectionFilter
import ammonite.terminal.LazyList.~:
import ammonite.terminal._
import fastparse.core.Parsed

import scala.annotation.tailrec
import Filter._
import ammonite.util.{Colors, Parsers, Res, Timer}
case class AmmoniteFrontEnd(extraFilters: Filter = Filter.empty) extends FrontEnd{

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
          case Parsed.Success(value, idx) =>
            Res.Success((code, value))
          case Parsed.Failure(_, index, extra) => Res.Failure(
            None,
            fastparse.core.ParseError.msg(extra.input, extra.traced.expected, index)
          )
        }
    }
    Timer("AmmoniteFrontEnd.action end")
    res
  }

  val cutPasteFilter = ReadlineFilters.CutPasteFilter()

  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: IndexedSeq[String]) = {
    Timer("AmmoniteFrontEnd.readLine start")
    val writer = new OutputStreamWriter(output)

    val autocompleteFilter: Filter = Filter.action(SpecialKeys.Tab){
      case TermState(rest, b, c, _) =>
        val (newCursor, completions, details) = compilerComplete(c, b.mkString)
        val details2 = for (d <- details) yield {

          Highlighter.defaultHighlight(
            d.toVector,
            colors.comment(),
            colors.`type`(),
            colors.literal(),
            colors.keyword(),
            fansi.Attr.Reset
          ).mkString
        }

        lazy val common = FrontEndUtils.findPrefix(completions, 0)
        val completions2 = for(comp <- completions) yield {

          val (left, right) = comp.splitAt(common.length)
          (colors.comment()(left) ++ right).render
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

    // Enter
    val multilineFilter = Filter.action(
      SpecialKeys.NewLine,
      ti => Parsers.split(ti.ts.buffer.mkString).isEmpty
    ){
      case TermState(rest, b, c, _) => BasicFilters.injectNewLine(b, c, rest)
    }

    val historyFilter = new HistoryFilter(
      () => history.reverse, colors.comment()
    )
    val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)

    val allFilters = Filter.merge(
      UndoFilter(),
      historyFilter,
      extraFilters,
      selectionFilter,
      GUILikeFilters.altFilter,
      GUILikeFilters.fnFilter,
      ReadlineFilters.navFilter,
      autocompleteFilter,
      cutPasteFilter,
      multilineFilter,
      BasicFilters.all
    )


    Timer("AmmoniteFrontEnd.readLine 1")
    val res = Terminal.readLine(
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
          fansi.Attr.Reset
        )
        val highlighted = fansi.Str(Highlighter.flattenIndices(indices, buffer).mkString)
        val (newBuffer, offset) = SelectionFilter.mangleBuffer(
          selectionFilter, highlighted, cursor, colors.selected()
        )

        val newNewBuffer = HistoryFilter.mangleBuffer(
          historyFilter, newBuffer, cursor, fansi.Underlined.On
        )
        (newNewBuffer, offset)
      }
    )
    Timer("TermCore.readLine")
    res
  }
}
