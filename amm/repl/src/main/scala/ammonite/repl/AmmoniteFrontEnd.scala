package ammonite.repl

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import ammonite.repl.api.FrontEnd
import ammonite.terminal.filters._
import GUILikeFilters.SelectionFilter
import ammonite.terminal._
import fastparse.Parsed
import ammonite.util.{Colors, Res}
import ammonite.compiler.iface.Parser
case class AmmoniteFrontEnd(
  parser: Parser,
  extraFilters: Filter = Filter.empty
) extends FrontEnd{

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
    readLine(reader, output, prompt, colors, compilerComplete, history) match{
      case None => Res.Exit(())
      case Some(code) =>
        addHistory(code)
        parser.split(code, ignoreIncomplete = false).get match{
          case Right(value) => Res.Success((code, value))
          case Left(error) => Res.Failure(error)
        }
    }
  }

  val cutPasteFilter = ReadlineFilters.CutPasteFilter()

  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: IndexedSeq[String]) = {

    val writer = new OutputStreamWriter(output)

    val autocompleteFilter: Filter = Filter.action(SpecialKeys.Tab){
      case TermState(rest, b, c, _) =>
        val (newCursor, completions, details) = TTY.withSttyOverride(TTY.restoreSigInt()) {
          compilerComplete(c, b.mkString)
        }
        val details2 = for (d <- details) yield {

          parser.defaultHighlight(
            d.toVector,
            colors.comment(),
            colors.`type`(),
            colors.literal(),
            colors.keyword(),
            colors.error(),
            fansi.Attr.Reset
          ).mkString
        }

        lazy val common = FrontEndUtils.findPrefix(completions, 0)

        val blacklisted = Seq(
          "!=",
          "==",
          "asInstanceOf",
          "equals",
          "getClass",
          "hashCode",
          "isInstanceOf",
          "toString",
          "|>"
        )

        val completions2 = for(comp <- completions.filterNot(blacklisted.contains)) yield {

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
      ti => parser.split(ti.ts.buffer.mkString).isEmpty
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


    val res = Terminal.readLine(
      prompt,
      reader,
      writer,
      allFilters,
      displayTransform = { (buffer, cursor) =>


        val highlighted = fansi.Str(parser.defaultHighlight(
          buffer.toVector,
          colors.comment(),
          colors.`type`(),
          colors.literal(),
          colors.keyword(),
          colors.error(),
          fansi.Attr.Reset
        ).mkString)
        val (newBuffer, offset) = SelectionFilter.mangleBuffer(
          selectionFilter, highlighted, cursor, colors.selected()
        )

        val newNewBuffer = HistoryFilter.mangleBuffer(
          historyFilter, newBuffer, cursor, fansi.Underlined.On
        )
        (newNewBuffer, offset)
      }
    )
    res
  }
}
