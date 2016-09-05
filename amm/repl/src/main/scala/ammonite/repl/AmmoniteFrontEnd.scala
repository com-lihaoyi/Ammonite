package ammonite.repl

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import ammonite.terminal.filters._
import GUILikeFilters.SelectionFilter
import ammonite.terminal._
import fastparse.core.Parsed
import acyclic.file
import ammonite.util.{Colors, Res}
import ammonite.runtime.Parsers
case class AmmoniteFrontEnd(extraFilters: Filter = Filter.empty) extends FrontEnd {

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
    val res = readLine(reader, output, prompt, colors, compilerComplete, history) match {
      case None => Res.Exit(())
      case Some(code) =>
        addHistory(code)
        Parsers.Splitter.parse(code) match {
          case Parsed.Success(value, idx) =>
            Res.Success((code, value))
          case Parsed.Failure(_, index, extra) =>
            Res.Failure(
              None,
              fastparse.core.ParseError.msg(extra.input, extra.traced.expected, index)
            )
        }
    }
    res
  }

  val cutPasteFilter = ReadlineFilters.CutPasteFilter()

  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: IndexedSeq[String]) = {

    val writer = new OutputStreamWriter(output)

    val autocompleteFilter: Filter = Filter.action(SpecialKeys.Tab) {
      case TermState(rest, b, c, _) =>
        val (newCursor, completions, details) = compilerComplete(c, b.mkString)

        lazy val common = FrontEndUtils.findPrefix(completions, 0)
        val completions2 = for (comp <- completions) yield {

          val (left, right) = comp.splitAt(common.length)
          (colors.comment()(left) ++ right).render
        }
        val stdout =
          FrontEndUtils.printCompletions(completions2, details).mkString

        if (details.nonEmpty || completions.isEmpty)
          Printing(TermState(rest, b, c), stdout)
        else {
          val newBuffer = b.take(newCursor) ++ common ++ b.drop(c)
          Printing(TermState(rest, newBuffer, newCursor + common.length), stdout)
        }

    }

    // Enter
    val multilineFilter = Filter.action(
      SpecialKeys.NewLine,
      ti => Parsers.split(ti.ts.buffer.mkString).isEmpty
    ) {
      case TermState(rest, b, c, _) => BasicFilters.injectNewLine(b, c, rest)
    }

    val historyFilter = new HistoryFilter(() => history.reverse)
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

    Terminal.readLine(prompt, reader, writer, allFilters)
  }
}
