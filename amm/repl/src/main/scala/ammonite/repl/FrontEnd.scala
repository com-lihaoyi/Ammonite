package ammonite.repl

import java.io.{InputStream, OutputStream}

import fastparse.core.Parsed
import org.jline.reader._
import org.jline.terminal._
import org.jline.reader.impl.history.DefaultHistory
import org.jline.utils.AttributedString
import ammonite.util.{Catching, Colors, Res}
import ammonite.interp.Parsers
import ammonite.util.Util.newLine

import scala.annotation.tailrec

/**
 * All the mucky JLine interfacing code
 */
trait FrontEnd{
  def width: Int
  def height: Int
  def action(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             colors: Colors,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: IndexedSeq[String],
             addHistory: String => Unit): Res[(String, Seq[String])]
}

object FrontEnd{
  object JLineUnix extends JLineTerm
  object JLineWindows extends JLineTerm
  class JLineTerm() extends FrontEnd{

    private val term = TerminalBuilder.builder().build()
    private val readerBuilder = LineReaderBuilder.builder().terminal(term)
    private val ammCompleter = new AmmCompleter()
    private val ammHighlighter = new AmmHighlighter()
    readerBuilder.completer(ammCompleter)
    readerBuilder.highlighter(ammHighlighter)
    readerBuilder.history(new DefaultHistory())
    private val reader = readerBuilder.build()

    def width = term.getWidth
    def height = term.getHeight

    def action(jInput: InputStream,
               jReader: java.io.Reader,
               jOutput: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               historyValues: IndexedSeq[String],
               addHistory: String => Unit) = {

      ammCompleter.compilerComplete = compilerComplete
      ammHighlighter.colors = colors
      historyValues.foreach(reader.getHistory.add)

      @tailrec def readCode(buffered: String): Res[(String, Seq[String])] = {
        Option(reader.readLine(
          if (buffered.isEmpty) prompt
          // Strip ANSI color codes, as described http://stackoverflow.com/a/14652763/871202
          else " " * prompt.replaceAll("\u001B\\[[;\\d]*m", "").length
        )) match {
          case None => Res.Exit(())
          case Some(newCode) =>
            val code = buffered + newCode
            Parsers.split(code) match{
              case Some(Parsed.Success(value, idx)) =>
                addHistory(code)
                Res.Success(code -> value)
              case Some(Parsed.Failure(p, index, extra)) =>
                addHistory(code)
                Res.Failure(
                  fastparse.core.ParseError.msg(extra.input, extra.traced.expected, index)
                )
              case None => readCode(code + newLine)
            }
        }
      }

      for {
        _ <- Catching {
          case e: UserInterruptException =>
            if (e.getPartialLine == "") term.writer().println("Ctrl-D to exit")
            Res.Skip
          case e: EndOfFileException =>
            Res.Exit("user exited")
        }
        res <- readCode("")
      } yield res
    }
  }
}

class AmmCompleter extends Completer {
  // completion varies from action to action
  var compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]) =
    (x, y) => (0, Seq.empty, Seq.empty)

  override def complete(reader: LineReader, line: ParsedLine, candidates: java.util.List[Candidate]): Unit = {
    val (completionBase, completions, sigs) = compilerComplete(
      line.cursor(),
      line.line()
    )
    completions.sorted.foreach { c =>
      // if member selection, concatenate compiler suggestion to variable
      val candidate = if (line.line().endsWith(".")) (line.word() + c) else c
      candidates.add(new Candidate(candidate, c, null, null, null, null, true))
    }
  }
}

class AmmHighlighter extends Highlighter {

  var colors: Colors = Colors.Default

  override def highlight(reader: LineReader, buffer: String): AttributedString = {
    val hl = Highlighter.defaultHighlight(
      buffer.toVector,
      colors.comment(),
      colors.`type`(),
      colors.literal(),
      colors.keyword(),
      fansi.Attr.Reset
    ).mkString
    AttributedString.fromAnsi(hl)
  }
}