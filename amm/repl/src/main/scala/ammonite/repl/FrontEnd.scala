package ammonite.repl

import java.io.{InputStream, OutputStream}

import scala.collection.JavaConverters._
import fastparse.core.Parsed
import org.jline.reader._
import org.jline.terminal._
import org.jline.reader.impl.history.DefaultHistory
import org.jline.utils.AttributedString
import org.jline.reader.impl.DefaultParser.ArgumentList
import ammonite.util.{Catching, Colors, Res}
import ammonite.interp.Parsers
import fastparse.utils.ParserInput

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
    private val ammParser = new AmmParser()
    readerBuilder.completer(ammCompleter)
    readerBuilder.parser(ammParser)
    readerBuilder.highlighter(ammHighlighter)
    readerBuilder.history(new DefaultHistory())
    readerBuilder.option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
    readerBuilder.option(LineReader.Option.INSERT_TAB, true)
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
      ammParser.addHistory = addHistory
      ammHighlighter.colors = colors
      historyValues.foreach(reader.getHistory.add)

      def readCode(): Res[(String, Seq[String])] = {
        Option(reader.readLine(prompt)) match {
          case Some(code) =>
            val pl = reader.getParser.parse(code, 0)
            Res.Success(code -> pl.words().asScala)
          case None => Res.Exit(())
        }
      }

      for {
        _ <- Catching {
          case e: UserInterruptException =>
            if (e.getPartialLine == "") term.writer().println("Ctrl-D to exit")
            Res.Skip
          case e: SyntaxError =>
            Res.Failure(e.msg)
          case e: EndOfFileException =>
            Res.Exit("user exited")
        }
        res <- readCode()
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
      val candidate = if (line.word().contains(".")) {
        val lastDotIndex = line.word().lastIndexOf(".")
        val prefix = line.word().substring(0, lastDotIndex + 1)
        prefix + c
      } else {
        c
      }
      candidates.add(new Candidate(candidate, c, null, null, null, null, false))
    }
  }
}

class AmmParser extends Parser {

  var addHistory: String => Unit = x => ()

  override def parse(line: String, cursor: Int, context: Parser.ParseContext): ParsedLine = {
    val words = new java.util.ArrayList[String]()
    var wordCursor = -1
    var wordIndex = -1
    Parsers.split(line) match {
      case Some(Parsed.Success(value, idx)) =>
        addHistory(line)
        words.addAll(value.asJava)
        if (cursor == line.length && words.size > 0) {
          wordIndex = words.size - 1
          wordCursor = words.get(words.size - 1).length
        }
        new ArgumentList(line, words, wordIndex, wordCursor, cursor)
      case Some(Parsed.Failure(p, idx, extra)) =>
        addHistory(line)
        throw new SyntaxError(
          fastparse.core.ParseError.msg(extra.input, extra.traced.expected, idx)
        )
      case None => // continue input
        if (context == Parser.ParseContext.COMPLETE) {
          new ArgumentList(line, words, wordIndex, wordCursor, cursor)
        } else {
          throw new EOFError(-1, -1, "Missing closing paren/quote/expression")
        }
    }
  }
}

class SyntaxError(val msg: String) extends RuntimeException

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