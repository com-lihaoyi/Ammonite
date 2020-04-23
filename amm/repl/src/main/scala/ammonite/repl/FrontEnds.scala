package ammonite.repl

import java.io.{InputStream, OutputStream}

import scala.collection.JavaConverters._
import fastparse.Parsed
import fastparse.ParserInput
import org.jline.reader.{Highlighter => _, _}
import org.jline.reader.impl.history.DefaultHistory

import org.jline.terminal._
import org.jline.utils.AttributedString
import ammonite.util.{Catching, Colors, Res}
import ammonite.repl.api.FrontEnd
import ammonite.interp.{Parsers, Preprocessor}
import org.jline.reader.impl.DefaultParser


object FrontEnds {
  object JLineUnix extends JLineTerm
  object JLineWindows extends JLineTerm
  class JLineTerm() extends FrontEnd {

    private val term = TerminalBuilder.builder().build()
    
    private val readerBuilder = LineReaderBuilder.builder().terminal(term)
    private val ammHighlighter = new AmmHighlighter()
    private val ammCompleter = new AmmCompleter(ammHighlighter)
    private val ammParser = new AmmParser()
    readerBuilder.highlighter(ammHighlighter)
    readerBuilder.completer(ammCompleter)
    readerBuilder.parser(ammParser)
    readerBuilder.history(new DefaultHistory())
    readerBuilder.option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
    readerBuilder.option(LineReader.Option.INSERT_TAB, true) // TAB on blank line
    readerBuilder.option(LineReader.Option.AUTO_FRESH_LINE, true) // if not at start of line
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
            val pl = reader.getParser.parse(code, 0).asInstanceOf[AmmParser#AmmoniteParsedLine]
            Res.Success(code -> pl.stmts)
          case None => Res.Exit(())
        }
      }

      for {
        _ <- Catching {
          case e: UserInterruptException =>
            if (e.getPartialLine == "") {
              term.writer().println("Ctrl-D to exit")
              term.flush()
            }
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

class AmmCompleter(highlighter: org.jline.reader.Highlighter) extends Completer {
  // completion varies from action to action
  var compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]) =
    (x, y) => (0, Seq.empty, Seq.empty)
  
  // used when making a candidate
  private val leftDelimiters  = Set('.')
  private val rightDelimiters = Set('.', '(', '{', '[')

  override def complete(reader: LineReader,
                        line: ParsedLine,
                        candidates: java.util.List[Candidate]): Unit = {
    val (completionBase, completions, sigs) = compilerComplete(
      line.cursor(),
      line.line()
    )
    // display method signature(s)
    if (sigs.nonEmpty) {
      reader.getTerminal.writer.println()
      sigs.foreach{ sig =>
        val sigHighlighted = highlighter.highlight(reader, sig).toAnsi
        reader.getTerminal.writer.println(sigHighlighted)
      }
      reader.callWidget(LineReader.REDRAW_LINE)
      reader.callWidget(LineReader.REDISPLAY)
      reader.getTerminal.flush()
    }
    // add suggestions
    completions.sorted.foreach { c =>
      val candidate = makeCandidate(line.word, line.wordCursor, c)
      candidates.add(new Candidate(candidate, c, null, null, null, null, false))
    }
  }

  /** Makes a full-word candidate based on autocomplete candidate */
  private def makeCandidate(word: String, wordCursor: Int, candidate: String): String = {
    val leftFromCursor  = word.substring(0, wordCursor)
    val rightFromCursor = word.substring(wordCursor)
    val left = leftFromCursor.reverse.dropWhile(c => !leftDelimiters.contains(c)).reverse
    val right = rightFromCursor.dropWhile(c => !rightDelimiters.contains(c))
    left + candidate + right
  }
}

class AmmParser extends Parser {
  class AmmoniteParsedLine(line: String, words: java.util.List[String],
                            wordIndex: Int, wordCursor: Int, cursor: Int,
                            val stmts: Seq[String] = Seq.empty // needed for interpreter
                          ) extends defaultParser.ArgumentList(line, words, wordIndex, wordCursor, cursor)

  var addHistory: String => Unit = x => ()

  val defaultParser = new org.jline.reader.impl.DefaultParser

  override def parse(line: String, cursor: Int, context: Parser.ParseContext): ParsedLine = {
    // let JLine's default parser to handle JLine words and indices
    val defParLine = defaultParser.parse(line, cursor, context)
    val words = defParLine.words
    val wordIndex = defParLine.wordIndex // index of the current word in the list of words
    val wordCursor = defParLine.wordCursor // cursor position within the current word
    
    Parsers.split(line) match {
      case Some(Parsed.Success(stmts, idx)) =>
        addHistory(line)
        // if ENTER and not at the end of input -> newline
        if (context == Parser.ParseContext.ACCEPT_LINE && cursor != line.length) {
          throw new EOFError(-1, -1, "Newline entered")
        } else {
          new AmmoniteParsedLine(line, words, wordIndex, wordCursor, cursor, stmts)
        }
      case Some(f @ Parsed.Failure(p, idx, extra)) =>
        // we "accept the failure" only when ENTER is pressed, loops forever otherwise...
        // https://groups.google.com/d/msg/jline-users/84fPur0oHKQ/bRnjOJM4BAAJ
        if (context == Parser.ParseContext.ACCEPT_LINE) {
          addHistory(line)
          throw new SyntaxError(
            Preprocessor.formatFastparseError("(console)", line, f)
          )
        } else {
          new AmmoniteParsedLine(line, words, wordIndex, wordCursor, cursor)
        }
      case None =>
        // when TAB is pressed (COMPLETE context) return a line so that it can show suggestions
        // else throw EOFError to signal that input isn't finished
        if (context == Parser.ParseContext.COMPLETE) {
          new AmmoniteParsedLine(line, words, wordIndex, wordCursor, cursor)
        } else {
          throw new EOFError(-1, -1, "Missing closing paren/quote/expression")
        }
    }
  }
}

class SyntaxError(val msg: String) extends RuntimeException

class AmmHighlighter extends org.jline.reader.Highlighter {

  var colors: Colors = Colors.Default
  def setErrorIndex(x$1: Int): Unit = ()
  def setErrorPattern(x$1: java.util.regex.Pattern): Unit = ()
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
