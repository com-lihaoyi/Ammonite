package ammonite.repl.frontend

import java.io.{OutputStreamWriter, OutputStream, InputStream}

import ammonite.repl._
import fastparse.core.Result
import jline.console.{completer, ConsoleReader}
import acyclic.file

import scala.annotation.tailrec
import scala.tools.nsc.interpreter
import ammonite.terminal._
import ammonite.terminal.LazyList._

import scala.tools.nsc.interpreter.JList
import scalaparse.Scala._
import scalaparse.syntax.Identifiers._

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
             history: Seq[String],
             addHistory: String => Unit): Res[(String, Seq[String])]
}

object FrontEnd{
  object Ammonite extends FrontEnd{

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
    @tailrec def findPrefix(a: String, b: String, index: Int): String = {
      if (index >= a.length || index >= b.length || a(index) != b(index)) b.take(index)
      else findPrefix(a, b, index + 1)
    }

    def action(input: InputStream,
               reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: Seq[String],
               addHistory: String => Unit) = {
      Timer("FrontEnd.Ammonite.action start")
      val res = readLine(reader, output, prompt, colors, compilerComplete, history) match{
        case None => Res.Exit(())
        case Some(code) =>
          addHistory(code)
          Parsers.Splitter.parse(code) match{
            case Result.Success(value, idx) => Res.Success((code, value))
            case f: Result.Failure => Res.Failure(
              fastparse.core.SyntaxError.msg(f.input, f.traced.expected, f.index)
            )
          }
      }
      Timer("FrontEnd.Ammonite.action end")
      res
    }

    def doSomething(completions: Seq[String],
                    details: Seq[String],
                    writer: OutputStreamWriter,
                    rest: LazyList[Int],
                    b: Vector[Char],
                    c: Int) = {
      // If we find nothing, we find nothing
      if (completions.length == 0 && details.length == 0) None
      else {
        writer.write("\n")
        details.foreach(writer.write)
        writer.write("\n")
      }

      writer.flush()
      // If we find no completions, we've already printed the details to abort
      if (completions.length == 0) None
      else {

        tabulate(completions, Ammonite.this.width).foreach(writer.write)
        writer.flush()
        // If the current prefix already matches a detailed result, we've
        // already printed the messages but no need to modify buffer
        if (details.length != 0) None
        else Some()
      }
    }

    def readLine(reader: java.io.Reader,
                 output: OutputStream,
                 prompt: String,
                 colors: Colors,
                 compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
                 history: Seq[String]) = {
      Timer("FrontEnd.Ammonite.readLine start")
      val writer = new OutputStreamWriter(output)

      val autocompleteFilter: TermCore.Filter = {
        case TermInfo(TermState(9 ~: rest, b, c), width) => // Enter
          val p = Parsers.PathComplete.RevPath.parse(b.reverse.mkString, b.length - c)
          p match{
            case Result.Success((base, seq, frag, cursorOffset), _) =>
              import ammonite.ops._
              if (base == None) {
                if (exists(cwd/seq)) {
                  val options = ls ! cwd / seq |? (_.last.startsWith(frag.getOrElse(""))) | (_.last)
                  val (completions, details) = options.partition(_ != frag.getOrElse(""))
                  lazy val common = findPrefix(completions.head, completions.last, 0)
                  doSomething(completions, details, writer, rest, b, c) match{
                    case None => TermState(rest, b, c)
                    case Some(_) =>
                      val newBuffer = b.take(c - cursorOffset + 1) ++ Parsers.PathComplete.stringSymWrap(common) ++ b.drop(c)
                      TermState(rest, newBuffer, c - cursorOffset + common.length + 1)
                  }
                }else TermState(rest, b, c)
              }else TermState(rest, b, c)


            case f: Result.Failure =>
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
              lazy val common = findPrefix(completions.head, completions.last, 0)
              val completions2 = for(comp <- completions) yield {

                val (left, right) = comp.splitAt(common.length)
                colors.comment() + left + colors.reset() + right
              }
              doSomething(completions2, details2, writer, rest, b, c) match{
                case None =>
                  TermState(rest, b, c)
                case Some(_) =>
                  val newBuffer = b.take(newCursor) ++ common ++ b.drop(c)
                  TermState(rest, newBuffer, newCursor + common.length)
              }
          }
      }

      val multilineFilter: TermCore.Filter = {
        case TermState(13 ~: rest, b, c) => // Enter
          val code = b.mkString
          ammonite.repl.Parsers.split(code) match {
            case None =>
              val (first, last) = b.splitAt(c)
              TermState(rest, (first :+ '\n') ++ last, c + 1)
            case Some(_)  =>
              ammonite.terminal.Result(code)
          }
      }

      val historyFilter = ReadlineFilters.HistoryFilter(() => history.reverse)
      val cutPasteFilter = ReadlineFilters.CutPasteFilter()
      val selectionFilter = GUILikeFilters.SelectionFilter()

      val allFilters =
        selectionFilter orElse
        GUILikeFilters.altFilter orElse
        GUILikeFilters.fnFilter orElse
        ReadlineFilters.navFilter orElse
        autocompleteFilter orElse
        historyFilter.filter orElse
        cutPasteFilter orElse
        multilineFilter orElse
        BasicFilters.all

      Timer("FrontEnd.Ammonite.readLine 1")
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
              (Highlighter.flattenIndices(newIndices, buffer) ++ Console.RESET, cursor + displayOffset)
            case _ => (Highlighter.flattenIndices(indices, buffer) ++ Console.RESET, cursor)
          }
        }
      )
      Timer("TermCore.readLine")
      res
    }
  }
  object JLineUnix extends JLineTerm(() => new jline.UnixTerminal())
  object JLineWindows extends JLineTerm(() => new jline.WindowsTerminal())
  class JLineTerm(makeTerm: () => jline.Terminal) extends FrontEnd{
    def width = makeTerm().getWidth
    def height = makeTerm().getHeight

    def action(input: InputStream,
               reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: Seq[String],
               addHistory: String => Unit) = {

      val term = makeTerm()
      term.init()
      val reader = new ConsoleReader(input, output, term)
      reader.setHistoryEnabled(true)
      var signatures = Seq.empty[String]
      reader.addCompleter(new jline.console.completer.Completer {

        def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
          val buf = if (_buf == null) "" else _buf
          import collection.JavaConversions._
          val (completionBase, completions, sigs) = compilerComplete(
            cursor,
            buf
          )
          if (!completions.isEmpty) {
            candidates.addAll(completions.sorted)
            signatures = sigs.sorted
          } else if (!sigs.isEmpty){
            reader.println()
            sigs.foreach(reader.println)
            reader.drawLine()
          }

          completionBase
        }
      })
      reader.setExpandEvents(false)
      reader.setHandleUserInterrupt(true)
      val defaultHandler = reader.getCompletionHandler
      reader.setCompletionHandler(new completer.CompletionHandler {
        def complete(reader: ConsoleReader, candidates: JList[CharSequence], position: Int): Boolean = {
          if (!signatures.isEmpty){
            reader.println()
            signatures.foreach(reader.println)
            reader.drawLine()
          }
          defaultHandler.complete(reader, candidates, position)
        }
      })

      history.foreach(reader.getHistory.add)

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
              case Some(Result.Success(value, idx)) =>
                addHistory(code)
                Res.Success(code -> value)
              case Some(f: Result.Failure) =>
                addHistory(code)
                Res.Failure(fastparse.core.SyntaxError.msg(f.input, f.traced.expected, f.index))
              case None => readCode(code + "\n")
            }
        }
      }


      try for {
        _ <- Catching{ case e: jline.console.UserInterruptException =>
          if (e.getPartialLine == "") reader.println("Ctrl-D to exit")
          Res.Skip
        }
        res <- readCode("")
      } yield res
      finally term.restore()
    }
  }
}
