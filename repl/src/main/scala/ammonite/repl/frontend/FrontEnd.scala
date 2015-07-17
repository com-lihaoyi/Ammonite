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
             history: Seq[String]): Res[(String, Seq[String])]
}

object FrontEnd{
  object Ammonite extends FrontEnd{

    def width = Term.consoleDim("cols")
    def height = Term.consoleDim("lines")
    def tabulate(snippets: Seq[String], width: Int) = {
      val gap =   2
      val maxLength = snippets.maxBy(_.replaceAll("\u001B\\[[;\\d]*m", "").length).length + gap
      val columns = width / maxLength
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
               history: Seq[String]) = {

      readLine(reader, output, prompt, colors, compilerComplete, history) match{
        case None => Res.Exit
        case Some(code) =>
          Parsers.Splitter.parse(code) match{
            case Result.Success(value, idx) => Res.Success((code, value))
            case f: Result.Failure => Res.Failure(
              fastparse.core.SyntaxError.msg(f.input, f.traced.expected, f.index)
            )
          }
      }
    }
    def readLine(reader: java.io.Reader,
                 output: OutputStream,
                 prompt: String,
                 colors: Colors,
                 compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
                 history: Seq[String]) = {
      val writer = new OutputStreamWriter(output)
      val autocompleteFilter: TermCore.Filter = {
        case TermInfo(TermState(9 ~: rest, b, c), width) => // Enter
          val (newCursor, completions, details) = compilerComplete(c, b.mkString)
          if (completions.length == 0 && details.length == 0) TermState(rest, b, c)
          else {
            writer.write("\n")
            for (d <- details){
              writer.write(Highlighter.defaultHighlight(
                d.toVector,
                colors.comment(),
                colors.`type`(),
                colors.literal(),
                colors.keyword(),
                colors.reset()
              ).mkString)
              writer.write("\n")
            }

            writer.flush()

            if (completions.length == 0) TermState(rest, b, c)
            else {
              val common = findPrefix(completions.head, completions.last, 0)
              val colored = for(comp <- completions) yield {
                val (left, right) = comp.splitAt(common.length)
                colors.comment() + left + colors.reset() + right
              }
              tabulate(colored, width).foreach(writer.write)
              writer.flush()
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
      val selectionFilter = AdvancedFilters.SelectionFilter()
      TermCore.readLine(
        prompt,
        reader,
        writer,
        selectionFilter orElse
        AdvancedFilters.altFilter orElse
        AdvancedFilters.fnFilter orElse
        ReadlineFilters.navFilter orElse
        autocompleteFilter orElse
        historyFilter.filter orElse
        cutPasteFilter orElse
        multilineFilter orElse
        BasicFilters.all,
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
              (Highlighter.flattenIndices(newIndices, buffer), cursor + displayOffset)
            case _ => (Highlighter.flattenIndices(indices, buffer), cursor)
          }
        }
      )

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
               history: Seq[String]) = {

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
          case None => Res.Exit
          case Some(newCode) =>
            val code = buffered + newCode
            Parsers.split(code) match{
              case Some(Result.Success(value, idx)) => Res.Success(code -> value)
              case Some(f: Result.Failure) => Res.Failure(
                fastparse.core.SyntaxError.msg(f.input, f.traced.expected, f.index)
              )
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
