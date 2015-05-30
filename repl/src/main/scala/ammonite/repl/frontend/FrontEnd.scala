package ammonite.repl.frontend

import java.io.{OutputStream, InputStream}

import ammonite.repl._
import ammonite.terminal.Term.HistoryFilter
import fastparse._
import fastparse.core.Result
import jline.console.{completer, ConsoleReader}
import acyclic.file

import scala.annotation.tailrec
import scala.tools.nsc.interpreter._
import collection.JavaConversions._
import ammonite.terminal._
import ammonite.terminal.LazyList._
/**
 * All the mucky JLine interfacing code
 */
trait FrontEnd{
  def action(input: InputStream,
             output: OutputStream,
             shellPrompt: String,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: Seq[String]): Res[(String, Seq[String])]
}

object FrontEnd{
  object Ammonite extends FrontEnd{
    def action(input: InputStream,
               output: OutputStream,
               shellPrompt: String,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: Seq[String]) = {
      val multilineFilter: TermCore.Filter = {
        case TermState(13 ~: rest, b, c) => // Enter
          val code = b.mkString
          ammonite.repl.Parsers.Splitter.parse(code) match {
            case Result.Failure(_, index) if code.drop(index).trim() == "" =>
              val (first, last) = b.splitAt(c)
              TermState(rest, (first :+ '\n') ++ last, c + 1)
            case _ =>
              ammonite.terminal.Result(code)
          }
      }
      
      val historyFilter = new HistoryFilter(history.reverse)

      val code = TermCore.readLine(
        shellPrompt,
        System.in,
        System.out,
        historyFilter.filter orElse multilineFilter orElse Term.defaultFilter,
        displayTransform = (buffer, cursor) => (Highlighter.defaultHighlight(buffer), cursor)
      )
      code match{
        case None => Res.Exit
        case Some(code) =>
          Parsers.Splitter.parse(code) match{
            case Result.Success(value, idx) => Res.Success((code, value))
            case f: Result.Failure => Res.Failure(SyntaxError.msg(code, f.parser, f.index))
          }
      }
    }
    def update(r: Res[Evaluated]) = {

    }
  }

  object JLine extends FrontEnd{
    def action(input: InputStream,
               output: OutputStream,
               shellPrompt: String,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: Seq[String]) = {
      val term = new jline.UnixTerminal()
      term.init()
      val reader = new ConsoleReader(input, output, term)

      reader.setHistoryEnabled(true)
      var signatures = Seq.empty[String]
      reader.addCompleter(new jline.console.completer.Completer {

        def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
          val buf   = if (_buf == null) "" else _buf
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
          if (buffered.isEmpty) shellPrompt
          // Strip ANSI color codes, as described http://stackoverflow.com/a/14652763/871202
          else " " * shellPrompt.replaceAll("\u001B\\[[;\\d]*m", "").length
        )) match {
          case None => Res.Exit
          case Some(newCode) =>
            val code = buffered + "\n" + newCode
            Parsers.Splitter.parse(code) match {
              case Result.Failure(_, index) if code.drop(index).trim() == "" => readCode(code)
              case f: Result.Failure => Res.Failure(SyntaxError.msg(f.input, f.parser, f.index))
              case Result.Success(split, idx) =>
                Res.Success(code -> split)
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
