package ammonite.repl.frontend

import java.io.{OutputStream, InputStream}

import ammonite.repl._
import fastparse.core.Result
import jline.console.{completer, ConsoleReader}
import acyclic.file

import scala.annotation.tailrec
import scala.tools.nsc.interpreter._
import collection.JavaConversions._

/**
 * All the mucky JLine interfacing code
 */
trait JLineFrontend{
  /**
   * The width of the terminal
   */
  def width: Int
  def action(): Res[Seq[String]]
  def update(r: Res[Evaluated]): Unit
}
object JLineFrontend{
  def apply(input: InputStream,
            output: OutputStream,
            shellPrompt: => String,
            compilerComplete: => (Int, String) => (Int, Seq[String], Seq[String]),
            initialHistory: Seq[String]): JLineFrontend
            = new JLineFrontend with jline.console.completer.Completer {

    val term = new jline.UnixTerminal()

    term.init()
    val reader = new ConsoleReader(input, output, term)
    def width = term.getWidth
    reader.setHistoryEnabled(true)
    reader.addCompleter(this)
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
    initialHistory.foreach(reader.getHistory.add)

    var signatures = Seq.empty[String]
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

    def history =
      reader.getHistory
            .entries()
            .map(_.value().toString)
            .toVector


    def action(): Res[Seq[String]] = for {
      _ <- Catching{ case e: jline.console.UserInterruptException =>
        if (e.getPartialLine == "") reader.println("Ctrl-D to exit")
        Res.Skip
      }
      res <- readCode("")
    } yield res

    @tailrec def readCode(buffered: String): Res[Seq[String]] = {
      Option(reader.readLine(
        if (buffered.isEmpty) shellPrompt + " "
        // Strip ANSI color codes, as described http://stackoverflow.com/a/14652763/871202
        else " " * (shellPrompt.replaceAll("\u001B\\[[;\\d]*m", "").length + 1)
      )) match {
        case None => Res.Exit
        case Some(newCode) =>
          val code = buffered + "\n" + newCode
          Parsers.Splitter.parse(code) match {
            case Result.Failure(_, index) if code.drop(index).trim() == "" => readCode(code)
            case f: Result.Failure => Res.Failure(SyntaxError.msg(f.input, f.parser, f.index))
            case Result.Success(split, idx) =>
              Res.Success(split)
          }
      }
    }

    def update(r: Res[Evaluated]) = r match{

      case Res.Success(ev) =>
        val last = reader.getHistory.size()-1
        reader.getHistory.set(last, reader.getHistory.get(last))

      case Res.Exit =>
        // Otherwise the terminal gets left in a funny
        // state after you exit the ammonite REPL
        term.restore()

      case _ =>
    }
  }
}
