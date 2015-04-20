package ammonite.repl.frontend

import java.io.{OutputStream, InputStream}
import java.nio.channels.CompletionHandler

import ammonite.repl.{Timer, Evaluated, Res}
import jline.console.{completer, ConsoleReader}
import acyclic.file
import jline.console.completer.Completer

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
  def action(buffered: String): Res[String]
  def update(buffered: String, r: Res[Evaluated]): Unit
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
    val defaultHandler = reader.getCompletionHandler
    reader.setCompletionHandler(new completer.CompletionHandler {
      def complete(reader: ConsoleReader, candidates: JList[CharSequence], position: Int): Boolean = {
        if (!signatures.isEmpty){
          println()
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
        println()
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


    def action(buffered: String): Res[String] = for {
      _ <- Signaller("INT") {
        if (reader.getCursorBuffer.length() == 0) {
          reader.println("Ctrl-D to exit")
          reader.drawLine()
          reader.flush()
        } else {
          reader.setCursorPosition(0)
          reader.killLine()
        }
      }

      res <- Option(
        reader.readLine(
          if (buffered.isEmpty) shellPrompt + " "
          // Strip ANSI color codes, as described http://stackoverflow.com/a/14652763/871202
          else " " * (shellPrompt.replaceAll("\u001B\\[[;\\d]*m", "").length + 1)
        )
      ).map(Res.Success(_))
        .getOrElse(Res.Exit)

    } yield buffered + res

    def update(buffered: String, r: Res[Evaluated]) = r match{

      case Res.Buffer(line) =>
        /**
         * Hack to work around the fact that if nothing got entered into
         * the prompt, the `ConsoleReader`'s history wouldn't increase
         */
        if(line != buffered + "\n") reader.getHistory.removeLast()

      case Res.Success(ev) =>
        val last = reader.getHistory.size()-1
        reader.getHistory.set(last, buffered + reader.getHistory.get(last))

      case Res.Exit =>
        // Otherwise the terminal gets left in a funny
        // state after you exit the ammonite REPL
        term.restore()

      case _ =>
    }
  }
}
