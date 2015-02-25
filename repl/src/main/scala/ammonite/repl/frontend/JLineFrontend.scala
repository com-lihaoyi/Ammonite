package ammonite.repl.frontend

import java.io.{OutputStream, InputStream}

import ammonite.repl.{Timer, Evaluated, Result}
import jline.console.ConsoleReader
import acyclic.file

import scala.tools.nsc.interpreter._
import collection.JavaConversions._

/**
 * All the mucky JLine interfacing code
 */
trait JLineFrontend{
  def width: Int
  def action(buffered: String): Result[String]
  def update(buffered: String, r: Result[Evaluated]): Unit
}
object JLineFrontend{
  def apply(input: InputStream,
            output: OutputStream,
            shellPrompt: => String,
            previousImportBlock: => String,
            compilerComplete: => (Int, String) => (Int, Seq[String]),
            initialHistory: Seq[String]): JLineFrontend
            = new JLineFrontend with jline.console.completer.Completer {

    val term = new jline.UnixTerminal()

    term.init()
    val reader = new ConsoleReader(input, output, term)
    def width = term.getWidth
    reader.setHistoryEnabled(true)
    reader.addCompleter(this)
    reader.setExpandEvents(false)

    initialHistory.foreach(reader.getHistory.add)

    def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
      val buf   = if (_buf == null) "" else _buf
      val prev = previousImportBlock + "\nobject AutocompleteWrapper{\n"
      import collection.JavaConversions._
      val (completionBase, completions) = compilerComplete(
        cursor + prev.length,
        prev + buf + "\n}"
      )
      candidates.addAll(completions)
      completionBase - prev.length
    }

    def history =
      reader.getHistory
            .entries()
            .map(_.value().toString)
            .toVector

    def action(buffered: String): Result[String] = for {
      _ <- Signaller("INT") {
        if (reader.getCursorBuffer.length() == 0) {
          println("Ctrl-D to exit")
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
      ).map(Result.Success(_))
        .getOrElse(Result.Exit)

    } yield buffered + res

    def update(buffered: String, r: Result[Evaluated]) = r match{

      case Result.Buffer(line) =>
        /**
         * Hack to work around the fact that if nothing got entered into
         * the prompt, the `ConsoleReader`'s history wouldn't increase
         */
        if(line != buffered + "\n") reader.getHistory.removeLast()

      case Result.Success(ev) =>
        val last = reader.getHistory.size()-1
        reader.getHistory.set(last, buffered + reader.getHistory.get(last))


      case _ =>
    }
  }
}
