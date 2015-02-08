package ammonite.repl

import java.io.{OutputStream, InputStream, PrintWriter, StringWriter}
import ammonite.repl.eval.{Evaluator, Preprocessor, Compiler}
import ammonite.repl.frontend.{DefaultReplAPI, ReplAPIHolder, ReplAPI}
import jline.console.ConsoleReader
import acyclic.file

import scala.annotation.tailrec
import scala.reflect.io.VirtualDirectory

class Repl(input: InputStream, output: OutputStream) {
  val reader = new ConsoleReader(input, output, term)
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  val compiler = new Compiler(dynamicClasspath)
  val mainThread = Thread.currentThread()
  val preprocess = new Preprocessor

  val term = new jline.UnixTerminal()

  term.init()

  val eval = new Evaluator(
    mainThread.getContextClassLoader,
    preprocess.apply,
    compiler.compile,
    compiler.importsFor
  )
  val completer = new frontend.Completer(eval.previousImportBlock, compiler.complete)
  reader.setHistoryEnabled(true)
  reader.addCompleter(completer)
  var buffered = ""
  compiler.importsFor("", eval.replBridgeCode)
  val cls = eval.evalClass(eval.replBridgeCode, "ReplBridge")

  val replAPI = new DefaultReplAPI({
    import collection.JavaConversions._
    reader.getHistory.entries().map(_.value().toString).toVector
  })

  Repl.initReplBridge(
    cls.asInstanceOf[Result.Success[Class[ReplAPIHolder]]].s,
    replAPI
  )

  def action(reader: ConsoleReader): Result[Evaluated] = for {
    _ <- Signaller("INT"){
      if (reader.getCursorBuffer.length() == 0) {
        println("Ctrl-D to exit")
      }else{
        reader.setCursorPosition(0); reader.killLine()
      }
    }

    _ <- Catching { case x: Throwable =>
      val sw = new StringWriter()
      x.printStackTrace(new PrintWriter(sw))

      sw.toString + "\nSomething unexpected went wrong =("
    }

    res <- Option(
      reader.readLine(
        if (buffered == "") replAPI.shellPrompt + " "
        // Strip ANSI color codes, as described http://stackoverflow.com/a/14652763/871202
        else " " * (replAPI.shellPrompt.replaceAll("\u001B\\[[;\\d]*m", "").length + 1)
      )
    ).map(Result.Success(_))
     .getOrElse(Result.Exit)
    out <- processLine(buffered + res)
  } yield out

  def processLine(line: String) = for {
    _ <- Signaller("INT"){
      mainThread.stop()
    }
  } yield eval.processLine(line)

  def run() = {
    @tailrec def loop(): Unit = {
      val r = action(reader)
      r match{
        case Result.Exit =>
          compiler.pressy.askShutdown()
          reader.println("Bye!")
        case Result.Buffer(line) =>

          /**
           * Hack to work around the fact that if nothing got entered into
           * the prompt, the `ConsoleReader`'s history wouldn't increase
           */
          if(line != buffered + "\n") reader.getHistory.removeLast()
          buffered = line + "\n"
          loop()
        case Result.Success(ev) =>
          val last = reader.getHistory.size()-1
          reader.getHistory.set(last, buffered + reader.getHistory.get(last))
          buffered = ""
          eval.update(r)
          reader.println(ev.msg)
          loop()
        case Result.Failure(msg) =>
          buffered = ""
          eval.update(r)
          reader.println(Console.RED + msg + Console.RESET)
          loop()
      }
    }
    loop()
  }


}

object Repl{
  def initReplBridge(holder: Class[ReplAPIHolder], api: ReplAPI) = {
    holder
      .getDeclaredMethods
      .find(_.getName.contains('$'))
      .get
      .invoke(null, api)
  }
  def main(args: Array[String]) = {
    val shell = new Repl(System.in, System.out)
    shell.run()
  }
}