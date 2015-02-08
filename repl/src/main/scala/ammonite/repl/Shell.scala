package ammonite.repl

import java.io.{OutputStream, InputStream, PrintWriter, StringWriter}
import ammonite.pprint
import ammonite.pprint.PPrint
import ammonite.repl.eval.{Evaluator, Preprocessor, Compiler}
import jline.console.ConsoleReader
import acyclic.file
import jline.console.completer.Completer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.Completion.Candidates
import scala.tools.nsc.interpreter._

class Shell(input: InputStream, output: OutputStream) {
  val reader = new ConsoleReader(input, output, term)

  reader.setHistoryEnabled(true)
  reader.addCompleter(ShellCompleter)

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
  var buffered = ""
  compiler.importsFor("", eval.shellBridgeCode)
  val cls = eval.evalClass(eval.shellBridgeCode, "ShellBridge")

  object Apis extends ShellAPIs {
    var shellPrompt: String = Console.MAGENTA + "scala>" + Console.RESET
    def help = "Hello!"
    def history: Seq[String] = {
      import collection.JavaConversions._
      reader.getHistory.entries().map(_.value().toString).toVector
    }
    def shellPPrint[T: WeakTypeTag](value: => T, ident: String) = {
      Console.CYAN + ident + Console.RESET + ": " +
      Console.GREEN + weakTypeOf[T].toString + Console.RESET
    }
  }

  Shell.initShellBridge(
    cls.asInstanceOf[Result.Success[Class[ShellAPIHolder]]].s,
    Apis
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
        if (buffered == "") Apis.shellPrompt + " "
        // Strip ANSI color codes, as described http://stackoverflow.com/a/14652763/871202
        else " " * (Apis.shellPrompt.replaceAll("\u001B\\[[;\\d]*m", "").length + 1)
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

  object ShellCompleter extends Completer {
    def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
      val buf   = if (_buf == null) "" else _buf
      val prevImports = eval.previousImportBlock
      val prev = prevImports + "\n" + "object Foo{\n"
      import collection.JavaConversions._
      val (newCursor, completions) = compiler.complete(
        cursor + prev.length,
        prev + buf + "\n}"
      )
      candidates.addAll(completions)
      newCursor - prev.length
    }
  }
}

object Shell{
  def initShellBridge(holder: Class[ShellAPIHolder], api: ShellAPIs) = {
    holder
      .getDeclaredMethods
      .find(_.getName.contains('$'))
      .get
      .invoke(null, api)
  }
  def main(args: Array[String]) = {
    val shell = new Shell(System.in, System.out)
    shell.run()
  }
}