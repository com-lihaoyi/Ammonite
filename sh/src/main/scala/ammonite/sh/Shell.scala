package ammonite.sh

import java.io.{OutputStream, InputStream, PrintWriter, StringWriter}
import java.lang.reflect.InvocationTargetException

import ammonite.sh.eval.{Evaluator, Preprocessor, Compiler}
import jline.console.ConsoleReader
import acyclic.file

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.reflect.io.VirtualDirectory
import scala.util.Try

class Shell() {
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
  compiler.importsFor("", eval.shellBridgeCode)
  val cls = eval.evalClass(eval.shellBridgeCode, "ShellBridge")

  Shell.initShellBridge(
    cls.asInstanceOf[Result.Success[Class[ShellAPIHolder]]].s,
    new ShellAPIs {
      def exit: Unit = ()
      def help: String = "Hello!"
      def history: Seq[String] = Seq("1")
      def shellPPrint[T: WeakTypeTag](value: => T, ident: String) = {
        Console.CYAN + ident + Console.RESET + ": " +
        Console.GREEN + weakTypeOf[T].toString + Console.RESET
      }
    }
  )
  def action(reader: ConsoleReader): Result[Evaluated] = for {
    _ <- Signaller("INT", () => println("Ctrl-D to Exit"))

    _ <- Catching { case x: Throwable =>
      val sw = new StringWriter()
      x.printStackTrace(new PrintWriter(sw))

      sw.toString + "\n" +
        "Something unexpected went wrong =("
    }

    res <- Option(reader.readLine(Console.MAGENTA + "scala> " + Console.RESET))
                          .map(Result.Success(_)).getOrElse(Result.Exit)
    out <- processLine(res)
  } yield out

  def processLine(line: String) = for {
    _ <- Signaller("INT", () => mainThread.stop())
  } yield eval.processLine(line)


  def run(input: InputStream, output: OutputStream) = {
    val reader = new ConsoleReader(input, output, term)
    @tailrec def loop(): Unit = {
      val r = action(reader)

      r match{
        case Result.Exit =>
          compiler.pressy.askShutdown()
          reader.println("Bye!")
        case Result.Success(ev) =>
          eval.update(r)
          reader.println(ev.msg)
          loop()
        case Result.Failure(msg) =>
          eval.update(r)
          reader.println(Console.RED + msg + Console.RESET)
          loop()
      }
    }
    loop()
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
    val shell = new Shell()
    shell.run(System.in, System.out)
  }


}