package ammonite.sh

import java.io.{OutputStream, InputStream, PrintWriter, StringWriter}
import java.lang.reflect.InvocationTargetException

import ammonite.sh.eval.{Evaluator, Preprocessor, Compiler}
import jline.console.ConsoleReader
import acyclic.file

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.io.VirtualDirectory
import scala.util.Try

class Shell() extends ShellAPIs{
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  val compiler = new Compiler(dynamicClasspath)
  val mainThread = Thread.currentThread()
  val preprocess = new Preprocessor
  val term = new jline.UnixTerminal()
  term.init()

  val defaultImports = compiler.askCustomImports("", """
    object Shell extends ammonite.sh.ShellAPIHolder{}
  """)

  val eval = new Evaluator(
    mainThread.getContextClassLoader,
    preprocess.apply,
    compiler.compile,
    compiler.askCustomImports
  )

  val cls = eval.evalClass("""
    object Shell extends ammonite.sh.ShellAPIHolder
  """, "Shell") match{
    case Result.Success(e) =>
      println("EVALED CLS " + e)
      e
  }


  cls.asInstanceOf[Class[ShellAPIHolder]]
    .getDeclaredMethods
    .find(_.getName.contains('$'))
    .get
    .invoke(null, this)
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
      eval.update(r)
      r match{
        case Result.Exit => reader.println("Bye!")
        case Result.Success(ev) =>
          reader.println(ev.msg)
          loop()
        case Result.Failure(msg) =>
          reader.println(Console.RED + msg + Console.RESET)
          loop()
      }
    }
    loop()
  }

  override def exit: Unit = ()

  override def help: String = "Hello!"

  override def history: Seq[String] = Seq("1")
}

object Shell{
  def main(args: Array[String]) = {
    val shell = new Shell()
    shell.run(System.in, System.out)
  }
  import scala.reflect.runtime.universe._
  def typeString[T: WeakTypeTag](t: => T) = weakTypeOf[T].toString

}