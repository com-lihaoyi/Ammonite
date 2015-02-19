package ammonite.repl

import java.io.{OutputStream, InputStream}
import java.lang.reflect.InvocationTargetException
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter

import scala.annotation.tailrec

class Repl(input: InputStream,
           output: OutputStream,
           colorSet: ColorSet = ColorSet.Default,
           shellPrompt0: String = "@") {

  var shellPrompt = Ref(shellPrompt0)

  lazy val frontEnd: JLineFrontend = new frontend.JLineFrontend(
    input,
    output,
    colorSet.prompt + shellPrompt() + Console.RESET,
    interp.eval.previousImportBlock,
    interp.pressy.complete
  )

  lazy val interp: Interpreter = new Interpreter(
    frontEnd.update,
    shellPrompt,
    () => frontEnd.history,
    ColorSet.Default
  )

  def action() = for{
    _ <- Catching { case x: Throwable =>
      var current = x
      var output = ""
      while(current != null) {
        output += current + "\n" + current.getStackTrace.map("  " + _).mkString("\n") + "\n"
        current = current.getCause
      }
      Result.Failure(output + "\nSomething unexpected went wrong =(")
    }
    res <- frontEnd.action()
    _ <- Signaller("INT") { interp.mainThread.stop() }
    _ <- Catching{
      case ex: InvocationTargetException
        if ex.getCause.getCause.isInstanceOf[ReplExit.type]  =>
        Result.Exit
      case ex: InvocationTargetException
        if ex.getCause.isInstanceOf[ExceptionInInitializerError]  =>
        val userEx = ex.getCause.getCause
        val trace =
          userEx
            .getStackTrace
            .takeWhile(x => !(x.getMethodName == "$main"))
            .mkString("\n")

        Result.Failure(userEx.toString + "\n" + trace)
      case ex: InvocationTargetException
        if ex.getCause.isInstanceOf[ThreadDeath]  =>
        // Clear the interrupted status
        Thread.interrupted()
        Result.Failure("\nInterrupted!")
    }
    out <- interp.eval.processLine(res)
  } yield {
    out.msg.foreach(print)
    println()
    out
  }

  def run() = {
    @tailrec def loop(): Unit = {
      val res = action()
      if (interp.handleOutput(res)) loop()
    }
    loop()
  }
}

object Repl{

  def main(args: Array[String]) = {
    val shell = new Repl(System.in, System.out)
    shell.run()
  }
}
