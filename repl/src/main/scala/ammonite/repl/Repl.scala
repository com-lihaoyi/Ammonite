package ammonite.repl

import java.io.{OutputStream, InputStream}
import java.lang.reflect.InvocationTargetException
import ammonite.pprint
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter

import scala.annotation.tailrec
import scala.util.Try

class Repl(input: InputStream,
           output: OutputStream,
           colorSet: ColorSet = ColorSet.Default,
           pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig,
           shellPrompt0: String = "@",
           initialHistory: Seq[String] = Nil,
           saveHistory: String => Unit = _ => (),
           predef: String = Repl.defaultPredef) {

  var shellPrompt = Ref(shellPrompt0)

  lazy val frontEnd = JLineFrontend(
    input,
    output,
    colorSet.prompt + shellPrompt() + Console.RESET,
    interp.eval.previousImportBlock,
    interp.pressy.complete,
    initialHistory
  )

  lazy val interp: Interpreter = new Interpreter(
    frontEnd.update,
    shellPrompt,
    pprintConfig,
    colorSet
  )

  def action() = for{
    _ <- Catching { case x: Throwable =>
      println("THROWN")
      var current = x
      var output = ""
      while(current != null) {
        output += current + "\n" + current.getStackTrace.map("  " + _).mkString("\n") + "\n"
        current = current.getCause
      }
      Result.Failure(output + "\nSomething unexpected went wrong =(")
    }
    line <- frontEnd.action()
    _ <- Signaller("INT") { interp.mainThread.stop() }
    _ <- Catching{
      case ex: InvocationTargetException
        if ex.getCause.getCause.isInstanceOf[ReplExit.type]  =>
        Result.Exit

      case ex: InvocationTargetException
        if ex.getCause.isInstanceOf[ThreadDeath] =>
        // Clear the interrupted status
        Thread.interrupted()
        Result.Failure("\nInterrupted!")

      case ex: InvocationTargetException  =>
        val userEx = ex.getCause.getCause
        val trace =
          userEx
            .getStackTrace
            .takeWhile(x => !(x.getMethodName == "$main"))
            .mkString("\n")

        Result.Failure(userEx.toString + "\n" + trace)

      case ex: ThreadDeath =>
        Thread.interrupted()
        Result.Failure("\nInterrupted!")
      case e =>
        println(e)
        throw e
    }
    out <- interp.processLine(line, saveHistory)
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
  val defaultPredef = """"""
  def main(args: Array[String]) = {
    import ammonite.shell._
    val saveFile = home/".amm"
    val delimiter = "\n\n\n"
    val shell = new Repl(
      System.in, System.out,
      initialHistory = Try{read! saveFile}.getOrElse("").split(delimiter),
      saveHistory = s => write.append(home/".amm", s + delimiter)
    )
    shell.run()
  }
}
