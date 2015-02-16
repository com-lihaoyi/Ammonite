package ammonite.repl

import java.io.{OutputStream, InputStream}
import java.lang.reflect.InvocationTargetException
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.{Pressy, Interpreter}

import scala.annotation.tailrec

class Repl(input: InputStream, output: OutputStream) {
  val interp: Interpreter = new Interpreter(replApi)

  lazy val replApi: ReplAPI = new DefaultReplAPI(
    frontEnd.history.dropRight(1),
    interp.loadJar,
    lines => for(line <- lines) handleOutput(interp.eval.processLine(line)),
    (groupId, artifactId, version) => {
      interp.loadJar(IvyThing.resolveArtifact(groupId, artifactId, version))
    },
    () => frontEnd.reader.clearScreen(),
    () => interp.init(),
    ColorSet.Default,
    ammonite.pprint.Config.Colors.PPrintConfig
  )

  val frontEnd = new frontend.JLineFrontend(
    input,
    output,
    replApi.shellPrompt,
    interp.eval.previousImportBlock,
    interp.pressy.complete
  )
  def handleOutput(res: Result[Evaluated]) = {
    frontEnd.update(res)

    res match{
      case Result.Skip => true
      case Result.Buffer(line) => true
      case Result.Exit =>
        println("Bye!")
        interp.pressy.shutdownPressy()
        false
      case Result.Success(ev) =>
        interp.eval.update(ev.imports)
        true
      case Result.Failure(msg) =>
        println(Console.RED + msg + Console.RESET)
        true
    }
  }
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
      if (handleOutput(res)) loop()
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
