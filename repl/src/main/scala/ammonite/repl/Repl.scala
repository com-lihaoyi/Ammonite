package ammonite.repl

import java.io.{OutputStream, InputStream}
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter

import scala.annotation.tailrec

class Repl(input: InputStream, output: OutputStream) {
  val interp: Interpreter = new Interpreter(replApi)

  lazy val replApi: ReplAPI = new DefaultReplAPI(
    frontEnd.history,
    interp.loadJar,
    (groupId, artifactId, version) => {
      interp.loadJar(IvyThing.resolveArtifact(groupId, artifactId, version))
    },
    () => interp.init(),
    ColorSet.Default,
    ammonite.pprint.Config.Colors.PPrintConfig
  )

  val frontEnd = new frontend.JLineFrontend(
    input,
    output,
    replApi.shellPrompt,
    interp.eval.previousImportBlock,
    interp.compiler.complete
  )

  def action() = for{
    _ <- Catching { case x: Throwable =>
      var current = x
      var output = ""
      while(current != null) {
        output += current + "\n" + current.getStackTrace.map("  " + _).mkString("\n") + "\n"
        current = current.getCause
      }
      output + "\nSomething unexpected went wrong =("
    }
    res <- frontEnd.action()
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.eval.processLine(res)
  } yield out

  def run() = {
    @tailrec def loop(): Unit = {
      val res = action()
      frontEnd.update(res)
      interp.eval.update(res)
      res match{
        case Result.Skip => loop()
        case Result.Buffer(line) => loop()
        case Result.Exit =>
          println("Bye!")
        case Result.Success(ev) =>
          println(ev.msg)
          loop()
        case Result.Failure(msg) =>
          println(Console.RED + msg + Console.RESET)
          loop()
      }
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
