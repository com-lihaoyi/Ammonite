package ammonite.repl

import java.io.{OutputStream, InputStream}
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter

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
  def handleOutput(res: Result[Evaluated]) = {
    frontEnd.update(res)

    res match{
      case Result.Skip => true
      case Result.Buffer(line) => true
      case Result.Exit =>
        println("Bye!")
        false
      case Result.Success(ev) =>
        interp.eval.update(ev.imports)
        println(ev.msg)
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
      output + "\nSomething unexpected went wrong =("
    }
    res <- frontEnd.action()
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.eval.processLine(res)
  } yield out

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
