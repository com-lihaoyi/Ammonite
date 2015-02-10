package ammonite.repl

import java.io.{OutputStream, InputStream}
import java.net.URLClassLoader
import ammonite.IvyThing
import ammonite.repl.eval.{Classpath, Evaluator, Preprocessor, Compiler}
import ammonite.repl.frontend.{ColorSet, DefaultReplAPI, ReplAPIHolder, ReplAPI}
import acyclic.file
import org.apache.ivy.Ivy

import scala.annotation.tailrec
import scala.reflect.io.VirtualDirectory

class Repl(input: InputStream, output: OutputStream) {
  val interp: Interpreter = new Interpreter(() => initReplBridge())
  def initReplBridge() = {
    interp.compiler.importsFor("", interp.eval.replBridgeCode)
    val cls = interp.eval.evalClass(interp.eval.replBridgeCode, "ReplBridge")
    ReplAPI.initReplBridge(
      cls.asInstanceOf[Result.Success[Class[ReplAPIHolder]]].s,
      replAPI
    )
  }

  lazy val replAPI: ReplAPI = new DefaultReplAPI(
    frontEnd.history,
    interp.loadJar,
    (groupId, artifactId, version) => {
      interp.loadJar(IvyThing.resolveArtifact(groupId, artifactId, version))
    },
    () => {
      interp.initCompiler()
      initReplBridge()
    },
    ColorSet.Default,
    ammonite.pprint.Config.Colors.PPrintConfig
  )

  initReplBridge()

  val frontEnd = new frontend.JLineFrontend(
    input,
    output,
    replAPI.shellPrompt,
    interp.eval.previousImportBlock,
    (i, c) => interp.compiler.complete(i, c)
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
          interp.compiler.pressy.askShutdown()
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
