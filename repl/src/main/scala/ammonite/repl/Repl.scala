package ammonite.repl

import java.io.{OutputStream, InputStream}
import java.net.URLClassLoader
import ammonite.IvyThing
import ammonite.repl.eval._
import ammonite.repl.frontend.{DefaultReplAPI, ReplAPI}
import acyclic.file
import org.apache.ivy.Ivy

import scala.annotation.tailrec
import scala.reflect.io.VirtualDirectory


class Repl(input: InputStream, output: OutputStream) {

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()
  var extraJarClassloaders = Seq[ClassLoader]()

  val mainThread = Thread.currentThread()

  val eval: Evaluator = new ScalaEvaluator(
    mainThread.getContextClassLoader,
    extraJarClassloaders
  )

  def initCompiler() = {
    eval.setJars(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath
    )
  }

  initCompiler()

  val frontEnd = new frontend.JLineFrontend(
    input,
    output,
    replAPI.shellPrompt,
    eval.previousImportBlock,
    (i, c) => eval.complete(i, c)
  )

  def loadJar(jar: java.io.File): Unit = {
    extraJars = extraJars ++ Seq(jar)
    extraJarClassloaders ++= Seq(new URLClassLoader(
      Array(jar.toURI.toURL),
      getClass.getClassLoader
    ))
    initCompiler()
    eval.initReplBridge(replAPI)
  }
  lazy val replAPI: ReplAPI = new DefaultReplAPI(
    frontEnd.history,
    loadJar(_),
    (groupId, artifactId, version) => {
      loadJar(IvyThing.resolveArtifact(groupId, artifactId, version))
    },
    () => {
      initCompiler()
      eval.initReplBridge(replAPI)
    }
  )

  eval.initReplBridge(replAPI)


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
    _ <- Signaller("INT") { mainThread.stop() }
    out <- eval.processLine(res)
  } yield out

  def run() = {
    @tailrec def loop(): Unit = {
      val res = action()
      frontEnd.update(res)
      eval.update(res)
      res match{
        case Result.Skip => loop()
        case Result.Buffer(line) => loop()
        case Result.Exit =>
          eval.askShutdown()
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
