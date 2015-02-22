package ammonite.repl.interp

import java.io.File
import java.lang.reflect.InvocationTargetException
import java.net.URLClassLoader

import ammonite.pprint
import ammonite.repl._
import ammonite.repl.frontend._

import scala.reflect.io.VirtualDirectory

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(handleResult: Result[Evaluated] => Unit,
                  shellPrompt0: Ref[String],
                  pprintConfig: pprint.Config = pprint.Config.Defaults.PPrintConfig,
                  colors0: ColorSet = ColorSet.BlackWhite,
                  stdout: String => Unit = println){ interp =>

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()

  val history = collection.mutable.Buffer.empty[String]


  def processLine(line: String, saveHistory: String => Unit, printer: Iterator[String] => Unit) = for{
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
    Preprocessor.Output(code, printSnippet) <- preprocess(line, eval.getCurrentLine)
    _ = history.append(line)
    _ = saveHistory(line)
    out <- eval.processLine(code, printSnippet, printer)
  } yield out

  def handleOutput(res: Result[Evaluated]) = {
    handleResult(res)

    res match{
      case Result.Skip => true
      case Result.Buffer(line) => true
      case Result.Exit =>
        stdout("Bye!")
        pressy.shutdownPressy()
        false
      case Result.Success(ev) =>
        eval.update(ev.imports)
        true
      case Result.Failure(msg) =>
        stdout(Console.RED + msg + Console.RESET)
        true
    }
  }

  lazy val replApi: ReplAPI = new DefaultReplAPI {
    def colors = colors0
    def shellPrompt: String = shellPrompt0()
    def shellPrompt_=(s: String) = shellPrompt0() = s
    object load extends Load{

      def apply(line: String) = handleOutput(processLine(
        line,
        _ => (), // Discard history of load-ed lines,
        _.foreach(print)
      ))

      def handleJar(jar: File): Unit = {
        extraJars = extraJars ++ Seq(jar)
        eval.addJar(jar.toURI.toURL)
      }
      def jar(jar: File): Unit = {
        eval.newClassloader()
        handleJar(jar)
        init()
      }
      def ivy(groupId: String, artifactId: String, version: String): Unit ={
        eval.newClassloader()
        IvyThing.resolveArtifact(groupId, artifactId, version)
          .map(handleJar)
        init()
      }
    }
    implicit def pprintConfig = interp.pprintConfig
    def clear() = ()
    def newCompiler() = init()
    def history = interp.history.toVector.dropRight(1)
  }

  var compiler: Compiler = _
  var pressy: Pressy = _
  def init() = {
    compiler = Compiler(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath,
      () => pressy.shutdownPressy()
    )
    pressy = Pressy(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath
    )
    val cls = eval.evalClass(
      "object ReplBridge extends ammonite.repl.frontend.ReplAPIHolder{}",
      "ReplBridge"
    )
    ReplAPI.initReplBridge(
      cls.map(_._1).asInstanceOf[Result.Success[Class[ReplAPIHolder]]].s,
      replApi
    )
  }


  val mainThread = Thread.currentThread()
  val preprocess = Preprocessor(compiler.parse)

  val eval = Evaluator(
    mainThread.getContextClassLoader,
    preprocess.apply,
    compiler.compile,
    stdout
  )
  init()
}
