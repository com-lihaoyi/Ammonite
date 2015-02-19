package ammonite.repl.interp

import java.io.File
import java.net.URLClassLoader

import ammonite.repl.{Ref, Evaluated, IvyThing, Result}
import ammonite.repl.frontend._

import scala.reflect.io.VirtualDirectory

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(handleResult: Result[Evaluated] => Unit,
                  shellPrompt0: Ref[String],
                  historyFunc: () => Seq[String],
                  colors0: ColorSet = ColorSet.BlackWhite,
                  stdout: String => Unit = println){

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()
  var extraJarClassloaders = Seq[ClassLoader]()

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
      def apply(line: String) = handleOutput(eval.processLine(line))

      def jar(jar: File): Unit = {
        extraJars = extraJars ++ Seq(jar)
        extraJarClassloaders ++= Seq(new URLClassLoader(
          Array(jar.toURI.toURL),
          getClass.getClassLoader
        ))
        init()
      }
      def ivy(groupId: String, artifactId: String, version: String): Unit =
        jar(IvyThing.resolveArtifact(groupId, artifactId, version))
    }
    implicit def pprintConfig = ammonite.pprint.Config.Defaults.PPrintConfig
    def clear() = ()
    def newCompiler() = init()
    def history = historyFunc()
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
    extraJarClassloaders,
    preprocess.apply,
    compiler.compile,
    stdout
  )
  init()
}
