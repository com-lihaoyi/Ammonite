package ammonite.repl.interp

import java.io.File
import acyclic.file
import ammonite.pprint
import ammonite.repl._
import ammonite.repl.frontend._

import scala.reflect.io.VirtualDirectory

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(handleResult: => (String, Res[Evaluated]) => Unit,
                  shellPrompt0: => Ref[String],
                  pprintConfig: pprint.Config = pprint.Config.Defaults.PPrintConfig,
                  colors0: ColorSet = ColorSet.BlackWhite,
                  stdout: String => Unit,
                  initialHistory: Seq[String]){ interp =>

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()

  val history = initialHistory.to[collection.mutable.Buffer]
  var buffered = ""

  def processLine(line: String,
                  saveHistory: (String => Unit, String) => Unit,
                  printer: Iterator[String] => Unit) = for{
    _ <- Catching { case Ex(x@_*) =>
      val Res.Failure(trace) = Res.Failure(x)
      Res.Failure(trace + "\nSomething unexpected went wrong =(")
    }
    Preprocessor.Output(code, printSnippet) <- preprocess(line, eval.getCurrentLine)
    _ = saveHistory(history.append(_), line)
    oldClassloader = Thread.currentThread().getContextClassLoader
    out <- try{
      Thread.currentThread().setContextClassLoader(eval.evalClassloader)
      eval.processLine(code, printSnippet.reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()"), printer)
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  } yield out

  def handleOutput(res: Res[Evaluated]) = {
    handleResult(buffered, res)

    res match{
      case Res.Skip => true
      case Res.Buffer(line) =>
        /**
         * Hack to work around the fact that if nothing got entered into
         * the prompt, the `ConsoleReader`'s history wouldn't increase
         */
        buffered = line + "\n"
        true
      case Res.Exit =>
        stdout("Bye!")
        pressy.shutdownPressy()
        false
      case Res.Success(ev) =>
        buffered = ""
        eval.update(ev.imports)
        true
      case Res.Failure(msg) =>
        buffered = ""
        stdout(Console.RED + msg + Console.RESET)
        true
    }
  }

  lazy val replApi: ReplAPI = new DefaultReplAPI {
    def imports = interp.eval.previousImportBlock
    def colors = colors0
    def shellPrompt: String = shellPrompt0()
    def shellPrompt_=(s: String) = shellPrompt0() = s
    object load extends Load{

      def apply(line: String) = handleOutput(processLine(
        line,
        (_, _) => (), // Discard history of load-ed lines,
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
      def ivy(coordinates: (String, String, String)): Unit ={
        val (groupId, artifactId, version) = coordinates
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
      cls.map(_._1).asInstanceOf[Res.Success[Class[ReplAPIHolder]]].s,
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
