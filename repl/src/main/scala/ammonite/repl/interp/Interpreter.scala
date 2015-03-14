package ammonite.repl.interp

import java.io.File
import acyclic.file
import ammonite.pprint
import ammonite.repl._
import ammonite.repl.frontend._

import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Global

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter[A,B](handleResult: => (String, Res[Evaluated[_]]) => Unit,
                       stdout: String => Unit,
                       initialHistory: Seq[String],
                       initialImports: Seq[(String, ImportData)],
                       preprocessor: (Unit => (String => Either[String, scala.Seq[Global#Tree]])) => (String, Int) => Res[A],
                       wrap: (A, String, String) => String,
                       bridgeInit: String,
                       bridgeInitName: String,
                       bridgeInitClass: (Interpreter[A,B], Class[_]) => Unit,
                       jarDeps: Seq[File],
                       dirDeps: Seq[File]){ interp =>

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()

  val history = initialHistory.to[collection.mutable.Buffer]
  var buffered = ""

  def processLine[C](line: String,
                     saveHistory: (String => Unit, String) => Unit,
                     printer: B => C,
                     useClassWrapper: Boolean = false) = for{
    _ <- Catching { case Ex(x@_*) =>
      val Res.Failure(trace) = Res.Failure(x)
      Res.Failure(trace + "\nSomething unexpected went wrong =(")
    }
    p <- preprocess(line, eval.getCurrentLine)
    _ = saveHistory(history.append(_), line)
    oldClassloader = Thread.currentThread().getContextClassLoader
    out <- try{
      Thread.currentThread().setContextClassLoader(eval.evalClassloader)
      eval.processLine(p, printer, useClassWrapper)
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  } yield out

  def handleOutput(res: Res[Evaluated[_]]) = {
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

  var compiler: Compiler = _
  var pressy: Pressy = _
  def init() = {
    compiler = Compiler(
      jarDeps ++ extraJars,
      dirDeps,
      dynamicClasspath,
      () => pressy.shutdownPressy()
    )
    pressy = Pressy(
      jarDeps ++ extraJars,
      dirDeps,
      dynamicClasspath
    )

    val cls = eval.evalClass(bridgeInit, bridgeInitName)
    bridgeInitClass(interp, cls.map(_._1).asInstanceOf[Res.Success[Class[_]]].s)
  }

  val mainThread = Thread.currentThread()
  val preprocess = preprocessor(_ => compiler.parse)

  val eval = Evaluator[A, B](
    mainThread.getContextClassLoader,
    initialImports,
    preprocess.apply,
    wrap,
    compiler.compile,
    stdout
  )

  init()
}

object Interpreter {
  type Console = Interpreter[Preprocessor.Output, Iterator[String]]

  def consoleInitialImports =
    Evaluator.namesFor[ReplAPI].map(n => n -> ImportData(n, n, "", "ReplBridge.shell")).toSeq ++
      Evaluator.namesFor[ammonite.repl.IvyConstructor].map(n => n -> ImportData(n, n, "", "ammonite.repl.IvyConstructor")).toSeq

  def console(handleResult: => (String, Res[Evaluated[_]]) => Unit,
              shellPrompt0: => Ref[String],
              pprintConfig0: pprint.Config = pprint.Config.Defaults.PPrintConfig,
              colors0: ColorSet = ColorSet.BlackWhite,
              stdout: String => Unit,
              initialHistory: Seq[String]): Console = {
    var replApi: ReplAPI = null

    def initReplApi(intp: Console) = {
      replApi = new DefaultReplAPI {
        def imports = intp.eval.previousImportBlock
        def colors = colors0
        def shellPrompt: String = shellPrompt0()
        def shellPrompt_=(s: String) = shellPrompt0() = s
        object load extends Load{

          def apply(line: String) = intp.handleOutput(intp.processLine(
            line,
            (_, _) => (), // Discard history of load-ed lines,
            _.foreach(print)
          ))

          def handleJar(jar: File): Unit = {
            intp.extraJars = intp.extraJars ++ Seq(jar)
            intp.eval.addJar(jar.toURI.toURL)
          }
          def jar(jar: File): Unit = {
            intp.eval.newClassloader()
            handleJar(jar)
            intp.init()
          }
          def ivy(coordinates: (String, String, String)): Unit ={
            val (groupId, artifactId, version) = coordinates
            intp.eval.newClassloader()
            IvyThing.resolveArtifact(groupId, artifactId, version)
              .map(handleJar)
            intp.init()
          }
        }
        implicit def pprintConfig = pprintConfig0
        def clear() = ()
        def newCompiler() = intp.init()
        def history = intp.history.toVector.dropRight(1)
      }
    }

    new Interpreter[Preprocessor.Output, Iterator[String]](
      handleResult, stdout, initialHistory,
      consoleInitialImports,
      f => Preprocessor(f()).apply,
      {
        (p: Preprocessor.Output, previousImportBlock: String, wrapperName: String) =>
          s"""$previousImportBlock

              object $wrapperName{
                ${p.code}
                def $$main() = {${p.printer.reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()")}}
              }
           """
      },
      "object ReplBridge extends ammonite.repl.frontend.ReplAPIHolder{}",
      "ReplBridge",
      {
        (intp, cls) =>
          if (replApi == null) initReplApi(intp)

          ReplAPI.initReplBridge(
            cls.asInstanceOf[Class[ReplAPIHolder]],
            replApi
          )
      },
      Classpath.jarDeps,
      Classpath.dirDeps
    )
  }
}
