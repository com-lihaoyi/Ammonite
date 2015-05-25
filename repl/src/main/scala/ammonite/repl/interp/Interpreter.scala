package ammonite.repl.interp

import java.io.File
import acyclic.file
import ammonite.pprint
import ammonite.repl._
import ammonite.repl.frontend._
import fastparse.core.Result

import scala.reflect.io.VirtualDirectory

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(handleResult: => Res[Evaluated] => Unit,
                  shellPrompt0: => Ref[String],
                  pprintConfig: pprint.Config,
                  colors0: ColorSet = ColorSet.BlackWhite,
                  stdout: String => Unit,
                  initialHistory: Seq[String],
                  predef: String){ interp =>

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()

  val history = initialHistory.to[collection.mutable.Buffer]

  def processLine(stmts: Seq[String],
                  saveHistory: (String => Unit, String) => Unit,
                  printer: Iterator[String] => Unit) = for{
    _ <- Catching { case Ex(x@_*) =>
      val Res.Failure(trace) = Res.Failure(x)
      Res.Failure(trace + "\nSomething unexpected went wrong =(")
    }
    Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
    _ = saveHistory(history.append(_), stmts.mkString)
    oldClassloader = Thread.currentThread().getContextClassLoader
    out <- try{
      Thread.currentThread().setContextClassLoader(eval.evalClassloader)
      eval.processLine(
        code,
        printSnippet.reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()"),
        printer
      )
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  } yield out

  def handleOutput(res: Res[Evaluated]) = {
    handleResult(res)

    res match{
      case Res.Skip =>
        true
      case Res.Exit =>
        stdout("Bye!\n")
        pressy.shutdownPressy()
        false
      case Res.Success(ev) =>
        eval.update(ev.imports)
        true
      case Res.Failure(msg) =>
        stdout(Console.RED + msg + Console.RESET + "\n")
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
        Parsers.split(line),
        (_, _) => (), // Discard history of load-ed lines,
        _.foreach(stdout)
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
      def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit ={
        val (groupId, artifactId, version) = coordinates
        eval.newClassloader()
        IvyThing.resolveArtifact(groupId, artifactId, version, if (verbose) 2 else 1)
                .map(handleJar)
        init()
      }
    }
    implicit var pprintConfig = interp.pprintConfig
    def clear() = ()
    def search(target: scala.reflect.runtime.universe.Type) = Interpreter.this.compiler.search(target)
    def compiler = Interpreter.this.compiler.compiler
    def newCompiler() = init()
    def history = interp.history.toVector.dropRight(1)
    def show[T](a: T, lines: Int = 0) = ammonite.pprint.Show(a, lines)
  }

  var compiler: Compiler = _
  var pressy: Pressy = _
  def init() = {
    compiler = Compiler(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath,
      eval.evalClassloader,
      () => pressy.shutdownPressy()
    )
    pressy = Pressy(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath,
      eval.evalClassloader,
      eval
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
    compiler.compile,
    if (predef != "") -1 else 0
  )

  init()
  // Run the predef. For now we assume that the whole thing is a single
  // command, and will get compiled & run at once. We hard-code the
  // line number to -1 if the predef exists so the first user-entered
  // line becomes 0
  if (predef != "") {
    val res1 = processLine(Parsers.split(predef), (_, _) => (), _.foreach(stdout))
    val res2 = handleOutput(res1)
    stdout("\n")
  }
}
