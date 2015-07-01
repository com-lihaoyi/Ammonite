package ammonite.repl.interp

import java.io.File
import java.nio.file.Files
import acyclic.file
import annotation.tailrec
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
class Interpreter(shellPrompt0: Ref[String],
                  frontEnd0: Ref[FrontEnd],
                  pprintConfig: pprint.Config,
                  colors0: Ref[ColorSet],
                  stdout: String => Unit,
                  history0: => History,
                  predef: String){ interp =>

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()

  def processLine(stmts: Seq[String],
                  printer: Iterator[String] => Unit) = for{
    _ <- Catching { case Ex(x@_*) =>
      val Res.Failure(trace) = Res.Failure(x)
      Res.Failure(trace + "\nSomething unexpected went wrong =(")
    }
    Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
    out <- evaluateLine(code, printSnippet, printer)
  } yield out

  def evaluateLine(code: String, printSnippet: Seq[String], printer: Iterator[String] => Unit) = {
    val oldClassloader = Thread.currentThread().getContextClassLoader
    try{
      Thread.currentThread().setContextClassLoader(eval.evalClassloader)
      eval.processLine(
        code,
        s"ReplBridge.shell.Internal.combinePrints(${printSnippet.mkString(", ")})",
        printer
      )
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  }

  def processScript(code: String): Unit = {
    val blocks = Parsers.splitScript(code).map(preprocess(_, eval.getCurrentLine))
    val errors = blocks.collect{ case Res.Failure(err) => err }
    if(!errors.isEmpty) 
      stdout(Console.RED + errors.mkString("\n") + Console.RESET + "\n")
    else
      loop(blocks.collect{ case Res.Success(o) => o })

    @tailrec def loop(blocks: Seq[Preprocessor.Output]): Unit = {
      if(!blocks.isEmpty){
        val Preprocessor.Output(code, _) = blocks.head //pretty printing results is disabled for scripts
        val ev = evaluateLine(code, Seq(), _ => ())
        ev match {
          case Res.Failure(msg) =>
            stdout(Console.RED + msg + Console.RESET + "\n")
          case Res.Success(ev) =>
            eval.update(ev.imports)
            loop(blocks.tail)
          case _ => loop(blocks.tail)
        }
      }
    }
  }

  def handleOutput(res: Res[Evaluated]) = {
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
    def shellPrompt = shellPrompt0
    def frontEnd = frontEnd0

    object load extends Load{

      def apply(line: String) = {
        processScript(line)
      }

      def script(file: File): Unit = {
        val content = Files.readAllBytes(file.toPath)
        apply(new String(content))
      }

      def script(path: String): Unit = {
        script(new File(path))
      }

      def handleJar(jar: File): Unit = {
        extraJars = extraJars ++ Seq(jar)
        eval.addJar(jar.toURI.toURL)
      }
      def jar(jar: File): Unit = {
        handleJar(jar)
        init()
      }
      def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit ={
        val (groupId, artifactId, version) = coordinates
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
    def history = history0
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
      eval.evalClassloader
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
    processScript(predef)
  }
}
