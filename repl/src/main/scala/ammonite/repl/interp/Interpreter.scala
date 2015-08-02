package ammonite.repl.interp

import java.io.File
import java.nio.file.{NotDirectoryException, Files}
import acyclic.file
import ammonite.ops._
import ammonite.repl.Util.IvyMap
import pprint.{Config, PPrint}
import annotation.tailrec
import ammonite.repl._
import ammonite.repl.frontend._
import Util.CompileCache
import fastparse.core.Result

import scala.reflect.io.VirtualDirectory

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(prompt0: Ref[String],
                  frontEnd0: Ref[FrontEnd],
                  width: => Int,
                  height: => Int,
                  pprintConfig: pprint.Config,
                  colors0: Ref[Colors],
                  stdout: String => Unit,
                  storage: Ref[Storage],
                  predef: String){ interp =>

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()

  var history = new History(Vector())
  def processLine(code: String,
                  stmts: Seq[String],
                  printer: Iterator[String] => Unit) = {
    if (code != "") {
      storage().fullHistory() = storage().fullHistory() :+ code
      history = history :+ code
    }
    for{
      _ <- Catching { case ex =>
        Res.Exception(ex, "Something unexpected went wrong =(")
      }
      Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
      out <- evaluateLine(code, printSnippet, printer)
    } yield out
  }

  def evaluateLine(code: String, printSnippet: Seq[String], printer: Iterator[String] => Unit) = {
    val oldClassloader = Thread.currentThread().getContextClassLoader
    try{
      Thread.currentThread().setContextClassLoader(eval.evalClassloader)
      eval.processLine(
        code,
        s"ammonite.repl.frontend.ReplBridge.repl.Internal.combinePrints(${printSnippet.mkString(", ")})",
        printer
      )
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  }

  def processModule(code: String) = processScript(code, eval.processScriptBlock)

  def processExec(code: String) = processScript(code, { (c, _) => evaluateLine(c, Seq(), _ => ()) })
 
  //common stuff in proccessModule and processExec
  def processScript(code: String, evaluate: (String, Seq[ImportData]) => Res[Evaluated]): Unit = {
    Timer("processScript 0")
    val blocks0 = Parsers.splitScript(code)
    Timer("processScript 0a")
    Parsers.splitScript(code)
    Timer("processScript 0b")

    val blocks = blocks0.map(preprocess(_, ""))
    Timer("processScript 1")
    val errors = blocks.collect{ case Res.Failure(err) => err }
    Timer("processScript 2")
    if(!errors.isEmpty)
      stdout(colors0().error() + errors.mkString("\n") + colors0().reset() + "\n")
    else
      loop(blocks.collect{ case Res.Success(o) => o }, Seq())
    Timer("processScript 3")
    @tailrec def loop(blocks: Seq[Preprocessor.Output], imports: Seq[ImportData]): Unit = {
      if(!blocks.isEmpty){
        Timer("processScript loop 0")
        val Preprocessor.Output(code, _) = blocks.head //pretty printing results is disabled for scripts
        Timer("processScript loop 1")
        val ev = evaluate(code, imports)
        Timer("processScript loop 2")
        ev match {
          case Res.Failure(msg) =>
            throw new CompilationError(msg)
          case Res.Success(ev) =>
            eval.update(ev.imports)
            loop(blocks.tail, imports ++ ev.imports)
          case _ => loop(blocks.tail, imports)
        }
      }
    }
  }

  def handleOutput(res: Res[Evaluated]) = {
    res match{
      case Res.Skip => true
      case Res.Exit =>
        pressy.shutdownPressy()
        false
      case Res.Success(ev) =>
        eval.update(ev.imports)
        true
      case Res.Failure(msg) => true
      case Res.Exception(ex, msg) => true
    }
  }

  lazy val replApi: ReplAPI = new DefaultReplAPI {

    def imports = interp.eval.previousImportBlock
    val colors = colors0
    val prompt = prompt0
    val frontEnd = frontEnd0

    object load extends Load{

      def apply(line: String) = processExec(line)

      def exec(file: Path): Unit = apply(read(file))

      def module(file: Path): Unit = {
        processModule(read(file))
        init()
      }

      def handleJar(jar: File): Unit = {
        extraJars = extraJars ++ Seq(jar)
        eval.addJar(jar.toURI.toURL)
      }
      def jar(jar: Path): Unit = {
        handleJar(new java.io.File(jar.toString))
        init()
      }
      def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit = {
        val (groupId, artifactId, version) = coordinates
        storage().ivyCache().get((groupId, artifactId, version)) match{
          case Some(ps) => ps.map(new java.io.File(_)).map(handleJar)
          case None =>
            val resolved = IvyThing.resolveArtifact(groupId, artifactId, version, if (verbose) 2 else 1)
            storage().ivyCache() = storage().ivyCache().updated(
              (groupId, artifactId, version),
              resolved.map(_.getAbsolutePath).toSet
            )

            resolved.map(handleJar)
        }

        init()
      }
    }

    implicit lazy val pprintConfig: Ref[pprint.Config] = {
      Ref.live[pprint.Config](
        () => interp.pprintConfig.copy(
          width = width,
          height = height / 2
        )
      )
    }

    def show[T: PPrint](implicit cfg: Config) = (t: T) => {
      pprint.tokenize(t, height = 0)(implicitly[PPrint[T]], cfg).foreach(print)
      println()
    }
    def show[T: PPrint](t: T,
                        width: Integer = null,
                        height: Integer = 0,
                        indent: Integer = null,
                        colors: pprint.Colors = null)
                       (implicit cfg: Config = Config.Defaults.PPrintConfig) = {


      pprint.tokenize(t, width, height, indent, colors)(implicitly[PPrint[T]], cfg).foreach(print)
      println()
    }

    def search(target: scala.reflect.runtime.universe.Type) = Interpreter.this.compiler.search(target)
    def compiler = Interpreter.this.compiler.compiler
    def newCompiler() = init()
    def fullHistory = storage().fullHistory()
    def history = Interpreter.this.history

    var wd0 = cwd
    /**
     * The current working directory of the shell, that will get picked up by
     * any ammonite.ops commands you use
     */
    implicit def wd = wd0
    /**
     * Change the working directory `wd`; if the provided path is relative it
     * gets appended on to the current `wd`, if it's absolute it replaces.
     */
    val cd = new ammonite.ops.Op1[ammonite.ops.Path, ammonite.ops.Path]{
      def apply(arg: Path) = {
        if (!stat(arg).isDir) throw new NotDirectoryException(arg.toString)
        else {
          wd0 = arg
          wd0
        }
      }
    }
    implicit def Relativizer[T](p: T)(implicit b: Path, f: T => RelPath): Path = b/f(p)

    def width = interp.width

    def height = interp.height
  }

  var compiler: Compiler = _
  var pressy: Pressy = _
  def init() = {
    Timer("Interpreter init init 0")
    compiler = Compiler(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath,
      eval.evalClassloader,
      () => pressy.shutdownPressy()
    )
    Timer("Interpreter init init compiler")
    pressy = Pressy(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath,
      eval.evalClassloader
    )
    Timer("Interpreter init init pressy")
  }


  val mainThread = Thread.currentThread()
  val preprocess = Preprocessor(compiler.parse)

  Timer("Interpreter init Preprocess")

  val eval = Evaluator(
    mainThread.getContextClassLoader,
    compiler.compile,
    0,
    storage().compileCacheLoad,
    storage().compileCacheSave,
    compiler.addToClasspath
  )

  eval.evalClassloader.findClassPublic("ammonite.repl.frontend.ReplBridge$")
  val bridgeCls = eval.evalClassloader.findClassPublic("ammonite.repl.frontend.ReplBridge")

  ReplAPI.initReplBridge(
    bridgeCls.asInstanceOf[Class[ReplAPIHolder]],
    replApi
  )
  Timer("Interpreterinit eval")
  init()
  Timer("Interpreter init init")
  val hardcodedPredef =
    """import ammonite.repl.frontend.ReplBridge.repl
      |import ammonite.repl.frontend.ReplBridge.repl._
      |import ammonite.repl.IvyConstructor._
      |""".stripMargin

  processModule(hardcodedPredef + predef)
  Timer("Interpreter init predef 0")
  init()
  Timer("Interpreter init predef 1")
}
