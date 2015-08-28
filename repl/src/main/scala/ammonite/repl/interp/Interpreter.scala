package ammonite.repl.interp

import java.io.File
import java.nio.file.NotDirectoryException
import scala.collection.mutable
import acyclic.file
import ammonite.ops._
import pprint.{Config, PPrint}
import annotation.tailrec
import ammonite.repl._
import ammonite.repl.frontend._

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
                  history: => History,
                  predef: String,
                  replArgs: Seq[Bind[_]]){ interp =>


  val hardcodedPredef =
    """import ammonite.repl.frontend.ReplBridge.repl
      |import ammonite.repl.frontend.ReplBridge.repl.{
      |  notify => _,
      |  wait => _,
      |  equals => _,
      |  asInstanceOf => _,
      |  synchronized => _,
      |  notifyAll => _,
      |  isInstanceOf => _,
      |  == => _,
      |  Internal => _,
      |  != => _,
      |  getClass => _,
      |  ne => _,
      |  eq => _,
      |  ## => _,
      |  hashCode => _,
      |  _
      |}
      |import ammonite.repl.IvyConstructor.{ArtifactIdExt, GroupIdExt}
      |""".stripMargin

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()

  def processLine(code: String,
                  stmts: Seq[String],
                  printer: Iterator[String] => Unit) = {
    for{
      _ <- Catching { case ex =>
        Res.Exception(ex, "Something unexpected went wrong =(")
      }
      Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
      out <- evaluateLine(code, printSnippet, printer)
    } yield out
  }

  def evaluateLine(code: String,
                   printSnippet: Seq[String],
                   printer: Iterator[String] => Unit,
                   extraImports: Seq[ImportData] = Seq() ) = {
    val oldClassloader = Thread.currentThread().getContextClassLoader
    try{
      Thread.currentThread().setContextClassLoader(eval.evalClassloader)
      eval.processLine(
        code,
        s"""
        ammonite.repl
                .frontend
                .ReplBridge
                .repl
                .Internal
                .combinePrints(${printSnippet.mkString(", ")})
        """,
        printer,
        extraImports
      )
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  }

  def processModule(code: String) =
    processScript(hardcodedPredef + "\n@\n" + code, eval.processScriptBlock)

  def processExec(code: String) =
    processScript(hardcodedPredef + "\n@\n" + code, { (c, i) => evaluateLine(c, Nil, _ => (), i)})
 
  //this variable keeps track of where should we put the imports resulting from scripts.
  private var scriptImportCallback: Seq[ImportData] => Unit = eval.update

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
    // we store the old value, because we will reassign this in the loop
    val outerScriptImportCallback = scriptImportCallback
    try{
      if(!errors.isEmpty)
        stdout(colors0().error() + errors.mkString("\n") + colors0().reset() + "\n")
      else
        loop(blocks.collect{ case Res.Success(o) => o }, Seq())
    } finally {
      scriptImportCallback = outerScriptImportCallback
    }
    Timer("processScript 3")
    @tailrec def loop(blocks: Seq[Preprocessor.Output], imports: Seq[Seq[ImportData]]): Unit = {
      if(!blocks.isEmpty){
        Timer("processScript loop 0")
        // imports from scripts loaded from this script block will end up in this buffer
        val nestedScriptImports = mutable.Buffer.empty[ImportData]
        scriptImportCallback = { imports => nestedScriptImports ++= imports }
        // pretty printing results is disabled for scripts
        val Preprocessor.Output(code, _) = blocks.head
        Timer("processScript loop 1")
        val ev = evaluate(code, imports.flatten)
        Timer("processScript loop 2")
        ev match {
          case Res.Failure(msg) => throw new CompilationError(msg)
          case Res.Success(ev) => loop(blocks.tail, imports :+ (ev.imports ++ nestedScriptImports))
          case _ => loop(blocks.tail, imports)
        }
      } else {
        // if we have imports to pass to the upper layer we do that
        imports.lastOption.foreach(outerScriptImportCallback)
      }
    }
  }

  def handleOutput(res: Res[Evaluated]) = {
    res match{
      case Res.Skip => true
      case Res.Exit(value) =>
        pressy.shutdownPressy()
        false
      case Res.Success(ev) =>
        eval.update(ev.imports)
        true
      case Res.Failure(msg) => true
      case Res.Exception(ex, msg) => true
    }
  }

  abstract class DefaultLoadJar extends LoadJar{

    def handleJar(jar: File): Unit

    def jar(jar: Path): Unit = {
      handleJar(new java.io.File(jar.toString))
      init()
    }
    def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit = {
      val (groupId, artifactId, version) = coordinates
      storage().ivyCache().get((groupId, artifactId, version)) match{
        case Some(ps) => ps.map(new java.io.File(_)).map(handleJar)
        case None =>
          val resolved = IvyThing.resolveArtifact(
            groupId,
            artifactId,
            version,
            if (verbose) 2 else 1
          )

          storage().ivyCache() = storage().ivyCache().updated(
            (groupId, artifactId, version),
            resolved.map(_.getAbsolutePath).toSet
          )

          resolved.map(handleJar)
      }

      init()
    }
  }

  lazy val replApi: ReplAPI = new DefaultReplAPI {

    def imports = interp.eval.previousImportBlock
    val colors = colors0
    val prompt = prompt0
    val frontEnd = frontEnd0

    object load extends DefaultLoadJar with Load {

      def handleJar(jar: File) = {
        extraJars = extraJars ++ Seq(jar)
        eval.addJar(jar.toURI.toURL)
      }

      def apply(line: String) = processExec(line)

      def exec(file: Path): Unit = apply(read(file))

      def module(file: Path): Unit = {
        processModule(read(file))
        init()
      }

      object plugin extends DefaultLoadJar {
        def handleJar(jar: File) =
          eval.addPluginJar(jar.toURI.toURL)
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

    def search(target: scala.reflect.runtime.universe.Type) = {
      Interpreter.this.compiler.search(target)
    }
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
    val cd = (arg: Path) => {
      if (!stat(arg).isDir) throw new NotDirectoryException(arg.toString)
      else {
        wd0 = arg
        wd0
      }

    }
    implicit def Relativizer[T](p: T)(implicit b: Path, f: T => RelPath): Path = b/f(p)

    def width = interp.width

    def height = interp.height

    override def replArgs = Interpreter.this.replArgs.toVector
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
      eval.pluginClassloader,
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
  val argString =
    replArgs.zipWithIndex
            .map{ case (b, idx) =>
              s"""
              val ${b.name} =
                ammonite.repl
                        .frontend
                        .ReplBridge
                        .repl
                        .replArgs($idx)
                        .value
                        .asInstanceOf[${b.typeTag.tpe}]
              """
            }
            .mkString("\n")

  processModule(hardcodedPredef)
  init()

  processModule(storage().loadPredef + "\n" + predef + "\n" + argString)
  Timer("Interpreter init predef 0")
  init()
  Timer("Interpreter init predef 1")
}
