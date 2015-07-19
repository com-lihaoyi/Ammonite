package ammonite.repl.interp

import java.io.File
import java.nio.file.Files
import acyclic.file
import ammonite.ops.BasePath
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

  def processLine(code: String,
                  stmts: Seq[String],
                  printer: Iterator[String] => Unit) = {
    if (code != "") storage().history() = storage().history() :+ code
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
        s"ReplBridge.repl.Internal.combinePrints(${printSnippet.mkString(", ")})",
        printer
      )
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  }

  def processScript(code: String): Unit = {
    val blocks = Parsers.splitScript(code).map(preprocess(_, eval.getCurrentLine))
    val errors = blocks.collect{ case Res.Failure(err) => err }
    if(!errors.isEmpty) 
      stdout(colors0().error() + errors.mkString("\n") + colors0().reset() + "\n")
    else
      loop(blocks.collect{ case Res.Success(o) => o })

    @tailrec def loop(blocks: Seq[Preprocessor.Output]): Unit = {
      if(!blocks.isEmpty){
        val Preprocessor.Output(code, _) = blocks.head //pretty printing results is disabled for scripts
        val ev = evaluateLine(code, Seq(), _ => ())
        ev match {
          case Res.Failure(msg) =>
            stdout(colors0().error() + msg + colors0().reset() + "\n")
          case Res.Success(ev) =>
            eval.update(ev.imports)
            loop(blocks.tail)
          case _ => loop(blocks.tail)
        }
      }
    }
  }
  
  def processModule(code: String): Unit = {
    val blocks = Parsers.splitScript(code).map(preprocess(_, ""))
    val errors = blocks.collect{ case Res.Failure(err) => err }
    if(!errors.isEmpty) 
      stdout(Console.RED + errors.mkString("\n") + Console.RESET + "\n")
    else
      loop(blocks.collect{ case Res.Success(o) => o }, Seq())

    @tailrec def loop(blocks: Seq[Preprocessor.Output], imports: Seq[ImportData]): Unit = {
      if(!blocks.isEmpty){
        val Preprocessor.Output(code, _) = blocks.head //pretty printing results is disabled for scripts
        val ev = eval.processScriptBlock(code, imports)
        ev match {
          case Res.Failure(msg) =>
            stdout(Console.RED + msg + Console.RESET + "\n")
          case Res.Success(ev) =>{
            eval.update(ev.imports)
            loop(blocks.tail, imports ++ ev.imports)
          }
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
  val wdRef: Ref[ammonite.ops.Path] = Ref(ammonite.ops.cwd)
  lazy val replApi: ReplAPI = new DefaultReplAPI {

    def imports = interp.eval.previousImportBlock
    val colors = colors0
    val prompt = prompt0
    val frontEnd = frontEnd0

    object load extends Load{

      def apply(line: String) = {
        processScript(line)
      }

      def exec(file: File): Unit = {
        val content = Files.readAllBytes(file.toPath)
        apply(new String(content))
      }

      def exec(path: String): Unit = {
        exec(new File(path))
      }

      def module(file: File): Unit = {
        val content = Files.readAllBytes(file.toPath)
        processModule(new String(content))
        init()
      }

      def module(path: String): Unit = {
        module(new File(path))
      }

      def handleJar(jar: File): Unit = {
        extraJars = extraJars ++ Seq(jar)
        eval.addJar(jar.toURI.toURL)
      }
      def jar(jar: File): Unit = {
        handleJar(jar)
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
          height = height
        )
      )
    }

    def search(target: scala.reflect.runtime.universe.Type) = Interpreter.this.compiler.search(target)
    def compiler = Interpreter.this.compiler.compiler
    def newCompiler() = init()
    def history = storage().history()
    implicit def wd = wdRef()
    val cd = new ammonite.ops.Op1[ammonite.ops.BasePath[_], ammonite.ops.Path]{
      def apply(arg: BasePath[_]) = {
        wdRef() = arg match{
          case p: ammonite.ops.Path => p
          case p: ammonite.ops.RelPath => wd / p
        }
        wdRef()
      }
    }
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
    if (predef != "") -1 else 0,
    storage().compileCacheLoad,
    storage().compileCacheSave,
    compiler.addToClasspath
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
