package ammonite.repl.interp

import java.io.File
import java.nio.file.Files
import acyclic.file
import ammonite.repl.Util.IvyMap
import pprint.{Config, PPrint}
import annotation.tailrec
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
        s"ReplBridge.shell.Internal.combinePrints(${printSnippet.mkString(", ")})",
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

    implicit def pprintConfig: Ref[pprint.Config] = Ref.live[pprint.Config](
      () => interp.pprintConfig.copy(
        width = frontEnd().width,
        height = frontEnd().height / 2
      )
    )

    def clear() = ()
    def search(target: scala.reflect.runtime.universe.Type) = Interpreter.this.compiler.search(target)
    def compiler = Interpreter.this.compiler.compiler
    def newCompiler() = init()
    def history = storage().history()

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
