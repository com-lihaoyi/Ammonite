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
import scalaz.{\/, \/-, -\/}
import scalaz.concurrent.Task

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

      import coursier._
      var dependencies = Set.empty[Dependency]
      val cache = new File(sys.props("user.home") + "/.coursier/cache")
      val centralMetadataCache = new File(cache, "metadata/central")
      val centralArtifactCache = new File(cache, "files/central")
      var repositories = Seq(
        Repository.ivy2Local,
        Repository.mavenCentral
          .copy(fetch = Repository.mavenCentral.fetch.copy(cache = Some(centralMetadataCache)))
      )
      var cachePolicy: Repository.CachePolicy =
        Repository.CachePolicy.Default
      def ivy(coordinates: (String, String, String), verbose: Boolean, force: Boolean): Unit = {
        val (org, name, version) = coordinates
        val dependencies0 = dependencies + Dependency(Module(org, name), version)

        val resolution: Resolution = Resolution(dependencies0)
          .process
          .run(repositories, maxIterations = 100)
          .run

        def checkIsDone(resolution: Resolution) = {
          val done = resolution.isDone
          if (!done)
            println(s"Resolution did not converge =|")

          if (done || force)
            Some(resolution)
          else
            None
        }

        def checkMetadataErrors(resolution: Resolution) = {
          // Print resolution errors
          val sortedErrors = resolution
            .errors
            .sortBy(t => (t._1.module.organization, t._1.module.name))
          if (sortedErrors.nonEmpty) {
            println(s"${sortedErrors.size} error(s) found")
            for ((dep, perRepoErrors) <- sortedErrors) {
              println(s"  ${dep.module.organization} % ${dep.module.name} % ${dep.version}:")
              for (error <- perRepoErrors)
                println(s"    $error")
            }
            println()
          }

          if (sortedErrors.isEmpty || force)
            Some(resolution)
          else
            None
        }

        def results(resolution: Resolution) = {
          val artifacts = resolution
            .artifacts
            .sortBy(_.url)

          def logger: FilesLogger = new FilesLogger {
            def foundLocally(f: File) =
              println(s"Found $f")
            def downloadingArtifact(url: String) =
              println(s"Downloading $url")
            def downloadedArtifact(url: String, success: Boolean) =
              println(s"$url: ${if (success) "done" else "error"}")
          }

          val files0 = coursier.Files(
            Seq(
              Repository.mavenCentral.fetch.root -> centralArtifactCache
            ),
            () => ???,
            logger = if (verbose) Some(logger) else None
          )

          val tasks = artifacts
            .map(a => files0.file(a, cachePolicy).run.map(a -> _))

          Task.gatherUnordered(tasks)
            .run
        }

        def checkDownloadResults(results: List[(Artifact, String \/ File)]) = {
          val downloadErrors = results
            .collect{case (a, -\/(err)) => a -> err}
            .sortBy(_._1.url)

          if (downloadErrors.nonEmpty) {
            println(s"${downloadErrors.size} download errors")
            for ((a, err) <- downloadErrors)
              println(s"  ${a.url}: $err")
            println()
          }

          def files = results.collect{case (_, \/-(f)) => f}

          if (downloadErrors.isEmpty || force)
            Some(files)
          else
            None
        }

        val filesOption =
          Some(resolution)
            .flatMap(checkIsDone)
            .flatMap(checkMetadataErrors)
            .map(results)
            .flatMap(checkDownloadResults)

        for (files <- filesOption) {
          dependencies = dependencies0
          if (files.nonEmpty) {
            files.foreach(handleJar)
            init()
          }
        }
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
