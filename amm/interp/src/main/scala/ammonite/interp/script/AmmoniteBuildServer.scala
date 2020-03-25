package ammonite.interp.script

import java.io.{InputStream, OutputStream, PrintStream, PrintWriter}
import java.net.URI
import java.nio.file.{FileSystemNotFoundException, Path, Paths}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CompletableFuture, Executors, ThreadFactory}
import java.util.UUID

import ammonite.interp.api.InterpAPI
import ammonite.interp.{CodeWrapper, DependencyLoader}
import ammonite.runtime.{Classpath, ImportHook, Storage}
import ammonite.util.{Imports, Printer}
import ch.epfl.scala.bsp4j.{Position => BPosition, _}
import coursierapi.{Dependency, Repository}
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

class AmmoniteBuildServer(
  initialScripts: Seq[os.Path] = Nil,
  initialImports: Imports = Imports(),
  defaultRepositories: Seq[Repository] = Repository.defaults().asScala.toList,
  codeWrapper: CodeWrapper = CodeWrapper,
  importHooks: Map[Seq[String], ImportHook] = ImportHook.defaults
) extends BuildServer with ScalaBuildServer with DummyBuildServerImplems {

  import AmmoniteBuildServer._

  private val storage = Storage.InMemory()
  private val printer = {
    val printStream = new PrintStream(System.out)
    Printer(
      printStream, new PrintStream(System.err), printStream,
      println, println, println
    )
  }

  private val dependencyLoader = new DependencyLoader(
    printer,
    storage,
    alreadyLoadedDependencies(),
    verboseOutput = false
  )


  private val initialClassLoader =
    if (isolatedApi)
      classOf[InterpAPI].getClassLoader
    else
      Thread.currentThread().getContextClassLoader
  private def initialClassPath = Classpath.classpath(initialClassLoader, storage)
    .map(_.toURI)

  private lazy val proc =
    withRoot { root =>
      ScriptProcessor(
        dependencyLoader,
        defaultRepositories,
        Nil,
        root,
        codeWrapper,
        importHooks
      )
  }
  private lazy val compiler =
    withRoot { root =>
      ScriptCompiler(
        storage,
        printer,
        codeWrapper,
        initialClassLoader,
        initialImports,
        classPathWhitelist(),
        Some(root),
        Some(
          root /
            ".ammonite" /
            s"scala-${scala.util.Properties.versionNumberString}" /
            s"amm-${ammonite.Constants.version}"
        )
      )
    }

  private var rootUriOpt = Option.empty[String]
  private var rootOpt = Option.empty[os.Path]

  private def withRoot[T](f: os.Path => T): T =
    rootOpt match {
      case None => throw new Exception("Uninitialized AmmoniteBuildServer")
      case Some(root) => f(root)
    }

  private var supportsScala = false

  private var clientOpt = Option.empty[BuildClient]

  private val defaultEc = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(1, threadFactory("ammonite-bsp"))
  )
  private val resolutionEc = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(1, threadFactory("ammonite-bsp-resolution"))
  )
  private val compileEc = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(1, threadFactory("ammonite-bsp-compile"))
  )

  override def onConnectWithClient(client: BuildClient): Unit = {
    clientOpt = Some(client)
  }


  def buildInitialize(params: InitializeBuildParams): CompletableFuture[InitializeBuildResult] =
    nonBlocking {

      if (rootUriOpt.nonEmpty || rootOpt.nonEmpty)
        () // TODO Warn…

      rootUriOpt = Some(params.getRootUri)
      rootOpt = rootUriOpt.flatMap { uri =>
        val uri0 = new URI(uri)
        try Some(os.Path(Paths.get(uri0)))
        catch {
          case _: IllegalArgumentException | _: FileSystemNotFoundException =>
            None
        }
      }

      // TODO Validate BSP version

      supportsScala = params.getCapabilities.getLanguageIds.asScala.exists(_ == "scala")

      val capabilities = new BuildServerCapabilities()
      capabilities.setCompileProvider(new CompileProvider(List("scala").asJava))
      capabilities.setDependencySourcesProvider(true)
      capabilities.setInverseSourcesProvider(true)
      capabilities.setResourcesProvider(true)
      capabilities.setBuildTargetChangedProvider(true)
      // capabilities.setRunProvider(new RunProvider(List("scala").asJava)) // TODO

      new InitializeBuildResult(
        "Ammonite",
        ammonite.Constants.version,
        ammonite.Constants.bspVersion,
        capabilities
      )
    }


  private lazy val moduleCache = new ScriptCache(proc)

  def workspaceBuildTargets(): CompletableFuture[WorkspaceBuildTargetsResult] =
    on(defaultEc) {
      moduleCache.load(initialScripts)

      val buildTargets = moduleCache.list.flatMap { mod =>
        // Write generated scala code before we compile things,
        // so that clients can index things straightaway
        try compiler.preCompile(mod)
        catch {
          case NonFatal(e) =>
            System.err.println(s"Caught $e")
            // FIXME Log this
        }

        mod.codeSource.path.toSeq.map { path =>
          val scalaTarget = new ScalaBuildTarget(
            "org.scala-lang",
            scala.util.Properties.versionNumberString,
            scala.util.Properties.versionNumberString.split('.').take(2).mkString("."),
            ScalaPlatform.JVM,
            // TODO This seems not to matter as long as the scala-compiler JARs and all
            // are in the classpath we pass via ScalacOptions.
            List.empty[String].asJava
          )
          val directDeps = mod.dependencies.scriptDependencies.flatMap(_.codeSource.path.toSeq)
          val target = new BuildTarget(
            buildTargetIdentifier(path),
            List.empty[String].asJava,
            List("scala").asJava,
            directDeps.map(buildTargetIdentifier).asJava,
            new BuildTargetCapabilities(true, false, false)
          )
          target.setBaseDirectory(path.toNIO.toAbsolutePath.getParent.toUri.toASCIIString)
          target.setDisplayName(path.last)
          target.setDataKind("scala")
          target.setData(scalaTarget)
          target
        }
      }

      new WorkspaceBuildTargetsResult(buildTargets.asJava)
  }

  def buildTargetSources(params: SourcesParams): CompletableFuture[SourcesResult] =
    nonBlocking {

      val sourcesItems = params.getTargets.asScala.toList.flatMap { id =>
        moduleCache.get(id.getUri).toSeq.map { mod =>
          val source = new SourceItem(id.getUri, SourceItemKind.FILE, false)
          new SourcesItem(id, List(source).asJava)
        }
      }

      new SourcesResult(sourcesItems.asJava)
    }

  def buildTargetDependencySources(
    params: DependencySourcesParams
  ): CompletableFuture[DependencySourcesResult] =
    on(resolutionEc) {
      val targets = params.getTargets.asScala.toList
      val items = targets.flatMap { target =>
        moduleCache.get(target.getUri).toSeq.map { mod =>
          val extra = initialClassPath.filter(_.toASCIIString.endsWith("-sources.jar")) // meh
          val jars = proc.jarDependencies(mod) match {
            case Left(err) =>
              // TODO Log error, or report via diagnostics? Only fetch non-errored deps?
              Nil
            case Right(jars0) => jars0.filter(_.last.endsWith("-sources.jar")) // meh
          }
          new DependencySourcesItem(
            target,
            (jars.map(_.toNIO.toUri) ++ extra).map(_.toASCIIString).asJava
          )
        }
      }
      new DependencySourcesResult(items.asJava)
    }

  def buildTargetInverseSources(
    params: InverseSourcesParams
  ): CompletableFuture[InverseSourcesResult] =
    nonBlocking {
      val uri = params.getTextDocument.getUri
      val id = new BuildTargetIdentifier(uri)
      val targets =
        if (moduleCache.get(id.getUri).isEmpty) Nil
        else List(id)
      new InverseSourcesResult(targets.asJava)
    }

  def buildTargetCompile(params: CompileParams): CompletableFuture[CompileResult] =
    on(compileEc) {
      val successes = params.getTargets.asScala.toList.flatMap { target =>
        moduleCache.get(target.getUri).toSeq.map { mod =>
          val name = mod.codeSource.path
            .map(p => rootOpt.fold(p.toString)(p.relativeTo(_).toString))
            .getOrElse("???")
          val taskId = new TaskId(UUID.randomUUID().toString)

          val startParams = new TaskStartParams(taskId)
          startParams.setEventTime(System.currentTimeMillis())
          startParams.setMessage(s"Compiling $name")
          startParams.setDataKind(TaskDataKind.COMPILE_TASK)
          startParams.setData(new CompileTask(target))
          clientOpt.foreach(_.onBuildTaskStart(startParams))

          // FIXME This actually compiles more scripts (the script dependencies are compiled too),
          // but this isn't reported via BSP tasks.
          val (diagnostics, res) = compiler.compile(mod, proc)
          val success = res.isRight

          val finalDiagnostics = res match {
            case Left(err) if !diagnostics.contains(mod) =>
              val end = PositionOffsetConversion.offsetToPos(mod.code)(mod.code.length)
              val extra = ("ERROR", Position(0, 0), end, err)
              diagnostics + (mod -> Seq(extra))
            case _ => diagnostics
          }

          // FIXME Includes diagnostics for the current scripts AND its script dependencies
          // (see comment above)
          var warningCount = 0
          var errorCount = 0
          finalDiagnostics
            .iterator
            .flatMap(_._2)
            .foreach {
              case ("WARNING", _, _, _) => warningCount += 1
              case ("ERROR", _, _, _) => errorCount += 1
              case _ =>
            }

          val finishParams = new TaskFinishParams(
            taskId,
            if (success) StatusCode.OK else StatusCode.ERROR
          )
          finishParams.setEventTime(System.currentTimeMillis())
          finishParams.setMessage(if (success) s"Compiled $name" else s"Error compiling $name")
          finishParams.setDataKind(TaskDataKind.COMPILE_REPORT)
          finishParams.setData(new CompileReport(target, errorCount, warningCount))
          clientOpt.foreach(_.onBuildTaskFinish(finishParams))

          for ((mod0, modDiagnostics) <- finalDiagnostics) {
            val modDiagnostics0 = modDiagnostics.map {
              case (severity, start, end, msg) =>
                val start0 = new BPosition(start.line, start.char)
                val end0 = new BPosition(end.line, end.char)
                val diagnostic = new Diagnostic(new Range(start0, end0), msg)
                diagnostic.setSeverity(
                  severity match {
                    case "INFO" => DiagnosticSeverity.INFORMATION
                    case "WARNING" => DiagnosticSeverity.WARNING
                    case "ERROR" => DiagnosticSeverity.ERROR
                    case _ => sys.error(s"Unrecognized severity: $severity")
                  }
                )
                diagnostic
            }

            val bspDiagnostics = new PublishDiagnosticsParams(
              new TextDocumentIdentifier(
                mod0
                  .codeSource.path
                  .getOrElse(???)
                  .toNIO
                  .toUri
                  .toASCIIString
              ),
              target,
              modDiagnostics0.asJava,
              true // ???
            )

            clientOpt.foreach(_.onBuildPublishDiagnostics(bspDiagnostics))
          }

          success
        }
      }

      val success = successes.forall(identity)

      new CompileResult(if (success) StatusCode.OK else StatusCode.ERROR)
    }

  def buildTargetScalacOptions(
    params: ScalacOptionsParams
  ): CompletableFuture[ScalacOptionsResult] =
    on(resolutionEc) {
      val targets = params.getTargets.asScala.toList
      val items = targets.flatMap { target =>
        moduleCache.get(target.getUri).toSeq.map { mod =>
          val extra = initialClassPath.filter(!_.toASCIIString.endsWith("-sources.jar")) // meh
          val jars = proc.jarDependencies(mod) match {
            case Left(err) =>
              // TODO Log error, or report via diagnostics? Only fetch non-errored deps?
              Nil
            case Right(jars0) => jars0.filter(!_.last.endsWith("-sources.jar")) // meh
          }
          val classDirectory = compiler.moduleTarget(mod).getOrElse(???)
          new ScalacOptionsItem(
            target,
            compiler.moduleSettings(mod).asJava,
            (jars.map(_.toNIO.toUri) ++ extra).map(_.toASCIIString).asJava,
            classDirectory.toNIO.toAbsolutePath.toUri.toASCIIString
          )
        }
      }
      new ScalacOptionsResult(items.asJava)
    }

}

object AmmoniteBuildServer {

  private def buildTargetIdentifier(p: os.Path): BuildTargetIdentifier =
    new BuildTargetIdentifier(p.toNIO.toAbsolutePath.toUri.toASCIIString)

  private def nonBlocking[T](t: T): CompletableFuture[T] =
    CompletableFuture.completedFuture(t)

  private def on[T](ec: ExecutionContext)(t: => T): CompletableFuture[T] = {
    implicit val ec0 = ec
    val cf = new CompletableFuture[T]
    val f = Future(t)
    f.onComplete {
      case Success(t) => cf.complete(t)
      case Failure(ex) => cf.completeExceptionally(ex)
    }
    cf
  }

  private def threadFactory(name: String): ThreadFactory =
    new ThreadFactory {
      val threadNumber = new AtomicInteger(1)
      override def newThread(r: Runnable): Thread = {
        val threadNumber0 = threadNumber.getAndIncrement()
        val t = new Thread(r, s"$name-$threadNumber0")
        t.setDaemon(true)
        t.setPriority(Thread.NORM_PRIORITY)
        t
      }
    }

  private lazy val isolatedApi: Boolean = {
    val interpLoader = classOf[AmmoniteBuildServer].getClassLoader
    val interpApiLoader = classOf[InterpAPI].getClassLoader
    interpLoader != interpApiLoader
  }

  private def alreadyLoadedDependencies(
    resourceName: String =
      if (isolatedApi) "amm-interp-api-dependencies.txt"
      else "amm-dependencies.txt"
  ): Seq[Dependency] = {

    var is: InputStream = null

    try {
      is = Thread.currentThread().getContextClassLoader.getResourceAsStream(resourceName)
      if (is == null)
        throw new Exception(s"Resource $resourceName not found")
      scala.io.Source.fromInputStream(is)(scala.io.Codec.UTF8)
        .mkString
        .split('\n')
        .filter(_.nonEmpty)
        .map(l => l.split(':') match {
          case Array(org, name, ver) =>
            Dependency.of(org, name, ver)
          case other =>
            throw new Exception(s"Cannot parse line '$other' from resource $resourceName")
        })
    } finally {
      if (is != null)
        is.close()
    }
  }

  def classPathWhitelist(): Set[Seq[String]] =
    if (isolatedApi)
      Set.empty
    else
      os.read
        .lines(os.resource / "ammonite-api-whitelist.txt")
        .map(_.split('/').toSeq)
        .toSet

  def start(
    server: AmmoniteBuildServer,
    input: InputStream = System.in,
    output: OutputStream = System.out
  ): Launcher[BuildClient] = {
    val ec = Executors.newFixedThreadPool(4) // FIXME Daemon threads
    val launcher = new Launcher.Builder[BuildClient]()
      .setExecutorService(ec)
      .setInput(input)
      .setOutput(output)
      .setRemoteInterface(classOf[BuildClient])
      .setLocalService(server)
      .create()
    val client = launcher.getRemoteProxy
    server.onConnectWithClient(client)
    launcher
  }

}
