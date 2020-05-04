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
import ch.epfl.scala.bsp4j.{Diagnostic => BDiagnostic, Position => BPosition, _}
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
        Seq(
          Dependency.of(
            "org.scalameta",
            "semanticdb-scalac_" + scala.util.Properties.versionNumberString,
            "4.3.9"
          )
        ),
        root,
        codeWrapper,
        importHooks
      )
  }
  private lazy val compiler =
    withRoot { root =>
      new ScriptCompiler(
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
        ),
        generateSemanticDbs = true,
        inMemoryCache = true
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

  private lazy val moduleCache = new ScriptCache(
    proc,
    events =>
      for (client <- clientOpt) {
        val params = new DidChangeBuildTarget(events.asJava)
        client.onBuildTargetDidChange(params)
      }
  )

  private def scriptBuildTarget(script: Script, path: os.Path): BuildTarget = {
    val scalaTarget = new ScalaBuildTarget(
      "org.scala-lang",
      scala.util.Properties.versionNumberString,
      scala.util.Properties.versionNumberString.split('.').take(2).mkString("."),
      ScalaPlatform.JVM,
      // TODO This seems not to matter as long as the scala-compiler JARs and all
      // are in the classpath we pass via ScalacOptions.
      List.empty[String].asJava
    )
    val directDeps = script.dependencies.scriptDependencies.flatMap(_.codeSource.path.toSeq)
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

  def workspaceBuildTargets(): CompletableFuture[WorkspaceBuildTargetsResult] =
    on(defaultEc) {
      moduleCache.load(initialScripts)
      val buildTargets = for {
        script <- moduleCache.list
        path <- script.codeSource.path.toSeq
        _ = {
          // Write generated scala code before we compile things,
          // so that clients can index things straightaway
          try compiler.preCompile(script)
          catch {
            case NonFatal(e) =>
              System.err.println(s"Caught $e")
              // FIXME Log this
          }
        }
      } yield scriptBuildTarget(script, path)
      new WorkspaceBuildTargetsResult(buildTargets.asJava)
    }

  def buildTargetSources(params: SourcesParams): CompletableFuture[SourcesResult] =
    nonBlocking {
      val sourcesItems = params.getTargets.asScala.toList.flatMap { id =>
        moduleCache.get(id.getUri).toSeq.map { _ =>
          val source = new SourceItem(id.getUri, SourceItemKind.FILE, false)
          new SourcesItem(id, List(source).asJava)
        }
      }
      new SourcesResult(sourcesItems.asJava)
    }

  private def scriptDependencySources(
    script: Script,
    target: BuildTargetIdentifier
  ): DependencySourcesItem = {
    val extra = initialClassPath.filter(_.toASCIIString.endsWith("-sources.jar")) // meh
    val jars = proc.jarDependencies(script) match {
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

  def buildTargetDependencySources(
    params: DependencySourcesParams
  ): CompletableFuture[DependencySourcesResult] =
    on(resolutionEc) {
      val items = for {
        target <- params.getTargets.asScala.toList
        script <- moduleCache.get(target.getUri).toSeq
      } yield scriptDependencySources(script, target)
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

  private def sendDiagnostics(
    client: BuildClient,
    mod0: Script,
    target: BuildTargetIdentifier,
    diagnostics: Seq[Diagnostic]
  ): Unit = {

    val bspDiagnostics = diagnostics.map {
      case Diagnostic(severity, start, end, msg) =>
        val start0 = new BPosition(start.line, start.char)
        val end0 = new BPosition(end.line, end.char)
        val diagnostic = new BDiagnostic(new Range(start0, end0), msg)
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

    val diagnosticsParams = new PublishDiagnosticsParams(
      new TextDocumentIdentifier(
        mod0
          .codeSource.path
          .getOrElse(???)
          .toNIO
          .toUri
          .toASCIIString
      ),
      target,
      bspDiagnostics.asJava,
      true // ???
    )

    client.onBuildPublishDiagnostics(diagnosticsParams)
  }

  private def startCompileTask(path: String, target: BuildTargetIdentifier): TaskId = {
    val taskId = new TaskId(UUID.randomUUID().toString)
    for (client <- clientOpt) {
      val startParams = new TaskStartParams(taskId)
      startParams.setEventTime(System.currentTimeMillis())
      startParams.setMessage(s"Compiling $path")
      startParams.setDataKind(TaskDataKind.COMPILE_TASK)
      startParams.setData(new CompileTask(target))
      client.onBuildTaskStart(startParams)
    }
    taskId
  }

  private def finishCompiling(
    taskId: TaskId,
    path: String,
    target: BuildTargetIdentifier,
    success: Boolean,
    diagnostics: Iterable[Diagnostic]
  ): Unit =
    for (client <- clientOpt) {

      var warningCount = 0
      var errorCount = 0
      diagnostics.foreach {
        case warn if warn.severity == "WARNING" => warningCount += 1
        case error if error.severity == "ERROR" => errorCount += 1
        case _ =>
      }

      val finishParams = new TaskFinishParams(
        taskId,
        if (success) StatusCode.OK else StatusCode.ERROR
      )
      finishParams.setEventTime(System.currentTimeMillis())
      finishParams.setMessage(if (success) s"Compiled $path" else s"Error compiling $path")
      finishParams.setDataKind(TaskDataKind.COMPILE_REPORT)
      finishParams.setData(new CompileReport(target, errorCount, warningCount))
      clientOpt.foreach(_.onBuildTaskFinish(finishParams))
    }

  private def compileScript(
    script: Script,
    dependencies: Script.ResolvedDependencies
  ): ScriptCompileResult = {

    def actualDiagnostics(result: ScriptCompileResult) =
      result.errorOrOutput match {
        case Left(err) if result.diagnostics.isEmpty =>
          val end = PositionOffsetConversion.offsetToPos(script.code)(script.code.length)
          Seq(Diagnostic("ERROR", Position(0, 0), end, err))
        case _ => result.diagnostics
      }

    val path = script.codeSource.path.getOrElse {
      sys.error("Unexpected script with no path")
    }
    val target = buildTargetIdentifier(path)

    val result = compiler
      .compileFromCache(script, dependencies)
      .getOrElse {
        val name = rootOpt.fold(path.toString)(path.relativeTo(_).toString)
        val taskId = startCompileTask(name, target)
        val result0 = compiler.compile(script, dependencies)
        finishCompiling(
          taskId,
          name,
          target,
          result0.errorOrOutput.isRight,
          actualDiagnostics(result0)
        )
        result0
      }

    for (client <- clientOpt)
      sendDiagnostics(client, script, target, actualDiagnostics(result))

    result
  }

  private def compileScript(script: Script, target: BuildTargetIdentifier): Boolean = {

    val (_, res) = compiler.compile(
      script,
      proc,
      doCompile = compileScript(_, _)
    )

    res.isRight
  }

  def buildTargetCompile(params: CompileParams): CompletableFuture[CompileResult] =
    on(compileEc) {
      val successes = for {
        target <- params.getTargets.asScala.toList
        script <- moduleCache.get(target.getUri).toSeq
      } yield compileScript(script, target)
      val success = successes.forall(identity)
      new CompileResult(if (success) StatusCode.OK else StatusCode.ERROR)
    }

  private def scriptScalacOptions(
    script: Script,
    target: BuildTargetIdentifier
  ): ScalacOptionsItem = {
    val extra = initialClassPath.filter(!_.toASCIIString.endsWith("-sources.jar")) // meh
    val scriptDependenciesTargets = proc.dependencies(script) match {
      case Left(err) =>
        // TODO Log error, or report via diagnostics?
        Nil
      case Right(scriptDeps) =>
        scriptDeps
          .flatMap(compiler.moduleTarget(_).toSeq)
          .map(_.toNIO.toUri.toASCIIString)
    }
    val jars = proc.jarDependencies(script) match {
      case Left(err) =>
        // TODO Log error, or report via diagnostics? Only fetch non-errored deps?
        Nil
      case Right(jars0) =>
        jars0
          .filter(!_.last.endsWith("-sources.jar")) // meh
          .map(_.toNIO.toUri.toASCIIString)
    }
    val classDirectory = compiler.moduleTarget(script).getOrElse(???)
    new ScalacOptionsItem(
      target,
      compiler.moduleSettings(script).asJava,
      (scriptDependenciesTargets ++ jars ++ extra.map(_.toASCIIString)).asJava,
      classDirectory.toNIO.toAbsolutePath.toUri.toASCIIString
    )
  }

  def buildTargetScalacOptions(
    params: ScalacOptionsParams
  ): CompletableFuture[ScalacOptionsResult] =
    on(resolutionEc) {
      val items = for {
        target <- params.getTargets.asScala.toList
        script <- moduleCache.get(target.getUri).toSeq
      } yield scriptScalacOptions(script, target)
      new ScalacOptionsResult(items.asJava)
    }

  def buildTargetCleanCache(params: CleanCacheParams): CompletableFuture[CleanCacheResult] =
    on(compileEc) {
      compiler.clearCache()
      new CleanCacheResult("", true)
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
