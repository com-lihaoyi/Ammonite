package ammonite.interp.script

import java.util.concurrent.ConcurrentHashMap

import ammonite.compiler.iface.{CodeWrapper, Imports}
import ammonite.runtime.Storage
import ammonite.util.Printer
import ammonite.util.InterfaceExtensions._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.tools.nsc.Settings

final class ScriptCompiler(
  storage: Storage,
  printer: Printer, // TODO Remove this
  codeWrapper: CodeWrapper,
  initialClassLoader: ClassLoader,
  initialImports: Imports,
  classPathWhitelist: Set[Seq[String]],
  wd: Option[os.Path],
  outputDirectory: Option[os.Path],
  generateSemanticDbs: Boolean,
  inMemoryCache: Boolean
) {

  import ScriptProcessor.SeqOps

  /** Compiles a script, along with its dependencies */
  def compile(
    module: Script,
    processor: ScriptProcessor,
    doCompile: (Script, Script.ResolvedDependencies) => ScriptCompileResult = compile(_, _)
  ): (
    Map[Script, Seq[Diagnostic]],
    Either[String, Seq[ammonite.compiler.iface.Compiler.Output]]
  ) = {

    val diagnostics = new mutable.HashMap[Script, Seq[Diagnostic]]

    val res = for {
      dependencies <- processor.dependencies(module)
      depsOutput <- dependencies
        .filter(_ != module)
        .traverse { mod =>
          // recurses, beware
          // and not stack safe…
          val (diagnostics0, res0) = compile(mod, processor, doCompile)
          diagnostics ++= diagnostics0
          res0
        }
        .left.map(_.mkString(", "))
        .map(_.flatten)

      jars <- processor.jarDependencies(module)
      pluginJars <- processor.jarPluginDependencies(module)

      resolvedDeps = Script.ResolvedDependencies(
        jars,
        pluginJars,
        depsOutput.flatMap(_.classFiles.map(e => (e.getKey, e.getValue)))
      )

      output <- {
        val compileResult = doCompile(module, resolvedDeps)
        diagnostics += module -> compileResult.diagnostics
        compileResult.errorOrOutput
      }
    } yield output

    (diagnostics.toMap, res)
  }

  /**
   * Compiles a single script.
   *
   * Beware that this *also* writes results on disk, in `outputDirectory`.
   * If semantic DB generation is enabled, semantic DBs are *only*
   * written on disk.
   */
  def compile(
    module: Script,
    dependencies: Script.ResolvedDependencies
  ): ScriptCompileResult =
    settingsOrError(module) match {
      case Left(errors) => ScriptCompileResult(Nil, Left(errors.mkString(", ")))
      case Right((settings, settingsArgs)) =>
        compileIfNeeded(settings, settingsArgs, module, dependencies)
    }

  /**
   * Reads compilation output from cache.
   */
  def compileFromCache(
    script: Script,
    dependencies: Script.ResolvedDependencies
  ): Option[ScriptCompileResult] = {
    val settingsArgs = moduleSettings(script)
    compileFromCache(settingsArgs, script, dependencies)
  }

  private def moduleOutput(module: Script): Option[os.Path] =
    for {
      outputDirectory0 <- outputDirectory
      segments0 <- module.segments(wd)
      dest = outputDirectory0 / segments0.init / segments0.last.stripSuffix(".sc")
    } yield dest

  private def moduleSources(module: Script): Option[os.Path] =
    moduleOutput(module).map(_ / "src")
  def moduleTarget(module: Script): Option[os.Path] =
    moduleOutput(module).map(_ / "target")

  /** Arguments passed to scalac to compile this script */
  def moduleSettings(module: Script): List[String] =
    if (generateSemanticDbs)
      List(
        "-Yrangepos",
        "-P:semanticdb:failures:warning",
        "-P:semanticdb:synthetics:on"
      ) ++
        moduleSources(module).map(d => s"-P:semanticdb:sourceroot:${d.toNIO.toAbsolutePath}") ++
        moduleTarget(module).map(d => s"-P:semanticdb:targetroot:${d.toNIO.toAbsolutePath}")
    else
      Nil

  private def settingsOrError(module: Script): Either[Seq[String], (Settings, Seq[String])] = {

    val args = moduleSettings(module)

    val errors = new mutable.ListBuffer[String]
    val settings0 = new Settings(err => errors += err)
    val (_, unparsed) = settings0.processArguments(args, true)
    for (arg <- unparsed)
      errors += s"Unrecognized argument: $arg"

    if (errors.isEmpty)
      Right((settings0, args))
    else
      Left(errors.toList)
  }

  /** Writes on disk the source passed to scalac, corresponding to this script */
  def preCompile(module: Script): Unit = {

    val settings = settingsOrError(module)
      .map(_._1)
      .getOrElse(new Settings(_ => ()))

    val compiler = new SingleScriptCompiler(
      initialClassLoader,
      storage,
      printer,
      initialImports,
      classPathWhitelist,
      codeWrapper,
      wd,
      generateSemanticDbs,
      settings,
      module,
      Script.ResolvedDependencies(Nil, Nil, Nil),
      moduleTarget(module),
      moduleSources(module)
    )

    compiler.writeSources()
  }

  private final case class InMemoryCacheKey(
    settings: Seq[String],
    script: Script,
    dependencies: Script.ResolvedDependencies
  ) {
    def stale: Boolean =
      script.codeSource.path.map(os.Path(_)).exists { path =>
        !os.isFile(path) || {
          // short-circuit that by looking at the file size?
          val content = os.read(path)
          script.code != content
        }
      }
  }
  private val cache = new ConcurrentHashMap[InMemoryCacheKey, ScriptCompileResult]

  def clearCache(): Unit = {
    cache.clear()
  }

  private def cleanUpCache(): Unit =
    for {
      (key, res) <- cache.asScala.toVector
      if key.stale
    } {
      cache.remove(key, res)
      for (dir <- moduleTarget(key.script).iterator ++ moduleSources(key.script).iterator)
        try os.remove.all(dir)
        catch {
          case _: java.nio.file.DirectoryNotEmptyException =>
            // Can happen on Windows, if any of the file we try to delete is opened elsewhere
        }
   }

  private def compileFromCache(
    settingsArgs: Seq[String],
    script: Script,
    dependencies: Script.ResolvedDependencies
  ): Option[ScriptCompileResult] =
    if (inMemoryCache && script.codeSource.path.nonEmpty) {
      cleanUpCache()
      val key = InMemoryCacheKey(settingsArgs, script, dependencies)
      Option(cache.get(key))
    } else
      None

  private def compileIfNeeded(
    settings0: Settings,
    settingsArgs: Seq[String],
    script: Script,
    dependencies: Script.ResolvedDependencies
  ): ScriptCompileResult =
    if (inMemoryCache && script.codeSource.path.nonEmpty) {
      val key = InMemoryCacheKey(settingsArgs, script, dependencies)
      Option(cache.get(key)).getOrElse {
        cleanUpCache()
        val res = doCompile(settings0, script, dependencies)
        Option(cache.putIfAbsent(key, res))
          .getOrElse(res)
      }
    } else
      doCompile(settings0, script, dependencies)

  private def doCompile(
    settings0: Settings,
    module: Script,
    dependencies: Script.ResolvedDependencies
  ): ScriptCompileResult = {

    val compiler = new SingleScriptCompiler(
      initialClassLoader,
      storage,
      printer,
      initialImports,
      classPathWhitelist,
      codeWrapper,
      wd,
      generateSemanticDbs,
      settings0,
      module,
      dependencies,
      moduleTarget(module),
      moduleSources(module)
    )

    compiler()
  }
}
