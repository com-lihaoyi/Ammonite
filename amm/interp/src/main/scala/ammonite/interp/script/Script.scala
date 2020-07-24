package ammonite.interp.script

import ammonite.util.Imports
import ammonite.util.Util.CodeSource
import ammonite.runtime.ImportHook
import ammonite.util.Name

final case class Script(
  code: String,
  codeSource: CodeSource,
  blocks: Seq[Script.Block],
  processorDiagnostics: Seq[Diagnostic]
) {

  lazy val dependencyImports: Imports = {
    val importData = dependencies.scriptDependencies.flatMap(_.hookImports.value)
    Imports(importData)
  }

  lazy val dependencies: Script.Dependencies =
    blocks
      .flatMap(_.imports)
      .map(Script.dependencies)
      .foldLeft(Script.Dependencies())(_ + _)
      .noDuplicates

  lazy val options: Script.Options =
    Script.Options() // TODO Add $imports for that

  def segments(wd: Option[os.Path]): Option[Seq[String]] =
    for {
      p <- codeSource.path
      segments = wd.fold(p.segments.toVector)(wd0 => p.relativeTo(wd0).segments.toVector)
    } yield segments

  def generatedScalaPath(clsName: Name): Seq[String] =
    codeSource.pkgName.map(_.encoded) :+ s"${clsName.encoded}.scala"
}

object Script {

  final case class Import(
    code: Either[String, os.Path],
    isExec: Boolean,
    codeSource: CodeSource,
    hookImports: Imports
  )

  final case class Dependencies(
    scriptDependencies: Seq[Script.Import] = Nil,
    dependencies: Seq[coursierapi.Dependency] = Nil,
    jarDependencies: Seq[os.Path] = Nil,
    pluginDependencies: Seq[coursierapi.Dependency] = Nil,
    jarPluginDependencies: Seq[os.Path] = Nil,
    extraRepositories: Seq[coursierapi.Repository] = Nil
  ) {
    def +(other: Dependencies): Dependencies =
      Dependencies(
        scriptDependencies ++ other.scriptDependencies,
        dependencies ++ other.dependencies,
        jarDependencies ++ other.jarDependencies,
        pluginDependencies ++ other.pluginDependencies,
        jarPluginDependencies ++ other.jarPluginDependencies,
        extraRepositories ++ other.extraRepositories
      )
    def noDuplicates: Dependencies =
      Dependencies(
        scriptDependencies.distinct,
        dependencies.distinct,
        jarDependencies.distinct,
        pluginDependencies.distinct,
        jarPluginDependencies.distinct,
        extraRepositories.distinct
      )
  }

  final case class Options(
    extraScalacOptions: Seq[String] = Nil
  ) {
    def +(other: Options): Options =
      Options(
        extraScalacOptions ++ other.extraScalacOptions
      )
    def noDuplicates: Options =
      Options(
        extraScalacOptions.distinct
      )
  }

  final case class Block(
    startIdx: Int,
    leadingSpaces: String,
    statements: Seq[String],
    imports: Seq[ImportHook.Result]
  )

  final case class ResolvedDependencies(
    jars: Seq[os.Path],
    pluginJars: Seq[os.Path],
    byteCode: Seq[(String, Array[Byte])]
  )

  private def dependencies(hookRes: ImportHook.Result): Dependencies =
    hookRes match {
      case cp: ImportHook.Result.ClassPath =>
        cp.origin match {
          case Some(deps) =>
            if (cp.plugin)
              Dependencies(pluginDependencies = deps)
            else
              Dependencies(dependencies = deps)
          case None =>
            if (cp.plugin)
              Dependencies(jarPluginDependencies = cp.files)
            else
              Dependencies(jarDependencies = cp.files)
        }
      case s: ImportHook.Result.Source =>
        s.codeSource.path match {
          case Some(p) =>
            Dependencies(
              scriptDependencies =
                Seq(Import(Right(p), s.exec, s.codeSource, s.hookImports))
            )
          case None =>
            Dependencies() // TODO import $url
        }
      case ImportHook.Result.Repo(r) =>
        Dependencies(extraRepositories = Seq(r))
    }

}
