package ammonite.runtime.tools

import java.io.PrintStream

import coursier.util.Task
import coursier.LocalRepositories
import coursier.cache.{CacheLogger, FileCache}
import coursier.cache.loggers.RefreshLogger
import coursier.core.{Classifier, ModuleName, Organization}


object IvyConstructor extends IvyConstructor
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = coursier.Module(Organization(groupId), ModuleName(artifactId))
    def %%(artifactId: String) = coursier.Module(
      Organization(groupId),
      ModuleName(artifactId + "_" + IvyThing.scalaBinaryVersion)
    )
  }
  implicit class ArtifactIdExt(t: coursier.Module){
    def %(version: String) = coursier.Dependency(t, version)
  }
}

object IvyThing{
  def completer(
    repositories: Seq[coursier.Repository],
    verbose: Boolean
  ): String => (Int, Seq[String]) = {
    val cache = FileCache()
      .withLogger(if (verbose) RefreshLogger.create() else CacheLogger.nop)
    val complete = coursier.complete.Complete(cache)
      .withScalaVersion(scala.util.Properties.versionNumberString)

    s =>
      complete.withInput(s).complete().unsafeRun()(cache.ec)
  }
  def resolveArtifact(repositories: Seq[coursier.Repository],
                      dependencies: Seq[coursier.Dependency],
                      verbose: Boolean,
                      output: PrintStream,
                      hooks: Seq[coursier.Fetch[Task] => coursier.Fetch[Task]]) = synchronized {
    val fetch = coursier.Fetch()
      .addDependencies(dependencies: _*)
      .withRepositories(repositories)
      .withCache(
        FileCache()
          .withLogger(if (verbose) RefreshLogger.create() else CacheLogger.nop)
      )
      .withMainArtifacts()

    Function.chain(hooks)(fetch).eitherResult() match {
      case Left(err) => Left("Failed to resolve ivy dependencies:" + err.getMessage)
      case Right(result) =>
        val noChangingArtifact = result.artifacts.forall(!_._1.changing)
        def noVersionInterval = dependencies.map(_.version).forall { v =>
          coursier.core.Parse.versionConstraint(v).interval == coursier.core.VersionInterval.zero
        }
        val files = result.artifacts.map(_._2)
        Right((noChangingArtifact && noVersionInterval, files))
    }
  }

  val defaultRepositories = List[coursier.Repository](
    LocalRepositories.ivy2Local,
    coursier.MavenRepository("https://repo1.maven.org/maven2")
  )

  val scalaFullBinaryVersion =
    scala.util.Properties
      .versionNumberString

  val scalaBinaryVersion = {
    if (scalaFullBinaryVersion.forall(c => c.isDigit || c == '.'))
      scalaFullBinaryVersion
        .split('.')
        .take(2)
        .mkString(".")
    else
      // e.g. for versions like 2.13.0-M5
      scalaFullBinaryVersion
  }

}

