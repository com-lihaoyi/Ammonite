package ammonite.runtime.tools

import java.io.PrintStream

import coursier.util.Task
import coursier.cache.{CacheLogger, FileCache, LocalRepositories}
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
      .addClassifiers(Classifier.sources)

    Function.chain(hooks)(fetch).eitherResult() match {
      case Left(err) => Left("Failed to resolve ivy dependencies:" + err.getMessage)
      case Right((_, artifacts)) =>
        val canBeCached = artifacts.forall(!_._1.changing)
        val files = artifacts.map(_._2)
        Right((canBeCached, files))
    }
  }

  val defaultRepositories = List(
    LocalRepositories.ivy2Local,
    coursier.MavenRepository("https://repo1.maven.org/maven2")
  )

  val scalaBinaryVersion =
    scala.util.Properties
              .versionString
              .stripPrefix("version ")
              .split('.')
              .take(2)
              .mkString(".")

  val scalaFullBinaryVersion = 
    scala.util.Properties
              .versionNumberString
  
}

