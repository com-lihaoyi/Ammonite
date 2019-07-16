package ammonite.runtime.tools

import java.io.PrintStream

import coursierapi.{Cache, Dependency, Fetch, Logger, Repository}

import scala.collection.JavaConverters._
import scala.util.Try


object IvyThing{
  def completer(
    repositories: Seq[Repository],
    verbose: Boolean
  ): String => (Int, Seq[String]) = {
    val cache = Cache.create()
      .withLogger(if (verbose) Logger.progressBars() else Logger.nop)
    val sv = scala.util.Properties.versionNumberString

    s =>
      val res = coursierapi.Complete.create()
        .withCache(cache)
        .withScalaVersion(sv)
        .withInput(s)
        .complete()
      (res.getFrom, res.getCompletions.asScala.toVector)
  }
  def resolveArtifact(repositories: Seq[Repository],
                      dependencies: Seq[Dependency],
                      verbose: Boolean,
                      output: PrintStream,
                      hooks: Seq[Fetch => Fetch]) = synchronized {
    val fetch = Fetch.create()
      .addDependencies(dependencies: _*)
      .withRepositories(repositories: _*)
      .withCache(
        Cache.create()
          .withLogger(if (verbose) Logger.progressBars(output) else Logger.nop)
      )
      .withMainArtifacts()
      .addClassifiers("sources")

    val finalFetch = Function.chain(hooks)(fetch)
    Try(finalFetch.fetchResult()).toEither match {
      case Left(err) => Left("Failed to resolve ivy dependencies:" + err.getMessage)
      case Right(res) =>
        // should really be fetch != finalFetch, but coursierapi.Fetch is mutable for now
        val customParams = hooks.nonEmpty
        def noChangingArtifact = res.getArtifacts.asScala.forall(!_.getKey.isChanging)
        def noVersionInterval = dependencies.map(_.getVersion).forall { v =>
          !v.startsWith("latest.") &&
            !v.exists(Set('[', ']', '(', ')')) &&
            !v.endsWith("+")
        }
        val files = res.getFiles.asScala.toList
        Right((!customParams && noChangingArtifact && noVersionInterval, files))
    }
  }

  val defaultRepositories = Repository.defaults().asScala.toList

}

