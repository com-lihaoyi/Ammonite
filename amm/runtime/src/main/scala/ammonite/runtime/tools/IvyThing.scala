package ammonite.runtime.tools

import java.io.PrintWriter

import ammonite.util.{Printer, Util}


object IvyConstructor extends IvyConstructor
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = coursier.Module(groupId, artifactId)
    def %%(artifactId: String) = coursier.Module(
      groupId,
      artifactId + "_" + IvyThing.scalaBinaryVersion
    )
  }
  implicit class ArtifactIdExt(t: coursier.Module){
    def %(version: String) = coursier.Dependency(t, version)
  }
}

object IvyThing{
  def resolveArtifact(repositories: Seq[coursier.Repository],
                      dependencies: Seq[coursier.Dependency],
                      verbose: Boolean) = synchronized {

    // Print directly to stderr, since Ammonite's own internal logging
    // doesn't allow us to print weird ASCII control codes to stdout,
    // which coursier does as part of it's progress bar
    val logger = if (!verbose) None
    else {
      val logger = new coursier.TermDisplay(new PrintWriter(Console.err))
      logger.init()
      Some(logger)
    }

    val start = coursier.Resolution(dependencies.toSet)

    val fetch = coursier.Fetch.from(repositories, coursier.Cache.fetch(logger = logger))

    val resolution = start.process.run(fetch).run

    if (resolution.metadataErrors.nonEmpty){
      val formattedMsgs = for(((module, version), msgs) <- resolution.metadataErrors) yield {
        module.organization + ":" +
        module.name + ":" +
        version + " " + msgs.map(Util.newLine + "    " + _).mkString
      }
      Left(
        "Failed to resolve ivy dependencies:" +
        formattedMsgs.map(Util.newLine + "  " + _).mkString
      )
    }else {
      val localArtifacts = scalaz.concurrent.Task.gatherUnordered(
        for (a <- resolution.artifacts)
        yield coursier.Cache.file(a, logger = logger).run
      ).run

      val errors = localArtifacts.collect { case scalaz.-\/(x) => x }
      val successes = localArtifacts.collect { case scalaz.\/-(x) => x }

      if (errors.nonEmpty) {
        Left(
          "Failed to load dependencies" +
          errors.map(Util.newLine + "  " + _.describe).mkString
        )
      } else {
        Right(successes)
      }
    }
  }

  val defaultRepositories = List(
    coursier.Cache.ivy2Local,
    coursier.MavenRepository("https://repo1.maven.org/maven2")
  )

  val scalaBinaryVersion =
    scala.util.Properties
              .versionString
              .stripPrefix("version ")
              .split('.')
              .take(2)
              .mkString(".")
  
}

