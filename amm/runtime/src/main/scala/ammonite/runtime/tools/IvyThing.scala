package ammonite.runtime.tools

import java.io.{PrintStream, PrintWriter}

import ammonite.util.Util


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
                      verbose: Boolean,
                      output: PrintStream) = synchronized {
    val writer = new PrintWriter(output)
    // Print directly to stderr, since Ammonite's own internal logging
    // doesn't allow us to print weird ASCII control codes to stdout,
    // which coursier does as part of it's progress bar
    val logger = if (!verbose) None
    else {
      val logger = new coursier.TermDisplay(writer)
      logger.init()
      Some(logger)
    }

    val start = coursier.Resolution(dependencies.toSet)

    val fetch = coursier.Fetch.from(repositories, coursier.Cache.fetch(logger = logger))

    val resolution = start.process.run(fetch).run


    val res =
      if (resolution.metadataErrors.nonEmpty){
        val formattedMsgs = for(((module, version), msgs) <- resolution.metadataErrors) yield {
          module.organization + ":" +
          module.name + ":" +
          version + " " + msgs.map(Util.newLine + "    " + _).mkString
        }
        (
          None,
          Left(
            "Failed to resolve ivy dependencies:" +
            formattedMsgs.map(Util.newLine + "  " + _).mkString
          )
        )
      }else {
        def load(artifacts: Seq[coursier.Artifact]) = {

          val loadedArtifacts = scalaz.concurrent.Task.gatherUnordered(
            for (a <- artifacts)
              yield coursier.Cache.file(a, logger = logger).run
          ).run

          val errors = loadedArtifacts.collect { case scalaz.-\/(x) => x }
          val successes = loadedArtifacts.collect { case scalaz.\/-(x) => x }
          (errors, successes)
        }

        val (jarErrors, jarSuccesses) = load(resolution.artifacts)
        val (sourceErrors, sourceSuccesses) = load(resolution.classifiersArtifacts(Seq("sources")))
        val srcWarnings =
          if (sourceErrors.isEmpty) None
          else Some(
            "Failed to load source dependencies" +
            sourceErrors.map(Util.newLine + "  " + _.describe).mkString
          )

        (
          srcWarnings,
          if (jarErrors.isEmpty) Right(jarSuccesses ++ sourceSuccesses)
          else Left(
            "Failed to load dependencies" +
            jarErrors.map(Util.newLine + "  " + _.describe).mkString
          )

        )
      }

    writer.flush()

    res
  }

  val defaultRepositories = List(
    coursier.Cache.ivy2Local,
    coursier.MavenRepository("https://repo1.maven.org/maven2"),
    coursier.MavenRepository("file://" + java.lang.System.getProperties.get("user.home") + "/.m2/repository/")
  )

  val scalaBinaryVersion =
    scala.util.Properties
              .versionString
              .stripPrefix("version ")
              .split('.')
              .take(2)
              .mkString(".")
  
}

