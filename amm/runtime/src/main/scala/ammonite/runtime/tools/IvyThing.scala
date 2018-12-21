package ammonite.runtime.tools

import java.io.{PrintStream, PrintWriter}
import coursier.util.Task

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
                      output: PrintStream,
                      hooks: Seq[coursier.Resolution => coursier.Resolution]) = synchronized {
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

    val start = Function.chain(hooks)(coursier.Resolution(dependencies.toSet))

    val fetch = coursier.Fetch.from(repositories, coursier.Cache.fetch[Task](logger = logger))

    import scala.concurrent.ExecutionContext.Implicits.global
    val resolution = start.process.run(fetch).unsafeRun()

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
        def load(artifacts: Seq[coursier.Artifact], artifactTypes: Set[String]) = {

          val loadedArtifacts = Task.gather.gather(
            for (a <- artifacts.distinct if artifactTypes(a.`type`))
              yield coursier.Cache.file[Task](a, logger = logger).run
                .map(a.isOptional -> _)
          ).unsafeRun()

          val errors = loadedArtifacts.collect {
            case (false, Left(x)) => x
            case (true, Left(x)) if !x.notFound => x
          }
          val successes = loadedArtifacts.collect { case (_, Right(x)) => x }
          (errors, successes)
        }

        val (jarErrors, jarSuccesses) = load(
          resolution.dependencyArtifacts(withOptional = true).map(_._2),
          jarTypes
        )
        val (sourceErrors, sourceSuccesses) = load(
          resolution.dependencyClassifiersArtifacts(Seq("sources")).map(_._2),
          srcTypes
        )
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

  // FIXME Allow users to tweak those?
  val jarTypes = Set("jar", "bundle")
  val srcTypes = Set("src")

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

  val scalaFullBinaryVersion = 
    scala.util.Properties
              .versionNumberString
  
}

