package ammonite.interp

import java.io.File

import ammonite.runtime.Storage
import ammonite.util.Printer
import coursierapi.{Dependency, Fetch, Repository}

final class DependencyLoader(
  printer: Printer,
  storage: Storage,
  alreadyLoadedDependencies: Seq[Dependency],
  verboseOutput: Boolean
) {

  private val alwaysExclude = alreadyLoadedDependencies
    .map(dep => (dep.getModule.getOrganization, dep.getModule.getName))
    .toSet

  def load(
    coordinates: Seq[Dependency],
    repositories: => Seq[Repository],
    resolutionHooks: Seq[Fetch => Fetch]
  ): Either[String, Seq[File]] = {
    val repositories0 = repositories
    val cacheKey = (
      repositories0.hashCode.toString,
      coordinates
      // FIXME Add resolutionHooks somehow?
    )

    storage.ivyCache().get(cacheKey) match{
      case Some(res) => Right(res.map(new java.io.File(_)))
      case None =>
        ammonite.runtime.tools.IvyThing.resolveArtifact(
          repositories0,
          coordinates
            .filter(dep => !alwaysExclude((dep.getModule.getOrganization, dep.getModule.getName)))
            .map { dep =>
              alwaysExclude.iterator.foldLeft(Dependency.of(dep))((dep, excl) =>
                dep.addExclusion(excl._1, excl._2)
              )
            },
          verbose = verboseOutput,
          output = printer.errStream,
          hooks = resolutionHooks
        )match{
          case Right((canBeCached, loaded)) =>
            if (canBeCached)
              storage.ivyCache() = storage.ivyCache().updated(
                cacheKey, loaded.map(_.getAbsolutePath)
              )
            Right(loaded)
          case Left(l) =>
            Left(l)
        }
    }
  }
}
