package ammonite.interp.script

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConverters._

final class ScriptCache(proc: ScriptProcessor) {

  private val cache = new ConcurrentHashMap[String, Script]

  private def identifier(p: os.Path): String =
    p.toNIO.toAbsolutePath.toUri.toASCIIString

  def cleanup(): Boolean = {
    val keys = cache.keys().asScala.toVector
    val removeKeys = keys.filter { id =>
      val path = os.Path(Paths.get(new URI(id)))
      !os.isFile(path)
    }
    for (k <- removeKeys)
      cache.remove(k)
    removeKeys.nonEmpty
  }

  def load(scripts: Seq[os.Path]): Boolean = {

    val cleanupChangedThings = cleanup()

    val allModules = scripts
      .filter(os.isFile(_))
      .flatMap { p =>
        // FIXME Blocking
        proc.load(p).flatMap(proc.dependencies) match {
          case Left(err) =>
            // TODO Log error
            Nil
          case Right(modules) =>
            modules
        }
      }
      .distinct

    for {
      mod <- allModules
      p <- mod.codeSource.path
    } {
      cache.put(identifier(p), mod)
    }

    cleanupChangedThings || allModules.nonEmpty
  }

  def get(id: String): Option[Script] = {

    val path = os.Path(Paths.get(new URI(id)))

    val loadOrReload = Option(cache.get(id))
      .map { mod =>
        // TODO Keep lastModified and check that prior to reading the file from disk?
        lazy val currentContent = os.read(path)
        !os.isFile(path) || mod.code != currentContent
      }
      .getOrElse(true)

    if (loadOrReload)
      load(Seq(path))

    Option(cache.get(id))
  }

  def list: Seq[Script] =
    cache.asScala.toVector.map(_._2)

}
