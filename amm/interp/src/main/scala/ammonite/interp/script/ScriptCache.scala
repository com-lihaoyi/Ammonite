package ammonite.interp.script

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap

import ch.epfl.scala.bsp4j.{BuildTargetEvent, BuildTargetEventKind, BuildTargetIdentifier}

import scala.collection.JavaConverters._

final class ScriptCache(
  proc: ScriptProcessor,
  onBuildTargetEvents: Seq[BuildTargetEvent] => Unit
) {

  private val cache = new ConcurrentHashMap[String, Script]

  private def identifier(p: os.Path): String =
    p.toNIO.toAbsolutePath.toUri.toASCIIString

  def cleanup(): Seq[BuildTargetEvent] = {
    val keys = cache.keys().asScala.toVector
    val removeKeys = keys.filter { id =>
      val path = os.Path(Paths.get(new URI(id)))
      !os.isFile(path)
    }

    for {
      k <- removeKeys
      _ <- Option(cache.remove(k))
    } yield {
      val event = new BuildTargetEvent(new BuildTargetIdentifier(k))
      event.setKind(BuildTargetEventKind.DELETED)
      event
    }
  }

  def load(scripts: Seq[os.Path]): Seq[BuildTargetEvent] = {

    val cleanupEvents = cleanup()

    val scripts0 = scripts
      .filter(os.isFile(_))
      .flatMap { p =>
        // FIXME Blocking
        val script = proc.load(p)
        Seq(script)
      }

    val dependencies = scripts0
      .flatMap { script =>
        proc.dependencies(script) match {
          case Left(err) =>
            // TODO Log error
            Nil
          case Right(modules) =>
            modules
        }
      }

    val allScripts = (scripts0 ++ dependencies).distinct

    val events = for {
      script <- allScripts
      p <- script.codeSource.path
      id = identifier(p)
      previousOpt = Option(cache.put(id, script))
      if previousOpt.forall { newScript =>
        newScript.dependencies != script.dependencies ||
          newScript.options != script.options
      }
    } yield {
      val event = new BuildTargetEvent(new BuildTargetIdentifier(id))
      val created = previousOpt.isEmpty
      event.setKind(
        if (created) BuildTargetEventKind.CREATED
        else BuildTargetEventKind.CHANGED
      )
      event
    }

    cleanupEvents ++ events
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

    if (loadOrReload) {
      val events = load(Seq(path))
      if (events.nonEmpty)
        onBuildTargetEvents(events)
    }

    Option(cache.get(id))
  }

  def list: Seq[Script] =
    cache.asScala.toVector.map(_._2)

}
