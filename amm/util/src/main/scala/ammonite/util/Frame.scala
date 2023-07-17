package ammonite.util

import java.net.URL

trait Frame {
  def classloader: ReplClassLoader
  def pluginClassloader: ReplClassLoader

  def classpath: Seq[URL]
  def version: Int

  /** Adds a [[Frame.Hook]] to be called every time JARs are added to the class path */
  def addHook(hook: Frame.Hook): Unit
}

object Frame {
  /** A hook that can be called every time JARs are added to the class path */
  trait Hook {
    /** Called when new JARs are added to the class path */
    def addClasspath(additional: Seq[java.net.URL]): Unit
  }
}
