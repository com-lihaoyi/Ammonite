package ammonite.util

import java.net.URL

trait Frame {
  def classloader: ReplClassLoader
  def pluginClassloader: ReplClassLoader

  def classpath: Seq[URL]
  def version: Int

  def addHook(hook: Frame.Hook): Unit
}

object Frame {
  trait Hook {
    def addClasspath(additional: Seq[java.net.URL]): Unit
  }
}
