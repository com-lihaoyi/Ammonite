package ammonite.repl.api

import java.net.URL

trait Frame {
  def classloader: ClassLoader
  def pluginClassloader: ClassLoader

  def classpath: Seq[URL]
}
