package ammonite.repl.api

import java.net.URL

trait Frame {
  def classloader: ReplClassLoader
  def pluginClassloader: ReplClassLoader

  def classpath: Seq[URL]
  def version: Int
}
