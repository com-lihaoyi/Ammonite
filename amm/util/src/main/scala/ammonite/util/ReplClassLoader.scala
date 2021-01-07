package ammonite.util

import java.net.{URL, URLClassLoader}

abstract class ReplClassLoader(urls: Array[URL], parent: ClassLoader)
    extends URLClassLoader(urls, parent) {
  def inMemoryClasses: Map[String, Array[Byte]]
}
