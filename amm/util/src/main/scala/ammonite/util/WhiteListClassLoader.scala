package ammonite.util

import java.net.URLClassLoader

class WhiteListClassLoader(whitelist: Set[Seq[String]], parent: ClassLoader)
    extends URLClassLoader(Array(), parent) {
  override def loadClass(name: String, resolve: Boolean) = {
    val tokens = name.split('.')
    if (Util.lookupWhiteList(whitelist, tokens.init ++ Seq(tokens.last + ".class"))) {
      super.loadClass(name, resolve)
    } else {
      throw new ClassNotFoundException(name)
    }

  }
  override def getResource(name: String) = {
    if (Util.lookupWhiteList(whitelist, name.split('/'))) super.getResource(name)
    else null
  }
}
