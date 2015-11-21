package ammonite.repl.interp

import java.net.{URL, URLClassLoader}
import java.security.MessageDigest

import ammonite.ops.{stat, Path}
import ammonite.repl.Util

import scala.collection.mutable

class RootSpecialClassLoader(parent: ClassLoader) extends SpecialClassLoader(parent){
  //we don't need to hash in classes from newFileDict, because cache tag depend on
  //them implicitly through having their hash in the imports.
  def initialClasspathHash = {

    // Lol this is so hax. But it works! And is way faster than trying
    // to iterate over *every* file in the classpath, and way more
    // convenient than trying to futz around with java agents
    val allClasses = collection.mutable.Buffer.empty[Class[_]]
    val f = classOf[ClassLoader].getDeclaredField("classes")
    f.setAccessible(true)
    var current: ClassLoader = this
    while(current != null){
      val classes = f.get(current).asInstanceOf[java.util.Vector[Class[_]]]
      import collection.JavaConversions._
      allClasses.appendAll(classes)
      current = current.getParent
    }
    val resources = for {
      cls <- allClasses
      res = this.getResource(cls.getName.replace(".", "/") + ".class")
      if res != null
      if res.getProtocol == "file"
    } yield res

    Util.md5Hash(
      for(res <- resources.distinct.iterator) yield {
        val path = Path(java.nio.file.Paths.get(res.toURI))
        val millis = stat(path).mtime.toMillis
        Util.md5Hash(Iterator(
          path.toString.getBytes,
          (0 until 64 by 8).map(offset => (millis >> offset).toByte).toArray
        ))
      }
    )
  }
}
class ChildSpecialClassLoader(parent: SpecialClassLoader) extends SpecialClassLoader(parent){
  def initialClasspathHash = parent.classpathHash

}
/**
  * Classloader used to implement the jar-downloading
  * command-evaluating logic in Ammonite.
  *
  * http://stackoverflow.com/questions/3544614/how-is-the-control-flow-to-findclass-of
  */
abstract class SpecialClassLoader(parent: ClassLoader) extends URLClassLoader(Array(), parent){
  /**
    * Files which have been compiled, stored so that our special
    * classloader can get at them.
    */
  val newFileDict = mutable.Map.empty[String, Array[Byte]]
  def findClassPublic(name: String) = findClass(name)
  val specialLocalClasses = Set(
    "ammonite.repl.frontend.ReplBridge",
    "ammonite.repl.frontend.ReplBridge$"
  )
  override def findClass(name: String): Class[_] = {
    def loadedFromBytes =
      for(bytes <- newFileDict.get(name))
        yield defineClass(name, bytes, 0, bytes.length)

    def special =
      if (!specialLocalClasses(name)) None
      else{
        import ammonite.ops._
        //          println("Custom finding class! " + name)
        val bytes = read.resource.bytes(root/RelPath(name.replace('.', '/') + ".class"))
        Some(defineClass(name, bytes, 0, bytes.length))
      }

    Option(this.findLoadedClass(name))
      .orElse(loadedFromBytes)
      .orElse(special)
      .getOrElse(super.findClass(name))
  }
  def add(url: URL) = {
    _classpathHash = Util.md5Hash(Iterator(_classpathHash, jarHash(url)))
    addURL(url)
  }

  private def jarHash(url: URL) = {
    val digest = MessageDigest.getInstance("MD5")
    val is = url.openStream
    try {
      val byteChunk = new Array[Byte](8192)
      while({
        val n = is.read(byteChunk)
        if (n <= 0) false
        else {
          digest.update(byteChunk, 0, n)
          true
        }
      })()
    } finally {
      if (is != null) is.close()
    }

    digest.digest()
  }

  def initialClasspathHash: Array[Byte]
  var _classpathHash = initialClasspathHash
  def classpathHash: Array[Byte] = _classpathHash
}
