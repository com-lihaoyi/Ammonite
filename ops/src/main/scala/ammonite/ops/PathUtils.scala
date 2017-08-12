package ammonite.ops

import java.io.InputStream

import scala.io.Codec

/**
  * A path that can be read from, either a [[Path]] or a [[ResourcePath]].
  * Encapsulates the logic of how to read from it in various ways.
  */
trait Readable{
  protected[ops] def getInputStream(): java.io.InputStream
  protected[ops] def getBytes(): Array[Byte] = {
    val is = getInputStream
    val out = new java.io.ByteArrayOutputStream()
    val buffer = new Array[Byte](32768)
    var r = 0
    while (r != -1) {
      r = is.read(buffer)
      if (r != -1) out.write(buffer, 0, r)
    }
    is.close()
    out.toByteArray
  }
  protected[ops] def getLineIterator(charSet: Codec) = geny.Generator.selfClosing{
    val is = getInputStream
    val s = io.Source.fromInputStream(is)(charSet)
    (s.getLines(), () => s.close())
  }

  protected[ops] def getLines(charSet: Codec): Vector[String] = {
    getLineIterator(charSet).toVector
  }
}

object Readable{
  implicit class InputStreamToReadable(is: InputStream) extends Readable{
    def getInputStream() = is
  }
}


/**
  * Represents a possible root where classpath resources can be loaded from;
  * either a [[ResourceRoot.ClassLoader]] or a [[ResourceRoot.Class]]. Resources
  * loaded from classloaders are always loaded via their absolute path, while
  * resources loaded via classes are always loaded relatively.
  */
sealed trait ResourceRoot{
  def getResourceAsStream(s: String): InputStream
  def errorName: String
}
object ResourceRoot{
  private[this] def renderClassloader(cl: java.lang.ClassLoader) = {
    cl.getClass.getName + "@" + java.lang.Integer.toHexString(cl.hashCode())
  }
  implicit def classResourceRoot(cls: java.lang.Class[_]): ResourceRoot = Class(cls)
  case class Class(cls: java.lang.Class[_]) extends ResourceRoot{
    def getResourceAsStream(s: String) = cls.getResourceAsStream(s)
    def errorName = renderClassloader(cls.getClassLoader) + ":" + cls.getName
  }
  implicit def classLoaderResourceRoot(cl: java.lang.ClassLoader): ResourceRoot = ClassLoader(cl)
  case class ClassLoader(cl: java.lang.ClassLoader) extends ResourceRoot{
    def getResourceAsStream(s: String) = cl.getResourceAsStream(s)
    def errorName = renderClassloader(cl)
  }

}

