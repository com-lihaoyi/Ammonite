package ammonite.interp

import java.net.{URL, URLClassLoader}

import ammonite.ops.Path

import scala.collection.mutable


trait SpecialClassLoader extends URLClassLoader {
  def newFileDict: mutable.Map[String, Array[Byte]]
  def addClassFile(name: String, bytes: Array[Byte]): Unit
  def findClassPublic(name: String): Class[_]
  def specialLocalClasses: Set[String]
  def add(url: URL): Unit
  def classpathSignature: Seq[(Path, Long)]
  def classpathHash: Array[Byte]
  def allJars: Seq[URL]
}
