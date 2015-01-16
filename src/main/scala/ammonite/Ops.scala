package ammonite
import java.io.{FileInputStream, FileOutputStream, File}
import java.nio.file.Files
import java.nio.file.attribute.{PosixFileAttributes, BasicFileAttributes}

import scala.language.dynamics

trait Op1[T1, R] extends (T1 => R){
  def apply(arg: T1): R
  def !(arg: T1): R = apply(arg)
}

trait Op2[T1, T2, R] extends ((T1, T2) => R){
  def apply(arg1: T1, arg2: T2): R
  case class !(arg1: T1) extends (T2 => R){
    def apply(arg2: T2) = Op2.this.apply(arg1, arg2)
    def !(arg2: T2): R = Op2.this.apply(arg1, arg2)
  }
}

/**
 * Makes directories up to the specified path.
 */
object mkdir extends Op1[Path, Unit]{
  def apply(path: Path) = new File(path.toString).mkdirs()
}

trait Mover{
  def check: Boolean
  def apply(t: PartialFunction[String, String])(from: Path) = {
    if (check || t.isDefinedAt(from.last)){
      val dest = from/up/t(from.last)
      new File(from.toString).renameTo(new File(dest.toString))
    }
  }
  def *(t: PartialFunction[Path, Path])(from: Path) = {
    if (check || t.isDefinedAt(from)) {
      val dest = t(from)
      mkdir! dest/up
      new File(from.toString).renameTo(new File(t(from).toString))
    }
  }
}

/**
 * Moves a file from one place to another. Creates any necessary directories
 */
object mv extends Op2[Path, Path, Unit] with Mover{
  def apply(from: Path, to: Path) = {
    new File(from.toString).renameTo(new File(to.toString))
  }
  def check = false

  object all extends Mover{
    def check = true
  }
}

/**
 * Roughly equivalent to bash's `rm -rf`. Deletes any files or folders in the
 * target path, or does nothing if there aren't any
 */
object rm extends Op1[Path, Unit]{
  def apply(target: Path) = {
    ls.rec(target).toArray
                  .reverseIterator
                  .foreach(p => new File(p.toString).delete())
    new File(target.toString).delete
  }
}

object ls extends Op1[Path, Seq[Path]]{
  def apply(arg: Path) =
    Option(new File(arg.toString).listFiles).toVector.flatMap(x=>x).map(f => Path(f.getCanonicalPath))

  object rec extends Op1[Path, Seq[Path]]{
    def recursiveListFiles(f: File): Iterator[File] = {
      def these = Option(f.listFiles).iterator.flatMap(x=>x)
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }
    def apply(arg: Path) =
      recursiveListFiles(new File(arg.toString)).map(f => Path(f.getCanonicalPath)).toVector

  }
}


class Writable(val writeableData: Array[Byte])
object Writable{
  implicit def WritableString(s: String) = new Writable(s.getBytes)
  implicit def WritableArray(a: Array[Byte]) = new Writable(a)
  implicit def WritableTraversable(a: Traversable[String]) = new Writable(a.mkString("\n").getBytes)
}

object write extends Op2[Path, Writable, Unit]{
  def apply(target: Path, data: Writable) = {
    mkdir(target/RelPath.up)
    val fw = new FileOutputStream(target.toString)
    fw.write(data.writeableData)
    fw.flush()
    fw.close()
  }
}

object read extends Op1[Path, String]{
  def apply(arg: Path) = io.Source.fromFile(arg.toString).mkString
  object lines extends Op1[Path, Iterator[String]]{
    def apply(arg: Path) = io.Source.fromFile(arg.toString).getLines()
  }
  object bytes extends Op1[Path, Array[Byte]]{
    def apply(arg: Path) = {
      val is = new FileInputStream(arg.toString)
      val cnt = is.available
      val bytes = Array.ofDim[Byte](cnt)
      is.read(bytes)
      is.close()
      bytes
    }
  }
}

object meta extends Op1[Path, PosixFileAttributes]{
  def apply(arg: Path) = {
    val file = java.nio.file.Paths.get(arg.toString)
    Files.readAttributes(file, classOf[PosixFileAttributes])

  }
}


object chmod extends Op2[Path, Unit, Unit]{
  def apply(arg1: Path, arg2: Unit) = ???
}
object chgrp extends Op2[Path, Unit, Unit]{
  def apply(arg1: Path, arg2: Unit) = ???
}
object chown extends Op2[Path, Unit, Unit]{
  def apply(arg1: Path, arg2: Unit) = ???
}
object ps extends Op1[Unit, Unit]{
  def apply(arg: Unit): Unit = ???
  object tree extends Op1[Unit, Unit]{
    def apply(arg: Unit): Unit = ???
  }
}
object kill extends Op1[Unit, Unit]{
  def apply(arg: Unit): Unit = ???
}
object ln extends Op2[Path, Path, Unit]{
  def apply(src: Path, dest: Path) = {
    java.nio.file.Files.createLink(
      java.nio.file.Paths.get(dest.toString),
      java.nio.file.Paths.get(src.toString)
    )
  }
  object s extends Op2[Path, Path, Unit]{
    def apply(src: Path, dest: Path) = {

      java.nio.file.Files.createSymbolicLink(
        java.nio.file.Paths.get(dest.toString),
        java.nio.file.Paths.get(src.toString)
      )
    }
  }
}
/*object free{
  def memory: Long = ???
  def disk: Long = ???
}
object process{
  def pid: Long = ???
  def pgrp: Long = ???
}
object system{
  object uname{
    def sysname: String = ???
    def nodename: String = ???
    def release: String = ???
    def version: String = ???
    def machine: String = ???
  }
}*/

object % extends Dynamic{
  import sys.process._
  def selectDynamic(s: String) = Seq(s).!
  def applyDynamic(s: String)(args: String*) = (s +: args).!
}