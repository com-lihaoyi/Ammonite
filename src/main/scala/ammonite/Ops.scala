package ammonite
import java.io.{FileInputStream, FileOutputStream, File}
import java.nio.file.Files
import java.nio.file.attribute.{PosixFileAttributes, BasicFileAttributes}

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

object mkdir extends Op1[Path, Unit]{
  def apply(path: Path) = new File(path.toString).mkdirs()
}

object mv extends Op2[Path, Path, Unit]{
  def apply(from: Path, to: Path) = {
    new File(from.toString).renameTo(new File(to.toString))
  }
}

/**
 * Roughly equivalent to bash's `rm -rf`. Deletes
 */
object rm extends Op1[Path, Unit]{
  def apply(target: Path) = {
    ls.rec(target).toArray
                  .reverseIterator
                  .foreach(p => new File(p.toString).delete())
    new File(target.toString).delete
  }
}

object ls extends Op1[Path, CustomTraversable[Path]]{
  def apply(arg: Path) = new CustomTraversable(
    Option(new File(arg.toString).listFiles).toStream.flatMap(x=>x).map(f => Path(f.getCanonicalPath))
  )
  object rec extends Op1[Path, CustomTraversable[Path]]{
    def recursiveListFiles(f: File): Iterator[File] = {
      def these = Option(f.listFiles).iterator.flatMap(x=>x)
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }
    def apply(arg: Path) = new CustomTraversable(
      recursiveListFiles(new File(arg.toString)).map(f => Path(f.getCanonicalPath)).toStream
    )
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
