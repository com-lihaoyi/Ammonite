/**
 * Basic operations that take place on files. Intended to be
 * both light enough to use from the command line as well as
 * powerful and flexible enough to use in real applications to
 * perform filesystem operations
 */
package ammonite.ops

import java.io.{File, InputStream}
import java.nio.file.{Files, Paths, StandardOpenOption}
import acyclic.file
import RelPath.up
import ammonite.pprint
import ammonite.pprint.PPrint

import Extensions._
object OpError{
  type IAE = IllegalArgumentException
  case class ResourceNotFound(src: Path)
    extends IAE(s"No resource found at path $src")
}

object Internals{

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

  trait Reader{
    def readIn(p: Path): InputStream
    def apply(arg: Path) = {
      val is = readIn(arg)
      val res = io.Source.fromInputStream(is).mkString
      is.close()
      res
    }

    object lines extends StreamableOp1[Path, String, Vector[String]]{
      def materialize(i: Iterator[String]) = i.toVector
      def !!(arg: Path) = {
        val is = readIn(arg)
        io.Source.fromInputStream(is).getLines()
      }
    }
    object bytes extends Op1[Path, Array[Byte]]{
      def apply(arg: Path) = {
        val is = readIn(arg)
        val out = new java.io.ByteArrayOutputStream()
        val buffer = new Array[Byte](8192)
        var r = 0
        while (r != -1) {
          r = is.read(buffer)
          if (r != -1) out.write(buffer, 0, r)
        }
        is.close()
        out.toByteArray
      }
    }
  }

  class Writable(val writeableData: Array[Byte])

  object Writable{
    implicit def WritableString(s: String) = new Writable(s.getBytes)
    implicit def WritableArray(a: Array[Byte]) = new Writable(a)
    implicit def WritableArray2(a: Array[Array[Byte]]) = new Writable(a.flatten)
    implicit def WritableTraversable(a: Traversable[String]) = new Writable(a.mkString("\n").getBytes)
  }
}

/**
 * An [[Op1]] that returns a Seq[R], but can also do so
 * lazily (Iterator[R]) via `op.!! arg`. You can then use
 * the iterator however you wish
 */
trait StreamableOp1[T1, R, C <: Seq[R]] extends Op1[T1, C]{
  def materialize(i: Iterator[R]): C
  def apply(arg: T1) = materialize(!!(arg))
  def !!(arg: T1): Iterator[R]
}


/**
 * Makes directories up to the specified path. Equivalent
 * to `mkdir -p` in bash
 */
object mkdir extends Op1[Path, Unit]{
  def apply(path: Path) = new File(path.toString).mkdirs()
}


/**
 * Moves a file from one place to another.
 * Creates any necessary directories
 */
object mv extends Op2[Path, Path, Unit] with Internals.Mover{
  def apply(from: Path, to: Path) =
    java.nio.file.Files.move(from.nio, to.nio)

  def check = false

  object all extends Internals.Mover{
    def check = true
  }
}

/**
 * Copies a file or folder from one place to another.
 * Creates any necessary directories, and copies folders
 * recursively.
 */
object cp extends Op2[Path, Path, Unit] {
  def apply(from: Path, to: Path) = {
    def copyOne(p: Path) = {
      Files.copy(Paths.get(p.toString), Paths.get((to/(p - from)).toString))
    }

    copyOne(from)
    FilterMapExt(ls.rec! from) | copyOne
  }
}

/**
 * Roughly equivalent to bash's `rm -rf`. Deletes
 * any files or folders in the target path, or
 * does nothing if there aren't any
 */
object rm extends Op1[Path, Unit]{
  def apply(target: Path) = {
    ls.rec(target).toArray
                  .reverseIterator
                  .foreach(p => new File(p.toString).delete())
    new File(target.toString).delete
  }
}

object LsSeq{
  implicit def lsSeqRepr(implicit wd: Path, cfg: pprint.Config): PPrint[LsSeq] =
    new PPrint[LsSeq](
      pprint.PPrinter[LsSeq] { (p, c) =>
        implicit val cfg = c
        implicitly[PPrint[Seq[RelPath]]].render(p.toSeq.map(_ - wd))
      },
      implicitly
    )
}

class LsSeq(s: Stream[Path]) extends Seq[Path]{
  def length = s.length
  def apply(idx: Int) = s.apply(idx)
  def iterator = s.iterator
}

/**
 * List the files and folders in a directory
 */
object ls extends StreamableOp1[Path, Path, LsSeq]{
  def materialize(i: Iterator[Path]) = new LsSeq(i.toStream)
  def !!(arg: Path) = {
    import scala.collection.JavaConverters._
    Files.newDirectoryStream(arg.nio).iterator().asScala.map(x => Path(x))
  }

  /**
   * Reads a classpath resource into memory, either as a
   * string, as a Seq[String] of lines, or as a Array[Byte]
   */
  object rec extends StreamableOp1[Path, Path, LsSeq]{
    def materialize(i: Iterator[Path]) = new LsSeq(i.toStream)
    def recursiveListFiles(f: File): Iterator[File] = {
      def these = Option(f.listFiles).iterator.flatMap(x=>x)
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }
    def !!(arg: Path) =
      recursiveListFiles(new File(arg.toString)).map(f => Path(f.getCanonicalPath))
  }
}

/**
 * Write some data to a file. This can be a String, an Array[Byte], or a
 * Seq[String] which is treated as consecutive lines. By default, this
 * fails if a file already exists at the target location. Use [[write.over]]
 * or [[write.append]] if you want to over-write it or add to what's already
 * there.
 */
object write extends Op2[Path, Internals.Writable, Unit]{
  def apply(target: Path, data: Internals.Writable) = {
    mkdir(target/RelPath.up)
    Files.write(target.nio, data.writeableData, StandardOpenOption.CREATE_NEW)
  }

  /**
   * Identical to [[write]], except if the file already exists,
   * appends to the file instead of error-ing out
   */
  object append extends Op2[Path, Internals.Writable, Unit]{
    def apply(target: Path, data: Internals.Writable) = {
      mkdir(target/RelPath.up)
      Files.write(target.nio, data.writeableData, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    }
  }
  /**
   * Identical to [[write]], except if the file already exists,
   * replaces the file instead of error-ing out
   */
  object over extends Op2[Path, Internals.Writable, Unit]{
    def apply(target: Path, data: Internals.Writable) = {
      mkdir(target/RelPath.up)
      Files.write(target.nio, data.writeableData)
    }
  }
}


/**
 * Reads a file into memory, either as a String,
 * as (read.lines(...): Seq[String]), or as (read.bytes(...): Array[Byte]).
 */
object read extends Internals.Reader with Op1[Path, String]{
  def readIn(p: Path) = {
    java.nio.file.Files.newInputStream(p.nio)
  }

  /**
   * Reads a classpath resource into memory, either as a
   * string, as a Seq[String] of lines, or as a Array[Byte]
   */
  object resource extends Internals.Reader with Op1[Path, String]{
    def readIn(p: Path) = {
      val ret = getClass.getResourceAsStream(p.toString)
      ret match{
        case null => throw new java.nio.file.NoSuchFileException(p.toString)
        case _ => ret
      }
    }
  }
}

/**
 * Checks if a file or folder exists at the given path.
 */
object exists extends Op1[Path, Boolean]{
  def apply(p: Path) = Files.exists(Paths.get(p.toString))
}

//object chmod extends Op2[Path, Unit, Unit]{
//  def apply(arg1: Path, arg2: Unit) = ???
//}
//object chgrp extends Op2[Path, Unit, Unit]{
//  def apply(arg1: Path, arg2: Unit) = ???
//}
//object chown extends Op2[Path, Unit, Unit]{
//  def apply(arg1: Path, arg2: Unit) = ???
//}
//object ps extends Op1[Unit, Unit]{
//  def apply(arg: Unit): Unit = ???
//  object tree extends Op1[Unit, Unit]{
//    def apply(arg: Unit): Unit = ???
//  }
//}



/**
 * Kills the given process with the given signal, e.g.
 * `kill(9)! pid`
 */
case class kill(signal: Int) extends Op1[Int, CommandResult]{
  def apply(pid: Int): CommandResult = {
    %kill("-" + signal, pid.toString)
  }
}
object ln extends Op2[Path, Path, Unit]{
  def apply(src: Path, dest: Path) = {
    Files.createLink(Paths.get(dest.toString), Paths.get(src.toString))
  }
  object s extends Op2[Path, Path, Unit]{
    def apply(src: Path, dest: Path) = {
      Files.createSymbolicLink(Paths.get(dest.toString), Paths.get(src.toString))
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
