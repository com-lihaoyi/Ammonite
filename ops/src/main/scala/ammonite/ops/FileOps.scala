/**
 * Basic operations that take place on files. Intended to be
 * both light enough to use from the command line as well as
 * powerful and flexible enough to use in real applications to
 * perform filesystem operations
 */
package ammonite.ops

import java.nio.file._

import scala.io.Codec


object Internals{


  trait Mover{
    def check: Boolean
    def apply(t: PartialFunction[String, String])(from: Path) = {
      if (check || t.isDefinedAt(from.last)){
        val dest = from/RelPath.up/t(from.last)
        Files.move(from.toNIO, dest.toNIO);
      }
    }
    def *(t: PartialFunction[Path, Path])(from: Path) = {
      if (check || t.isDefinedAt(from)) {
        val dest = t(from)
        mkdir(dest/RelPath.up)
        Files.move(from.toNIO, dest.toNIO)
      }
    }
  }


  class Writable(val writeableData: geny.Generator[Array[Byte]])

  object Writable extends LowPri{
    implicit def WritableString(s: String) = new Writable(
      geny.Generator(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    )
    implicit def WritableBytes(a: Array[Byte]): Writable = new Writable(geny.Generator(a))

  }
  trait LowPri{

    implicit def WritableGenerator[M[_], T](a: M[T])
                                           (implicit f: T => Writable,
                                            i: M[T] => geny.Generator[T]) = {
      new Writable(
        i(a).flatMap(f(_).writeableData)
      )
    }
  }
}

/**
 * An [[Callable1]] that returns a Seq[R], but can also do so
 * lazily (Iterator[R]) via `op.iter! arg`. You can then use
 * the iterator however you wish
 */
trait StreamableOp1[T1, R, C <: Seq[R]] extends Function1[T1, C]{
  def materialize(src: T1, i: geny.Generator[R]): C
  def apply(arg: T1) = materialize(arg, iter(arg))

  /**
    * Returns a lazy [[Iterator]] instead of an eager sequence of results.
    */
  val iter: T1 => geny.Generator[R]
}


trait CopyMove extends Function2[Path, Path, Unit]{

  /**
    * Copy or move a file into a particular folder, rather
    * than into a particular path
    */
  object into extends Function2[Path, Path, Unit]{
    def apply(from: Path, to: Path) = {
      CopyMove.this(from, to/from.last)
    }
  }

  /**
    * Copy or move a file, stomping over anything
    * that may have been there before
    */
  object over extends Function2[Path, Path, Unit]{
    def apply(from: Path, to: Path) = {
      rm(to)
      CopyMove.this(from, to)
    }
  }
}

/**
 * Moves a file or folder from one place to another.
 *
 * Creates any necessary directories
 */
object mv extends Function2[Path, Path, Unit] with Internals.Mover with CopyMove{
  def apply(from: Path, to: Path) = {
    require(
      !to.startsWith(from),
      s"Can't move a directory into itself: $to is inside $from"
    )
    java.nio.file.Files.move(from.wrapped, to.wrapped)
  }


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
object cp extends Function2[Path, Path, Unit] with CopyMove{
  def apply(from: Path, to: Path) = {
    require(
      !to.startsWith(from),
      s"Can't copy a directory into itself: $to is inside $from"
    )
    def copyOne(p: Path) = {
      Files.copy(p.wrapped, (to/(p relativeTo from)).wrapped)
    }

    copyOne(from)
    if (stat(from).isDir) new ammonite.ops.FilterMapExt(os.walk(from)) | copyOne
  }

}



/**
 * A specialized Seq[Path] used to provide better a better pretty-printed
 * experience
 */
case class LsSeq(base: Path, listed: RelPath*) extends Seq[Path]{
  def length = listed.length
  def apply(idx: Int) = base/listed.apply(idx)
  def iterator = listed.iterator.map(base/)
}

/**
 * Kills the given process with the given signal, e.g.
 * `kill(9)! pid`
 */
case class kill(signal: Int)(implicit wd: Path) extends Function1[Int, CommandResult]{
  def apply(pid: Int): CommandResult = {
    Shellout.%%('kill, "-" + signal, pid.toString)
  }
}
