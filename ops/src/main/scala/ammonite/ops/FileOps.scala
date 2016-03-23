/**
 * Basic operations that take place on files. Intended to be
 * both light enough to use from the command line as well as
 * powerful and flexible enough to use in real applications to
 * perform filesystem operations
 */
package ammonite.ops

import java.io.{File, InputStream}
import java.nio.charset.Charset
import java.nio.file._

import acyclic.file


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


  class Writable(val writeableData: Array[Byte])

  object Writable{
    implicit def WritableString(s: String) = new Writable(s.getBytes("UTF-8"))
    implicit def WritableArray(a: Array[Byte]) = new Writable(a)
    implicit def WritableArray2(a: Array[Array[Byte]]) = new Writable(a.flatten)
    implicit def WritableTraversable(a: Traversable[String]) = {
      new Writable(a.mkString("\n").getBytes("UTF-8"))
    }
  }
}

/**
 * An [[Callable1]] that returns a Seq[R], but can also do so
 * lazily (Iterator[R]) via `op.iter! arg`. You can then use
 * the iterator however you wish
 */
trait StreamableOp1[T1, R, C <: Seq[R]] extends Function1[T1, C]{
  def materialize(src: T1, i: Iterator[R]): C
  def apply(arg: T1) = materialize(arg, iter(arg))

  /**
    * Returns a lazy [[Iterator]] instead of an eager sequence of results.
    */
  val iter: T1 => Iterator[R]
}


/**
 * Makes directories up to the specified path. Equivalent
 * to `mkdir -p` in bash
 */
object mkdir extends Function1[Path, Unit]{
  def apply(path: Path) = new File(path.toString).mkdirs()
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
    java.nio.file.Files.move(from.toNIO, to.toNIO)
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
      Files.copy(p.toNIO, (to/(p relativeTo from)).toNIO)
    }

    copyOne(from)
    if (stat(from).isDir) FilterMapExt(ls.rec! from) | copyOne
  }

}

/**
 * Roughly equivalent to bash's `rm -rf`. Deletes
 * any files or folders in the target path, or
 * does nothing if there aren't any
 */
object rm extends Function1[Path, Unit]{
  def apply(target: Path) = {
    require(
      target.segments.nonEmpty,
      s"Cannot rm a root directory: $target"
    )
    // Emulate `rm -rf` functionality by ignoring non-existent files
    val files =
      try ls.rec! target
      catch {
        case e: NoSuchFileException => Nil
        case e: NotDirectoryException => Nil
      }

    files.toArray
         .reverseIterator
         .foreach(p => new File(p.toString).delete())
    new File(target.toString).delete
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

trait ImplicitOp[V] extends Function1[Path, V]{
  /**
   * Make the common case of looking around the current directory fast by
   * letting the user omit the argument if there's one in scope
   */
  def !(implicit arg: Path): V = apply(arg)
}
/**
  * List the files and folders in a directory. Can be called with `.iter`
  * to return an iterator, or `.rec` to recursively list everything in
  * subdirectories. `.rec` is a [[ls.Walker]] which means that apart from
  * straight-forwardly listing everything, you can pass in a `skip` predicate
  * to cause your recursion to skip certain files or folders.
  */
object ls extends StreamableOp1[Path, Path, LsSeq] with ImplicitOp[LsSeq]{
  def materialize(src: Path, i: Iterator[Path]) =
    new LsSeq(src, i.map(_ relativeTo src).toVector.sorted:_*)


  object iter extends (Path => Iterator[Path]){
    def apply(arg: Path) = {
      import scala.collection.JavaConverters._
      val dirStream = Files.newDirectoryStream(arg.toNIO)
      new SelfClosingIterator(
        dirStream.iterator().asScala.map(x => Path(x)),
        () => dirStream.close()
      )
    }
  }

  object rec extends Walker(){
    def apply(skip: Path => Boolean = _ => false,
              preOrder: Boolean = false) = Walker(skip, preOrder)
  }

  /**
    * Walks a directory recursively and returns a [[LsSeq]] of all its contents.
    *
    * @param skip Skip certain files or folders from appearing in the output.
    *             If you skip a folder, its entire subtree is ignored
    * @param preOrder Whether you want a folder to appear before or after its
    *                 contents in the final sequence. e.g. if you're deleting
    *                 them recursively you want it to be false so the folder
    *                 gets deleted last, but if you're copying them recursively
    *                 you want `preOrder` to be `true` so the folder gets
    *                 created first.
    */
  case class Walker(skip: Path => Boolean = _ => false,
                    preOrder: Boolean = false)
  extends StreamableOp1[Path, Path, LsSeq] with ImplicitOp[LsSeq]{

    def materialize(src: Path, i: Iterator[Path]) = ls.this.materialize(src, i)
    def recursiveListFiles(p: Path): Iterator[Path] = {
      def these = ls.iter! p
      for{
        thing <- these
        if !skip(thing)
        sub <- {
          if (!stat(thing).isDir) Iterator(thing)
          else{
            val children = recursiveListFiles(thing)
            if (preOrder) Iterator(thing) ++ children
            else children ++ Iterator(thing)
          }
        }
      } yield sub
    }
    object iter extends (Path => Iterator[Path]){
      def apply(arg: Path) = recursiveListFiles(arg)
    }

  }
}

/**
 * Write some data to a file. This can be a String, an Array[Byte], or a
 * Seq[String] which is treated as consecutive lines. By default, this
 * fails if a file already exists at the target location. Use [[write.over]]
 * or [[write.append]] if you want to over-write it or add to what's already
 * there.
 */
object write extends Function2[Path, Internals.Writable, Unit]{
  def apply(target: Path, data: Internals.Writable) = {
    mkdir(target/RelPath.up)
    Files.write(target.toNIO, data.writeableData, StandardOpenOption.CREATE_NEW)
  }

  /**
   * Identical to [[write]], except if the file already exists,
   * appends to the file instead of error-ing out
   */
  object append extends Function2[Path, Internals.Writable, Unit]{
    def apply(target: Path, data: Internals.Writable) = {
      mkdir(target/RelPath.up)
      Files.write(
        target.toNIO,
        data.writeableData,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    }
  }
  /**
   * Identical to [[write]], except if the file already exists,
   * replaces the file instead of error-ing out
   */
  object over extends Function2[Path, Internals.Writable, Unit]{
    def apply(target: Path, data: Internals.Writable) = {
      mkdir(target/RelPath.up)
      Files.write(target.toNIO, data.writeableData)
    }
  }
}


/**
 * Reads a file into memory, either as a String,
 * as (read.lines(...): Seq[String]), or as (read.bytes(...): Array[Byte]).
 */
object read extends Function1[InputPath, String]{
  def getInputStream(p: InputPath) = p.getInputStream()

//  def apply(arg: InputPath) = new String(arg.getBytes, Charset.forName("UTF-8"))
  def apply(arg: InputPath) = apply(arg, "utf-8")
  def apply(arg: InputPath, charSet: String) = new String(arg.getBytes, charSet)

  object lines extends StreamableOp1[InputPath, String, Vector[String]]{
    def materialize(src: InputPath, i: Iterator[String]) = i.toVector

    object iter extends (InputPath => Iterator[String]){
      def apply(arg: InputPath) = arg.getLineIterator("utf-8")
      def apply(arg: InputPath, charSet: String) = arg.getLineIterator(charSet)
    }

    def apply(arg: InputPath, charSet: String) = arg.getLines(charSet)
    override def apply(arg: InputPath) = apply(arg, "utf-8")
  }
  object bytes extends Function1[InputPath, Array[Byte]]{
    def apply(arg: InputPath) = arg.getBytes
  }
}

/**
 * Checks if a file or folder exists at the given path.
 */
object exists extends Function1[Path, Boolean]{
  def apply(p: Path) = Files.exists(Paths.get(p.toString))
}

//object chmod extends Function2[Path, Unit, Unit]{
//  def apply(arg1: Path, arg2: Unit) = ???
//}
//object chgrp extends Function2[Path, Unit, Unit]{
//  def apply(arg1: Path, arg2: Unit) = ???
//}
//object chown extends Function2[Path, Unit, Unit]{
//  def apply(arg1: Path, arg2: Unit) = ???
//}
//object ps extends Function1[Unit, Unit]{
//  def apply(arg: Unit): Unit = ???
//  object tree extends Function1[Unit, Unit]{
//    def apply(arg: Unit): Unit = ???
//  }
//}



/**
 * Kills the given process with the given signal, e.g.
 * `kill(9)! pid`
 */
case class kill(signal: Int) extends Function1[Int, CommandResult]{
  def apply(pid: Int): CommandResult = {

    %%('kill, "-" + signal, pid.toString)(wd = Path(new java.io.File("")))
  }
}

/**
  * Creates a hardlink between two paths. Use `.s(src, dest)` to create a
  * symlink
  */
object ln extends Function2[Path, Path, Unit]{
  def apply(src: Path, dest: Path) = {
    Files.createLink(Paths.get(dest.toString), Paths.get(src.toString))
  }
  object s extends Function2[Path, Path, Unit]{
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
