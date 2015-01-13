package ammonite


import java.nio.file.Paths
import java.nio.file.attribute._

import ammonite.BasePath.{NoRelativePathException, InvalidSegmentException, AbsolutePathOutsideRoot}

import scala.collection.{mutable, TraversableLike}
import scala.collection.generic.{GenericTraversableTemplate, CanBuildFrom, TraversableFactory}
import scala.collection.immutable.Stream.StreamBuilder
import scala.collection.mutable.ListBuffer

/**
 * A path which is either an absolute [[Path]] or a relative [[RelPath]],
 * with shared APIs and implementations 
 */
trait BasePath[ThisType <: BasePath[ThisType]]{
  /**
   * The individual path segments of this path.
   */
  def segments: Seq[String]

  def make(p: Seq[String], ups: Int): ThisType

  /**
   * Combines this path with the given relative path, returning
   * a path of the same type as this one (e.g. `Path` returns `Path`,
   * `RelPath` returns `RelPath`
   */
  def /(subpath: RelPath): ThisType = {
    make(
      segments.dropRight(subpath.ups) ++ subpath.segments,
      math.max(subpath.ups - segments.length, 0)
    )
  }

  /**
   * Relativizes this path with the given `base` path, finding a
   * relative path `p` such that base/p == this. 
   * 
   * Note that you can only relativize paths of the same type, e.g.
   * `Path` & `Path` or `RelPath` & `RelPath`. In the case of `RelPath`,
   * this can throw a [[NoRelativePathException]] if there is no 
   * relative path that satisfies the above requirement in the general 
   * case.
   */
  def -(base: ThisType): RelPath

  /**
   * Gives you the file extension of this path, or the empty
   * string if there is no extension
   */
  def ext = {
    if (!segments.last.contains('.')) ""
    else segments.last.split('.').lastOption.getOrElse("")
  }
}
object BasePath{
  def invalidChars = Set('/')
  def checkSegment(s: String) = {
    if (s.exists(BasePath.invalidChars)){
      val invalid = BasePath.invalidChars.filter(s.contains(_))
      throw BasePath.InvalidCharException(s, invalid)
    }
    s match{
      case "" => throw EmptySegmentException
      case "." => throw new InvalidSegmentException(".")
      case ".." => throw new InvalidSegmentException("..")
      case _ =>
    }
  }
  case class InvalidCharException(segment: String, chars: Set[Char])
    extends Exception(s"Invalid characters $chars in path segment [$segment]")
  case class InvalidSegmentException(segment: String)
    extends Exception(s"Path segment [$segment] not allowed")
  case object EmptySegmentException
    extends Exception("Path segments must be non-empty")
  case object AbsolutePathOutsideRoot
    extends Exception("The path created has enough ..s that it would start outside the root directory")
  case class NoRelativePathException(src: RelPath, base: RelPath)
    extends Exception(s"Can't relativize relative paths $src from $base")
}

/**
 * An absolute path on the filesystem. Note that the path is 
 * normalized and cannot contain any empty, "." or ".." segments
 */
class Path(val segments: Seq[String]) extends BasePath[Path]{
  segments.foreach(BasePath.checkSegment)
  def make(p: Seq[String], ups: Int) = {
    if (ups > 0){
      throw AbsolutePathOutsideRoot
    }
    new Path(p)
  }
  override def toString = "/" + segments.mkString("/")

  override def equals(o: Any): Boolean = o match {
    case p: Path => segments == p.segments
    case _ => false
  }
  def -(base: Path): RelPath = {
    var newUps = 0
    var s2 = base.segments

    while(!segments.startsWith(s2)){
      s2 = s2.drop(1)
      newUps += 1
    }
    new RelPath(segments.drop(s2.length), newUps)
  }
}

object Path{
  def apply(s: String): Path = {
    root/RelPath.SeqPath(s.split("/").drop(1))
  }
  def apply(f: java.io.File): Path = {
    apply(f.getCanonicalPath)
  }
  val root = new Path(Nil)
  val home = new Path(System.getProperty("user.home").split("/").drop(1))
  def tmp = java.nio.file.Files.createTempDirectory(
    java.nio.file.Paths.get(System.getProperty("java.io.tmpdir")), "ammonite"
  )
}

/**
 * An absolute path on the filesystem. Note that the path is 
 * normalized and cannot contain any empty or ".". Parent ".." 
 * segments can only occur at the left-end of the path, and
 * are collapsed into a single number [[ups]].
 */
class RelPath(val segments: Seq[String], val ups: Int) extends BasePath[RelPath]{
  require(ups >= 0)
  segments.foreach(BasePath.checkSegment)
  def make(p: Seq[String], ups: Int) = new RelPath(p, ups + this.ups)
  def -(base: RelPath): RelPath = {
    if (base.ups < ups) {
      new RelPath(segments, ups + base.segments.length)
    } else if (base.ups == ups) {

      val commonPrefix = {
        val maxSize = scala.math.min(segments.length, base.segments.length)
        var i = 0
        while ( i < maxSize && segments(i) == base.segments(i)) i += 1
        i
      }
      val newUps = base.segments.length - commonPrefix

      new RelPath(segments.drop(commonPrefix), ups + newUps)
    } else throw NoRelativePathException(this, base)
  }

  override def toString = (Seq.fill(ups)("..") ++ segments).mkString("/")
  override def equals(o: Any): Boolean = o match {
    case p: RelPath => segments == p.segments && p.ups == ups
    case _ => false
  }
}

trait RelPathStuff{
  val up = new RelPath(Nil, 1)
  val empty = new RelPath(Nil, 0)
  implicit def SymPath(s: Symbol): RelPath = StringPath(s.name)
  implicit def StringPath(s: String): RelPath = {
    BasePath.checkSegment(s)
    new RelPath(Seq(s), 0)

  }
  implicit def SeqPath[T](s: Seq[T])(implicit conv: T => RelPath): RelPath = {
    s.foldLeft(empty){_ / _}
  }
  implicit def ArrayPath[T](s: Array[T])(implicit conv: T => RelPath): RelPath = SeqPath(s)
}

object RelPath extends RelPathStuff with (String => RelPath){
  def apply(s: String) = new RelPath(s.split("/").segments, 0)
}
sealed trait FileType
object FileType{
  case object File extends FileType
  case object Dir extends FileType
  case object SymLink extends FileType
  case object Other extends FileType
}
object FileData{
  def make(attrs: PosixFileAttributes) = FileData(
    attrs.size(),
    attrs.lastModifiedTime(),
    attrs.creationTime(),
    attrs.lastAccessTime(),
    attrs.group(),
    attrs.owner(),
    attrs.permissions(),
    if (attrs.isRegularFile) FileType.File
    else if (attrs.isDirectory) FileType.Dir
    else if (attrs.isSymbolicLink) FileType.SymLink
    else if (attrs.isOther) FileType.Other
    else ???
  )
}
case class FileData(size: Long,
                    mtime: FileTime,
                    ctime: FileTime,
                    atime: FileTime,
                    group: GroupPrincipal,
                    owner: UserPrincipal,
                    permissions: java.util.Set[PosixFilePermission],
                    fileType: FileType){
  def isDir = fileType == FileType.Dir
  def isSymLink = fileType == FileType.SymLink
  def isFile = fileType == FileType.File
}

object CustomTraversable extends TraversableFactory[CustomTraversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, CustomTraversable[A]] =
    new GenericCanBuildFrom[A]
  implicit def fromIterator[A](i: Iterator[A]) = new CustomTraversable[A](i.toStream)
  def newBuilder[A] = new mutable.LazyBuilder[A, CustomTraversable[A]] {
    def result(): CustomTraversable[A] = new CustomTraversable(parts.flatten.toStream)
  }
}

class CustomTraversable[+A](inner: Stream[A])
  extends Traversable[A]
  with GenericTraversableTemplate[A, CustomTraversable]
  with TraversableLike[A, CustomTraversable[A]] {

  override def companion = CustomTraversable
  override def toString = mkString("\n")
  def foreach[U](f: A => U) = inner.foreach(f)
}