package ammonite.ops

import java.io.InputStream
import java.nio.charset.Charset

import acyclic.file

import scala.io.Codec

/**
  * Enforces a standard interface for constructing [[BasePath]]-like things
  * from java types of various sorts
  */
sealed trait PathFactory[PathType <: BasePath] extends (String => PathType) {
  def apply(f: java.io.File): PathType = apply(f.getPath)
  def apply(s: String): PathType = apply(java.nio.file.Paths.get(s))
  def apply(f: java.nio.file.Path): PathType
}

/**
  * A path which is either an absolute [[Path]], a relative [[RelPath]],
  * or a [[ResourcePath]] with shared APIs and implementations.
  *
  * Most of the filesystem-independent path-manipulation logic that lets you
  * splice paths together or navigate in and out of paths lives in this interface
  */
sealed trait BasePath {
  type ThisType <: BasePath

  /**
    * The individual path segments of this path.
    */
  def segments: Seq[String]

  /**
    * Combines this path with the given relative path, returning
    * a path of the same type as this one (e.g. `Path` returns `Path`,
    * `RelPath` returns `RelPath`
    */
  def /(subpath: RelPath): ThisType

  /**
    * Relativizes this path with the given `base` path, finding a
    * relative path `p` such that base/p == this.
    *
    * Note that you can only relativize paths of the same type, e.g.
    * `Path` & `Path` or `RelPath` & `RelPath`. In the case of `RelPath`,
    * this can throw a [[PathError.NoRelativePath]] if there is no
    * relative path that satisfies the above requirement in the general
    * case.
    */
  def relativeTo(target: ThisType): RelPath

  /**
    * This path starts with the target path, including if it's identical
    */
  def startsWith(target: ThisType): Boolean

  /**
    * The last segment in this path. Very commonly used, e.g. it
    * represents the name of the file/folder in filesystem paths
    */
  def last: String

  /**
    * Gives you the file extension of this path, or the empty
    * string if there is no extension
    */
  def ext: String
}

object BasePath {

  def invalidChars = Set('/')
  def checkSegment(s: String) = {
    def fail(msg: String) = throw PathError.InvalidSegment(s, msg)
    def considerStr =
      "use the Path(...) or RelPath(...) constructor calls to convert them. "

    s.find(BasePath.invalidChars) match {
      case Some(c) =>
        fail(
          s"[$c] is not a valid character to appear in a path segment. " +
            "If you want to parse an absolute or relative path that may have " +
            "multiple segments, e.g. path-strings coming from external sources" +
            considerStr
        )
      case None =>
    }
    def externalStr =
      "If you are dealing with path-strings coming from external sources, "
    s match {
      case "" =>
        fail(
          "Ammonite-Ops does not allow empty path segments " +
            externalStr + considerStr
        )
      case "." =>
        fail(
          "Ammonite-Ops does not allow [.] as a path segment " +
            externalStr + considerStr
        )
      case ".." =>
        fail(
          "Ammonite-Ops does not allow [..] as a path segment " +
            externalStr +
            considerStr +
            "If you want to use the `..` segment manually to represent going up " +
            "one level in the path, use the `up` segment from `ammonite.ops.up` " +
            "e.g. an external path foo/bar/../baz translates into 'foo/'bar/up/'baz."
        )
      case _ =>
    }
  }
  def chunkify(s: java.nio.file.Path) = {
    import collection.JavaConversions._
    s.iterator().map(_.toString).filter(_ != ".").toVector
  }
}

/**
  * Represents a value that is either an absolute [[Path]] or a
  * relative [[ResourcePath]], and can be constructed from
  */
sealed trait FilePath extends BasePath
object FilePath extends PathFactory[FilePath] {
  def apply(f: java.nio.file.Path) = {
    if (f.isAbsolute) Path(f)
    else RelPath(f)
  }
}

trait BasePathImpl extends BasePath {
  def segments: Seq[String]

  protected[this] def make(p: Seq[String], ups: Int): ThisType

  def /(subpath: RelPath) = make(
    segments.dropRight(subpath.ups) ++ subpath.segments,
    math.max(subpath.ups - segments.length, 0)
  )

  def ext = {
    if (!segments.last.contains('.')) ""
    else segments.last.split('.').lastOption.getOrElse("")
  }

  def last = segments.last
}

/**
  * An absolute path on the filesystem. Note that the path is
  * normalized and cannot contain any empty or ".". Parent ".."
  * segments can only occur at the left-end of the path, and
  * are collapsed into a single number [[ups]].
  */
case class RelPath private[ops] (segments: Vector[String], ups: Int) extends FilePath with BasePathImpl {
  type ThisType = RelPath
  require(ups >= 0)
  protected[this] def make(p: Seq[String], ups: Int) =
    new RelPath(p.toVector, ups + this.ups)
  def relativeTo(base: RelPath): RelPath = {
    if (base.ups < ups) {
      new RelPath(segments, ups + base.segments.length)
    } else if (base.ups == ups) {
      val commonPrefix = {
        val maxSize = scala.math.min(segments.length, base.segments.length)
        var i = 0
        while (i < maxSize && segments(i) == base.segments(i)) i += 1
        i
      }
      val newUps = base.segments.length - commonPrefix

      new RelPath(segments.drop(commonPrefix), ups + newUps)
    } else throw PathError.NoRelativePath(this, base)
  }

  def startsWith(target: RelPath) = {
    this.segments.startsWith(target.segments) && this.ups == target.ups
  }

  override def toString = segments.mkString("/")
  override def hashCode = segments.hashCode() + ups.hashCode()
  override def equals(o: Any): Boolean = o match {
    case p: RelPath => segments == p.segments && p.ups == ups
    case _ => false
  }
}

object RelPath extends RelPathStuff with PathFactory[RelPath] {
  def apply(f: java.nio.file.Path): RelPath = {

    import collection.JavaConversions._
    require(!f.isAbsolute, f + " is not an relative path")

    val segments = BasePath.chunkify(f.normalize())
    val (ups, rest) = segments.partition(_ == "..")
    new RelPath(rest, ups.length)
  }

  implicit def SymPath(s: Symbol): RelPath = StringPath(s.name)
  implicit def StringPath(s: String): RelPath = {
    BasePath.checkSegment(s)
    new RelPath(Vector(s), 0)

  }

  implicit def SeqPath[T](s: Seq[T])(implicit conv: T => RelPath): RelPath = {
    s.foldLeft(empty) { _ / _ }
  }

  implicit def ArrayPath[T](s: Array[T])(implicit conv: T => RelPath): RelPath = SeqPath(s)

  implicit val relPathOrdering: Ordering[RelPath] =
    Ordering.by((rp: RelPath) => (rp.ups, rp.segments.length, rp.segments.toIterable))
}
trait RelPathStuff {
  val up: RelPath = new RelPath(Vector.empty, 1)
  val empty: RelPath = new RelPath(Vector.empty, 0)
  implicit class RelPathStart(p1: String) {
    def /(subpath: RelPath) = empty / p1 / subpath
  }
  implicit class RelPathStart2(p1: Symbol) {
    def /(subpath: RelPath) = empty / p1 / subpath
  }
}

object Path extends PathFactory[Path] {
  def apply(p: FilePath, base: Path) = p match {
    case p: RelPath => base / p
    case p: Path => p
  }
  def apply(f: java.io.File, base: Path): Path = apply(FilePath(f), base)
  def apply(s: String, base: Path): Path = apply(FilePath(s), base)
  def apply(f: java.nio.file.Path, base: Path): Path = apply(FilePath(f), base)
  def apply(f: java.nio.file.Path): Path = {
    import collection.JavaConversions._
    val chunks = BasePath.chunkify(f)
    if (chunks.count(_ == "..") > chunks.size / 2)
      throw PathError.AbsolutePathOutsideRoot

    require(f.isAbsolute, f + " is not an absolute path")
    Path(f.getRoot, BasePath.chunkify(f.normalize()))
  }

  val root = Path(java.nio.file.Paths.get("").toAbsolutePath.getRoot)
  val home = Path(System.getProperty("user.home"))

  implicit val pathOrdering: Ordering[Path] =
    Ordering.by((rp: Path) => (rp.segments.length, rp.segments.toIterable))
}

/**
  * An absolute path on the filesystem. Note that the path is
  * normalized and cannot contain any empty `""`, `"."` or `".."` segments
  */
case class Path private[ops] (root: java.nio.file.Path, segments: Vector[String])
    extends FilePath
    with BasePathImpl
    with Readable {
  protected[ops] def getInputStream = java.nio.file.Files.newInputStream(toNIO)
  type ThisType = Path

  def toNIO = root.resolve(segments.mkString(root.getFileSystem.getSeparator))

  protected[this] def make(p: Seq[String], ups: Int) = {
    if (ups > 0) {
      throw PathError.AbsolutePathOutsideRoot
    }
    new Path(root, p.toVector)
  }
  override def toString = toNIO.toString

  override def equals(o: Any): Boolean = o match {
    case p: Path => segments == p.segments
    case _ => false
  }
  override def hashCode = segments.hashCode()

  def startsWith(target: Path) = this.segments.startsWith(target.segments)

  def relativeTo(base: Path): RelPath = {
    var newUps = 0
    var s2 = base.segments

    while (!segments.startsWith(s2)) {
      s2 = s2.dropRight(1)
      newUps += 1
    }
    RelPath(segments.drop(s2.length), newUps)
  }

  def toIO = toNIO.toFile

  override def getBytes = java.nio.file.Files.readAllBytes(toNIO)
  import collection.JavaConversions._

  override def getLines(charSet: Codec) = {
    java.nio.file.Files.readAllLines(toNIO, charSet.charSet).toVector
  }
}

object ResourcePath {
  def resource(resRoot: ResourceRoot) = {
    ResourcePath(resRoot, Vector.empty)
  }
}

/**
  * Classloaders are tricky: http://stackoverflow.com/questions/12292926
  *
  * @param resRoot
  * @param segments
  */
case class ResourcePath private[ops] (resRoot: ResourceRoot, segments: Vector[String])
    extends BasePathImpl
    with Readable {
  type ThisType = ResourcePath
  override def toString = resRoot.errorName + "/" + segments.mkString("/")

  protected[ops] def getInputStream = {
    resRoot.getResourceAsStream(segments.mkString("/")) match {
      case null => throw new ResourceNotFoundException(this)
      case stream => stream
    }
  }
  protected[this] def make(p: Seq[String], ups: Int) = {
    if (ups > 0) {
      throw PathError.AbsolutePathOutsideRoot
    }
    new ResourcePath(resRoot, p.toVector)
  }

  def relativeTo(base: ResourcePath) = {
    var newUps = 0
    var s2 = base.segments

    while (!segments.startsWith(s2)) {
      s2 = s2.dropRight(1)
      newUps += 1
    }
    RelPath(segments.drop(s2.length), newUps)
  }

  def startsWith(target: ResourcePath) = {
    segments.startsWith(target.segments)
  }

}

/**
  * Thrown when you try to read from a resource that doesn't exist.
  * @param path
  */
case class ResourceNotFoundException(path: ResourcePath) extends Exception(path.toString)

object PathError {
  type IAE = IllegalArgumentException
  private[this] def errorMsg(s: String, msg: String) =
    s"[$s] is not a valid path segment. $msg"

  case class InvalidSegment(segment: String, msg: String) extends IAE(errorMsg(segment, msg))

  case object AbsolutePathOutsideRoot
      extends IAE("The path created has enough ..s that it would start outside the root directory")

  case class NoRelativePath(src: RelPath, base: RelPath)
      extends IAE(s"Can't relativize relative paths $src from $base")
}
