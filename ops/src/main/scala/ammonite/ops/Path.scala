package ammonite.ops


import java.nio.file.attribute._

import acyclic.file
import ammonite.pprint

/**
 * Created by haoyi on 1/25/15.
 */
object BasePath{
  def invalidChars = Set('/')
  def checkSegment(s: String) = {
    if (s.exists(BasePath.invalidChars)){
      val invalid = BasePath.invalidChars.filter(s.contains(_))
      throw PathError.InvalidSegment(s)
    }
    s match{
      case "" => throw new PathError.InvalidSegment("")
      case "core/src/test" => throw new PathError.InvalidSegment("core/src/test")
      case _ =>
    }
  }
  def reprSection(s: String, cfg: pprint.Config) = {
    val validIdentifier = "([a-zA-Z_][a-zA-Z_0-9]+)".r
    if (validIdentifier.findFirstIn(s) == Some(s)){
      implicitly[pprint.PPrinter[scala.Symbol]].render(Symbol(s), cfg)
    }else{
      implicitly[pprint.PPrinter[String]].render(s, cfg)
    }
  }
}

/**
 * A path which is either an absolute [[Path]] or a relative [[RelPath]],
 * with shared APIs and implementations.
 */
trait BasePath[ThisType <: BasePath[ThisType]]{
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
  def -(target: ThisType): RelPath

  /**
   * This path starts with the target path, including if it's identical
   */
  def >=(target: ThisType): Boolean
  /**
   * This path starts with the target path and is strictly longer than it
   */
  def >(target: ThisType): Boolean
  /**
   * The target path starts with this path, including if it's identical
   */
  def <=(target: ThisType): Boolean
  /**
   * The target path starts with this path and is strictly longer than it
   */
  def <(target: ThisType): Boolean

  /**
   * The last segment in this path. Very commonly used, e.g. it
   * represents the name of the file/folder in filesystem paths
   */
  def last: String
}

trait BasePathImpl[ThisType <: BasePath[ThisType]] extends BasePath[ThisType]{
  def segments: Seq[String]

  def make(p: Seq[String], ups: Int): ThisType

  def /(subpath: RelPath) = make(
    segments.dropRight(subpath.ups) ++ subpath.segments,
    math.max(subpath.ups - segments.length, 0)
  )

  def >=(target: ThisType) = this.segments.startsWith(target.segments)
  def >(target: ThisType) = this >= target && this != target
  def <=(target: ThisType) = target.segments.startsWith(this.segments)
  def <(target: ThisType) = this <= target && this != target
  /**
   * Gives you the file extension of this path, or the empty
   * string if there is no extension
   */
  def ext = {
    if (!segments.last.contains('.')) ""
    else segments.last.split('.').lastOption.getOrElse("")
  }

  def last = segments.last
}

object PathError{
  type IAE = IllegalArgumentException
  case class InvalidSegment(segment: String)
    extends IAE(s"Path segment [$segment] not allowed")

  case object AbsolutePathOutsideRoot
    extends IAE("The path created has enough ..s that it would start outside the root directory")

  case class NoRelativePath(src: RelPath, base: RelPath)
    extends IAE(s"Can't relativize relative paths $src from $base")
}

/**
 * An absolute path on the filesystem. Note that the path is
 * normalized and cannot contain any empty or ".". Parent ".."
 * segments can only occur at the left-end of the path, and
 * are collapsed into a single number [[ups]].
 */
case class RelPath(segments: Seq[String], ups: Int) extends BasePathImpl[RelPath]{
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
    } else throw PathError.NoRelativePath(this, base)
  }

  override def toString = (Seq.fill(ups)("..") ++ segments).mkString("/")
  override def hashCode = segments.hashCode() + ups.hashCode()
  override def equals(o: Any): Boolean = o match {
    case p: RelPath => segments == p.segments && p.ups == ups
    case _ => false
  }
}
trait RelPathStuff{
  val up = new RelPath(Nil, 1)
  val empty = new RelPath(Nil, 0)
  implicit class RelPathStart(p1: String){
    def /(p2: RelPath) = empty/p1/p2
  }
  implicit class RelPathStart2(p1: Symbol){
    def /(p2: RelPath) = empty/p1/p2
  }
}
object RelPath extends RelPathStuff with (String => RelPath){
  def apply(s: String) = {
    require(!s.startsWith("/"), "Relative paths cannot start with /")
    s.split("/").foldLeft(empty)(_/_)
  }
  implicit class Transformable1(p: RelPath){
    def nio = java.nio.file.Paths.get(p.toString)
  }

  implicit def SymPath(s: Symbol): RelPath = StringPath(s.name)
  implicit def StringPath(s: String): RelPath = {
    BasePath.checkSegment(s)
    new RelPath(Seq(s), 0)

  }
  implicit def SeqPath[T](s: Seq[T])(implicit conv: T => RelPath): RelPath = {
    s.foldLeft(empty){_ / _}
  }

  implicit def ArrayPath[T](s: Array[T])(implicit conv: T => RelPath): RelPath = SeqPath(s)

  implicit val relPathRepr = pprint.PPrinter[ammonite.ops.RelPath]{(p, c) =>
    Iterator((Seq.fill(p.ups)("up") ++ p.segments.map(BasePath.reprSection(_, c).mkString)).mkString("/"))
  }
}
object Path extends (String => Path){

  def apply(s: String): Path = {
    require(s.startsWith("/"), "Absolute Paths must start with /")

    root/RelPath.ArrayPath(
      s.split("/").drop(1).map(RelPath.StringPath)
    )
  }

  def apply(f: java.io.File): Path = apply(f.getCanonicalPath)
  def apply(f: java.nio.file.Path): Path = apply(f.toString)

  val root = new Path(Nil)
  val home = new Path(System.getProperty("user.home").split("/").drop(1))
  def makeTmp = java.nio.file.Files.createTempDirectory(
    java.nio.file.Paths.get(System.getProperty("java.io.tmpdir")), "ammonite"
  )
  implicit class Transformable(p: Path){
    def nio = java.nio.file.Paths.get(p.toString)
  }

  implicit def pathRepr = pprint.PPrinter[ammonite.ops.Path]{(p, c) =>
    Iterator("root") ++ p.segments.iterator.map("/" + BasePath.reprSection(_, c).mkString)
  }
}

/**
 * An absolute path on the filesystem. Note that the path is
 * normalized and cannot contain any empty, "." or ".." segments
 */
case class Path(segments: Seq[String]) extends BasePathImpl[Path]{
  segments.foreach(BasePath.checkSegment)

  def make(p: Seq[String], ups: Int) = {
    if (ups > 0){
      throw PathError.AbsolutePathOutsideRoot
    }
    new Path(p)
  }
  override def toString = "/" + segments.mkString("/")

  override def equals(o: Any): Boolean = o match {
    case p: Path => segments == p.segments
    case _ => false
  }
  override def hashCode = segments.hashCode()
  def -(base: Path): RelPath = {
    var newUps = 0
    var s2 = base.segments

    while(!segments.startsWith(s2)){
      s2 = s2.dropRight(1)
      newUps += 1
    }
    new RelPath(segments.drop(s2.length), newUps)
  }
  
  def suggestions: Seq[String] = {
    import scala.collection.JavaConverters._
    java.nio.file.Files.list(this.nio).iterator.asScala.map(_.getFileName.toString).toVector
  }
}