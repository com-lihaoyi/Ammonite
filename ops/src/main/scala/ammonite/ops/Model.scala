package ammonite.ops

import java.nio.file.{LinkOption, Paths, Files}
import java.nio.file.attribute._

import ammonite.ops

import scala.util.Try

/**
  * Simple enum with the possible filesystem objects a path can resolve to
  */
sealed trait FileType
object FileType{
  case object File extends FileType
  case object Dir extends FileType
  case object SymLink extends FileType
  case object Other extends FileType
}
object PermSet{

}

/**
  * A set of permissions
  */
class PermSet(s: Set[PosixFilePermission]) extends Set[PosixFilePermission]{
  def contains(elem: PosixFilePermission) = s.contains(elem)
  def +(elem: PosixFilePermission) = new PermSet(s + elem)
  def -(elem: PosixFilePermission) = new PermSet(s - elem)
  def iterator = s.iterator

}


object stat extends Function1[ops.Path, ops.stat]{
  def apply(p: ops.Path) = ops.stat.make(
    // Don't blow up if we stat `root`
    p.segments.lastOption.getOrElse("/"),
    Files.readAttributes(
      Paths.get(p.toString),
      classOf[BasicFileAttributes],
      LinkOption.NOFOLLOW_LINKS
    ),
    Try(Files.readAttributes(
      Paths.get(p.toString),
      classOf[PosixFileAttributes],
      LinkOption.NOFOLLOW_LINKS
    )).toOption
  )
  def make(name: String, attrs: BasicFileAttributes, posixAttrs: Option[PosixFileAttributes]) = {
    import collection.JavaConversions._
    new stat(
      name,
      attrs.size(),
      attrs.lastModifiedTime(),
      posixAttrs.map(_.owner).getOrElse(null),
      posixAttrs.map(a => new PermSet(a.permissions.toSet)).getOrElse(null),
      if (attrs.isRegularFile) FileType.File
      else if (attrs.isDirectory) FileType.Dir
      else if (attrs.isSymbolicLink) FileType.SymLink
      else if (attrs.isOther) FileType.Other
      else ???
    )
  }
  object full extends Function1[ops.Path, ops.stat.full] {
    def apply(p: ops.Path) = ops.stat.full.make(
      p.last,
      Files.readAttributes(
        Paths.get(p.toString),
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      ),
      Try(Files.readAttributes(
        Paths.get(p.toString),
        classOf[PosixFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )).toOption
    )
    def make(name: String, attrs: BasicFileAttributes, posixAttrs: Option[PosixFileAttributes]) = {
      import collection.JavaConversions._
      new full(
        name,
        attrs.size(),
        attrs.lastModifiedTime(),
        attrs.lastAccessTime(),
        attrs.creationTime(),
        posixAttrs.map(_.group()).getOrElse(null),
        posixAttrs.map(_.owner()).getOrElse(null),
        posixAttrs.map(a => new PermSet(a.permissions.toSet)).getOrElse(null),
        if (attrs.isRegularFile) FileType.File
        else if (attrs.isDirectory) FileType.Dir
        else if (attrs.isSymbolicLink) FileType.SymLink
        else if (attrs.isOther) FileType.Other
        else ???
      )
    }
  }

  /**
    * A richer, more informative version of the [[stat]] object.
    *
    * Created using `stat.full! filePath`
    */
  case class full(name: String,
                  size: Long,
                  mtime: FileTime,
                  ctime: FileTime,
                  atime: FileTime,
                  group: GroupPrincipal,
                  owner: UserPrincipal,
                  permissions: PermSet,
                  fileType: FileType){
    override def productPrefix = "stat.full"
    def isDir = fileType == FileType.Dir
    def isSymLink = fileType == FileType.SymLink
    def isFile = fileType == FileType.File
  }
}

/**
  * The result from doing an system `stat` on a particular path.
  *
  * Created via `stat! filePath`.
  *
  * If you want more information, use `stat.full`
  */
case class stat(name: String,
                size: Long,
                mtime: FileTime,
                owner: UserPrincipal,
                permissions: PermSet,
                fileType: FileType){
  def isDir = fileType == FileType.Dir
  def isSymLink = fileType == FileType.SymLink
  def isFile = fileType == FileType.File
}
