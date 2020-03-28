package ammonite.interp.internal

import java.io.{ByteArrayInputStream, InputStream}
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream}

import scala.reflect.io.{AbstractFile, Streamable, VirtualFile}

// Adapted from scala.reflect.io.URLZipArchive.
// The only relevant change here is that it doesn't rely on
// java.util.zip.ZipEntry.getSize to get the actual size of
// a zip entry, as it is usually -1 (size unknown).
// Plus allDirsByDottedName was added for scala 2.12, and dirs is memo-ized via a lazy val.
final class CustomURLZipArchive(val url: java.net.URL) extends AbstractFile with Equals { self =>

  override def toURL = url

  def file: java.io.File = null

  private def dirPath(path: String)  = path.split('/').toSeq.filter(_.nonEmpty)
  private def dirName(path: String)  = splitPath(path, front = true)
  private def baseName(path: String) = splitPath(path, front = false)
  private def splitPath(path0: String, front: Boolean): String = {
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path  = if (isDir) path0.substring(0, path0.length - 1) else path0
    val idx   = path.lastIndexOf('/')

    if (idx < 0)
      if (front) "/"
      else path
    else
    if (front) path.substring(0, idx + 1)
    else path.substring(idx + 1)
  }
  override def underlyingSource = Some(this)
  def isDirectory = true
  def lookupName(name: String, directory: Boolean) = unsupported()
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported()
  def create()  = unsupported()
  def delete()  = unsupported()
  def output    = unsupported()
  def container = unsupported()
  def absolute  = unsupported()

  abstract class Entry(path: String) extends VirtualFile(baseName(path), path) {
    // have to keep this name for compat with sbt's compiler-interface
    def getArchive: ZipFile = null
    override def underlyingSource = Some(self)
    override def toString = self.path + "(" + path + ")"
  }

  final class DirEntry(path: String) extends Entry(path) {
    val entries = collection.mutable.HashMap[String, Entry]()

    override def isDirectory = true
    override def iterator: Iterator[Entry] = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry = {
      if (directory) entries(name + "/")
      else entries(name)
    }
  }

  private object Entry {

    final class EmptyFile(name: String) extends Entry(name) {
      override def toByteArray: Array[Byte] = null
      override def sizeOption = Some(0)
    }

    final class File(name: String, content: Array[Byte]) extends Entry(name) {
      override val toByteArray: Array[Byte] = content
      private def initContent(): Unit = {
        // this initializes VirtualFile.content (private field),
        // which is required in 2.12.11, for the newly added AbstractFile.unsafeToByteArray
        val os = output
        os.write(content)
        os.close()
      }
      if (CustomURLZipArchive.is2_12_11)
        initContent()
      override def sizeOption = Some(toByteArray.length)
      // Seems we have to provide that one in 2.13
      override def input: InputStream =
        new ByteArrayInputStream(content)
    }
  }

  private def ensureDir(
    dirs: collection.mutable.Map[Seq[String], DirEntry],
    path: String,
    zipEntry: ZipEntry
  ): DirEntry =
    dirs.get(dirPath(path)) match {
      case Some(v) => v
      case None =>
        val parent = ensureDir(dirs, dirName(path), null)
        val dir    = new DirEntry(path)
        parent.entries(baseName(path)) = dir
        dirs(dirPath(path)) = dir
        dir
    }

  private def getDir(
    dirs: collection.mutable.Map[Seq[String], DirEntry],
    entry: ZipEntry
  ): DirEntry = {
    if (entry.isDirectory) ensureDir(dirs, entry.getName, entry)
    else ensureDir(dirs, dirName(entry.getName), null)
  }


  private lazy val dirs: Map[Seq[String], DirEntry] = {
    val root     = new DirEntry("/")
    val dirs     = collection.mutable.HashMap[Seq[String], DirEntry](Nil -> root)
    val in       = new ZipInputStream(new ByteArrayInputStream(Streamable.bytes(input)))

    @annotation.tailrec def loop() {
      val zipEntry = in.getNextEntry

      if (zipEntry != null) {
        val dir = getDir(dirs, zipEntry)
        if (!zipEntry.isDirectory) {
          val f =
            if (zipEntry.getSize == 0)
              new Entry.EmptyFile(zipEntry.getName)
            else {
              val content = {
                val baos = new java.io.ByteArrayOutputStream
                val b = Array.ofDim[Byte](16*1024)

                def loop() {
                  val read = in.read(b, 0, b.length)
                  if (read >= 0) {
                    baos.write(b, 0, read)
                    loop()
                  }
                }
                loop()

                baos.toByteArray
              }
              new Entry.File(zipEntry.getName, content)
            }
          dir.entries(f.name) = f
        }
        in.closeEntry()
        loop()
      }
    }

    loop()
    dirs.toMap
  }

  def iterator: Iterator[AbstractFile] =
    dirs(Nil).iterator

  def name  = url.getFile
  def path  = url.getPath
  def input = url.openStream()
  def lastModified =
    try url.openConnection().getLastModified
    catch { case _: java.io.IOException => 0 }

  override def canEqual(other: Any) = other.isInstanceOf[CustomURLZipArchive]
  override def hashCode() = url.hashCode
  override def equals(that: Any) = that match {
    case x: CustomURLZipArchive=> url == x.url
    case _                => false
  }


  def allDirsByDottedName: collection.Map[String, DirEntry] = {
    dirs.map {
      case (k, v) =>
        k.mkString(".") -> v
    }
  }

}

object CustomURLZipArchive{
  def closeZipFile = false
  private val is2_12_11 = scala.util.Properties.versionNumberString == "2.12.11"
}
