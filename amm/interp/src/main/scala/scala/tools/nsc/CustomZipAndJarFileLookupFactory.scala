package scala.tools.nsc

import java.io.File
import java.net.URL

import ammonite.interp.internal.CustomURLZipArchive

import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.FileUtils.AbstractFileOps
import scala.tools.nsc.classpath.{ClassPathEntries, _}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}

// Originally based on
// https://github.com/scala/scala/blob/329deac9ab4f39e5e766ec3ab3f3f4cddbc44aa1
// /src/compiler/scala/tools/nsc/classpath/ZipAndJarFileLookupFactory.scala#L50-L166,
// then adapted to rely on ammonite.interp.internal.CustomURLZipArchive (accepting URLs)
// rather than FileZipArchive (only accepting files).

object CustomZipAndJarFileLookupFactory {

  private final class ZipArchiveClassPath(val zipUrl: URL)
    extends ClassPath with NoSourcePaths with AmmClassPath{

    def zipFile: File = null

    override def asURLs: Seq[URL] = Seq(zipUrl)

    override def asClassPathStrings: Seq[String] = Seq(zipUrl.toURI.toASCIIString) // ???

    private val archive = new CustomURLZipArchive(zipUrl)

    override def ammPackages(inPackage: String): Seq[PackageEntry] = {
      val prefix = PackageNameUtils.packagePrefix(inPackage)
      for {
        dirEntry <- findDirEntry(inPackage).toSeq
        entry <- dirEntry.iterator if entry.isPackage
      } yield PackageEntryImpl(prefix + entry.name)
    }

    protected def files(inPackage: String): Seq[ClassFileEntryImpl] =
      for {
        dirEntry <- findDirEntry(inPackage).toSeq
        entry <- dirEntry.iterator if isRequiredFileType(entry)
      } yield createFileEntry(entry)

    protected def file(inPackage: String, name: String): Option[ClassFileEntryImpl] =
      for {
        dirEntry <- findDirEntry(inPackage)
        entry <- Option(dirEntry.lookupName(name, directory = false))
        if isRequiredFileType(entry)
      } yield createFileEntry(entry)

    def ammHasPackage(pkg: String) = {
      findDirEntry(pkg).isDefined
    }

    override def ammList(inPackage: String): ClassPathEntries = {
      val foundDirEntry = findDirEntry(inPackage)

      foundDirEntry map { dirEntry =>
        val pkgBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
        val fileBuf = collection.mutable.ArrayBuffer.empty[ClassFileEntryImpl]
        val prefix = PackageNameUtils.packagePrefix(inPackage)

        for (entry <- dirEntry.iterator) {
          if (entry.isPackage)
            pkgBuf += PackageEntryImpl(prefix + entry.name)
          else if (isRequiredFileType(entry))
            fileBuf += createFileEntry(entry)
        }
        ClassPathEntries(pkgBuf, fileBuf)
      } getOrElse ClassPathEntries(Nil, Nil)
    }

    private def findDirEntry(pkg: String): Option[archive.DirEntry] =
      archive.allDirsByDottedName.get(pkg)

    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(pkg, simpleClassName + ".class").map(_.file)
    }

    // This method is performance sensitive as it is used by SBT's ExtractDependencies phase.
    override def findClass(className: String): Option[ClassRepresentation] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(pkg, simpleClassName + ".class")
    }

    override def ammClasses(inPackage: String): Seq[ClassFileEntry] =
      files(inPackage)

    protected def createFileEntry(file: CustomURLZipArchive#Entry): ClassFileEntryImpl =
      ClassFileEntryImpl(file)

    protected def isRequiredFileType(file: AbstractFile): Boolean =
      !file.isDirectory && file.hasExtension("class")
  }


  def create(zipFile: AbstractFile, settings: Settings): ClassPath =
    new ZipArchiveClassPath(zipFile.toURL)
}