package scala.tools.nsc

import java.io.File
import java.net.URL

import ammonite.compiler.internal.CustomURLZipArchive

import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.FileUtils.AbstractFileOps
import scala.tools.nsc.classpath.{ClassPathEntries, _}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}


trait AmmClassPath extends ClassPath{
  def zipUrl: URL
  def ammPackages(inPackage: String): Seq[PackageEntry]
  def packages(inPackage: String): Seq[PackageEntry] = {
    ammPackages(inPackage)
  }

  def ammList(inPackage: String): ClassPathEntries
  def list(inPackage: String): ClassPathEntries = {
    ammList(inPackage)
  }


  def ammClasses(inPackage: String): Seq[ClassFileEntry]
  def classes(inPackage: String): Seq[ClassFileEntry] = {
    ammClasses(inPackage)
  }

  def ammHasPackage(pkg: String): Boolean
  def hasPackage(pkg: String) = ammHasPackage(pkg)
}
