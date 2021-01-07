package scala.tools.nsc

import java.io.File
import java.net.URL

import ammonite.compiler.internal.CustomURLZipArchive

import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.FileUtils.AbstractFileOps
import scala.tools.nsc.classpath.{ClassPathEntries, _}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}

trait AmmClassPath extends ClassPath {
  def zipUrl: URL
  def ammPackages(inPackage: String): Seq[PackageEntry]
  def packages(inPackage: scala.tools.nsc.classpath.PackageName): Seq[PackageEntry] = {
    ammPackages(inPackage.dottedString)
  }

  def ammList(inPackage: String): ClassPathEntries
  def list(inPackage: scala.tools.nsc.classpath.PackageName): ClassPathEntries = {
    ammList(inPackage.dottedString)
  }


  def ammClasses(inPackage: String): Seq[ClassFileEntry]
  def classes(inPackage: scala.tools.nsc.classpath.PackageName): Seq[ClassFileEntry] = {
    ammClasses(inPackage.dottedString)
  }


  def ammHasPackage(pkg: String): Boolean
  def hasPackage(pkg: scala.tools.nsc.classpath.PackageName) = ammHasPackage(pkg.dottedString)
}
