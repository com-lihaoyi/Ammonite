package ammonite.shell

import java.nio.file.NotDirectoryException
import java.nio.file.attribute.PosixFilePermission

import ammonite.ops._
import ammonite.repl.FrontEndUtils
import pprint.Renderer

import scala.util.Try

case class ShellSession() extends OpsAPI {
  var wd0 = pwd
  /**
    * The current working directory of the shell, that will get picked up by
    * any ammonite.ops commands you use
    */
  implicit def wd = wd0
  /**
    * Change the working directory `wd`; if the provided path is relative it
    * gets appended on to the current `wd`, if it's absolute it replaces.
    */
  val cd = (arg: Path) => {
    /*
     * `arg` should be a directory or a symlink to a directory
     * `realPath will be None if that is not the case,
     * otherwise - Some(realPath)
     */
    val realPath = Option(arg)
      .filter(_.isDir)
      .orElse(os.followLink(arg).filter(_.isDir))

    realPath match {
      case None => throw new NotDirectoryException(arg.toString)
      case Some(path) => wd0 = arg; wd0
    }
  }

  implicit def Relativizer[T](p: T)(implicit b: Path, f: T => RelPath): Path = b/f(p)
}

trait OpsAPI{
  /**
    * The current working directory of the shell, that will get picked up by
    * any [[Relativizer]] below, and can be modified using [[cd]]
    */
  implicit def wd: Path
  /**
    * Change the working directory `wd`; if the provided path is relative it
    * gets appended on to the current `wd`, if it's absolute it replaces. It
    * returns the resultant absolute path.
    */
  val cd: os.Path => os.Path

  /**
    * Allows you to use relative paths (and anything convertible to a relative
    * path) as absolute paths when working in the REPL. Note that this isn't
    * available when using Ammonite-Ops in a standalone project! In such cases,
    * it's good practice to convert paths from relative to absolute explicitly.
    */
  implicit def Relativizer[T](p: T)(implicit b: Path, f: T => RelPath): Path

}