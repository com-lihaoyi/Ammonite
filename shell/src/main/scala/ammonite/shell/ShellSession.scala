package ammonite.shell

import java.nio.file.NotDirectoryException

import ammonite.ops._
import ammonite.repl.frontend.FrontEndUtils
import pprint.{Config, PPrinter, PPrint}

case class ShellSession() extends OpsAPI {
  var wd0 = cwd
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
    if (!stat(arg).isDir) throw new NotDirectoryException(arg.toString)
    else {
      wd0 = arg
      wd0
    }

  }
  implicit def Relativizer[T](p: T)(implicit b: Path, f: T => RelPath): Path = b/f(p)

  implicit def lsSeqRepr: PPrint[LsSeq] = PPrint(
    PPrinter(
      (t: LsSeq, c: Config) =>
        Iterator("\n", FrontEndUtils.tabulate(
          t.map(p => Iterator(PathComplete.colorPath(p), p.last, Console.RESET).mkString),
          FrontEndUtils.width
        ).mkString)
    )
  )

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
  val cd: ammonite.ops.Path => ammonite.ops.Path

  /**
   * Allows you to use relative paths (and anything convertible to a relative
   * path) as absolute paths when working in the REPL. Note that this isn't
   * available when using Ammonite-Ops in a standalone project! In such cases,
   * it's good practice to convert paths from relative to absolute explicitly.
   */
  implicit def Relativizer[T](p: T)(implicit b: Path, f: T => RelPath): Path

}