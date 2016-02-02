package ammonite.shell

import java.nio.file.NotDirectoryException
import java.nio.file.attribute.PosixFilePermission

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
}

object PPrints{

  implicit def lsSeqRepr: PPrinter[LsSeq] =
    PPrinter { (t: LsSeq, c: Config) =>
      val snippets = for(p <- t) yield {
        val parts =
          Iterator(PathComplete.colorPath(p)) ++
          pprint.tokenize(p relativeTo t.base)(implicitly, c).mkString ++
          Iterator(Console.RESET)
        parts.mkString
      }
      Iterator("\n") ++ FrontEndUtils.tabulate(snippets,FrontEndUtils.width)
    }

  def reprSection(s: String, cfg: pprint.Config) = {
    val validIdentifier = "([a-zA-Z_][a-zA-Z_0-9]+)".r
    if (validIdentifier.findFirstIn(s) == Some(s)){
      implicitly[pprint.PPrinter[scala.Symbol]].render(Symbol(s), cfg)
    }else{
      implicitly[pprint.PPrinter[String]].render(s, cfg)
    }
  }

  implicit val relPathRepr = pprint.PPrinter[ammonite.ops.RelPath]{(p, c) =>
    Iterator(
      (Seq.fill(p.ups)("up") ++ p.segments.map(reprSection(_, c).mkString)).mkString("/")
    )
  }

  implicit def pathRepr = pprint.PPrinter[ammonite.ops.Path]{(p, c) =>
    Iterator("root") ++ p.segments.iterator.map("/" + reprSection(_, c).mkString)
  }
  implicit def commandResultRepr: PPrinter[CommandResult] =
    PPrinter[CommandResult]((x, c) =>
      x.chunks.iterator.flatMap { chunk =>
        val (color, s) = chunk match{
          case Left(s) => (c.colors.literalColor, s)
          case Right(s) => (Console.RED, s)
        }
        Iterator("\n", color, new String(s.array), c.colors.endColor)
      }
    )
  implicit def permissionPPrintConfig: PPrinter[PermSet] =
    PPrinter[PermSet]{ (p, c) =>
      Iterator(
        "rwxrwxrwx".zip(PosixFilePermission.values()).map{ case (k, v) =>
          if(p.contains(v)) k else '-'
        }.mkString)
    }

  implicit val defaultHighlightColor = {
    GrepResult.Color(Console.BLUE + Console.YELLOW_B, Console.RESET)
  }
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