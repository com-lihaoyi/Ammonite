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
      .orElse(arg.tryFollowLinks.filter(_.isDir))

    realPath match {
      case None => throw new NotDirectoryException(arg.toString)
      case Some(path) => wd0 = arg; wd0
    }
  }

  implicit def Relativizer[T](p: T)(implicit b: Path, f: T => RelPath): Path = b/f(p)
}

object PPrints{

  def lsSeqRepr(t: LsSeq) = pprint.Tree.Lazy { ctx =>
    val renderer = new Renderer(
      ctx.width, ctx.applyPrefixColor, ctx.literalColor, ctx.indentStep
    )
    val snippets = for (p <- t) yield {
      fansi.Str.join(renderer.rec(relPathRepr(p relativeTo t.base), 0, 0).iter.toStream:_*)
    }
    Iterator("\n") ++ FrontEndUtils.tabulate(snippets, FrontEndUtils.width)
  }


  def reprSection(s: String, cfg: pprint.Tree.Ctx): fansi.Str = {
    val validIdentifier = "([a-zA-Z_][a-zA-Z_0-9]+)".r

    if (validIdentifier.findFirstIn(s) == Some(s)){
      cfg.literalColor(''' + s)
    }else{
      cfg.literalColor(pprint.Util.literalize(s))
    }
  }

  def relPathRepr(p: ammonite.ops.RelPath) = pprint.Tree.Lazy(ctx =>
    Iterator(
      (Seq.fill(p.ups)("up") ++ p.segments.map(reprSection(_, ctx))).mkString("/")
    )
  )

  def pathRepr(p: ammonite.ops.Path) = pprint.Tree.Lazy(ctx =>
    Iterator("root") ++ p.segments.iterator.map("/" + reprSection(_, ctx))
  )

  def commandResultRepr(x: CommandResult) = pprint.Tree.Lazy(ctx =>
    x.chunks.iterator.flatMap { chunk =>
      val (color, s) = chunk match{
        case Left(s) => (ctx.literalColor, s)
        case Right(s) => (fansi.Color.Red, s)
      }
      Iterator("\n", color(new String(s.array)).render)
    }
  )

  implicit val defaultHighlightColor = {
    ammonite.runtime.tools.GrepResult.Color(
      fansi.Color.Blue ++ fansi.Back.Yellow,
      fansi.Color.Yellow
    )
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