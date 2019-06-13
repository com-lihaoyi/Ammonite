package ammonite.repl

import ammonite.ops.{CommandResult, LsSeq}
import ammonite.runtime.History
import ammonite.runtime.tools.GrepResult
import ammonite.util.Util
import pprint.Renderer

object PPrints{
  val replPPrintHandlers: PartialFunction[Any, pprint.Tree] = {
    case x: ammonite.ops.LsSeq => PPrints.lsSeqRepr(x)
    case x: os.Path => PPrints.pathRepr(x)
    case x: os.RelPath => PPrints.relPathRepr(x)
    case x: ammonite.ops.Path => PPrints.pathRepr(os.Path(x.toString))
    case x: ammonite.ops.RelPath => PPrints.relPathRepr(os.RelPath(x.toString))
    case x: ammonite.ops.CommandResult => PPrints.commandResultRepr(x)
    case t: History => pprint.Tree.Lazy(ctx => Iterator(t.mkString(Util.newLine)))
    case t: GrepResult => pprint.Tree.Lazy(ctx => Iterator(GrepResult.grepResultRepr(t, ctx)))
    case t: scala.xml.Elem => pprint.Tree.Lazy(_ => Iterator(t.toString))
  }
  def lsSeqRepr(t: LsSeq) = pprint.Tree.Lazy { ctx =>
    val renderer = new Renderer(
      ctx.width, ctx.applyPrefixColor, ctx.literalColor, ctx.indentStep
    )
    val snippets = for (p <- t) yield {
      fansi.Str.join(
        renderer.rec(relPathRepr(os.RelPath(p.relativeTo(t.base).toString)), 0, 0)
                .iter
                .toStream:_*
      )
    }
    Iterator(Util.newLine) ++ FrontEndUtils.tabulate(snippets, FrontEndUtils.width)
  }


  def reprSection(s: String, cfg: pprint.Tree.Ctx): fansi.Str = {
    val validIdentifier = "([a-zA-Z_][a-zA-Z_0-9]+)".r

    if (validIdentifier.findFirstIn(s) == Some(s)){
      cfg.literalColor('\'' + s)
    }else{
      cfg.literalColor(pprint.Util.literalize(s))
    }
  }

  def relPathRepr(p: os.RelPath) = pprint.Tree.Lazy(ctx =>
    Iterator(
      (Seq.fill(p.ups)("up") ++ p.segments.map(reprSection(_, ctx))).mkString("/")
    )
  )

  def pathRepr(p: os.Path) = pprint.Tree.Lazy(ctx =>
    Iterator("root") ++ p.segments.map("/" + reprSection(_, ctx))
  )

  def commandResultRepr(x: CommandResult) = pprint.Tree.Lazy(ctx =>
    x.chunks.iterator.flatMap { chunk =>
      val (color, s) = chunk match{
        case Left(s) => (ctx.literalColor, s)
        case Right(s) => (fansi.Color.Red, s)
      }
      Iterator(Util.newLine, color(new String(s.array)).render)
    }
  )
}

