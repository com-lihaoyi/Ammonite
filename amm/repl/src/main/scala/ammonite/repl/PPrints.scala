package ammonite.repl

import ammonite.ops.{CommandResult, LsSeq}
import pprint.Renderer

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

