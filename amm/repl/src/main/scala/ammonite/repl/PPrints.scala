package ammonite.repl

import ammonite.repl.api.History
import ammonite.runtime.tools.GrepResult
import ammonite.util.Util
import pprint.Renderer

object PPrints {
  def replPPrintHandlers(width: => Int): PartialFunction[Any, pprint.Tree] = {
//    case x: os.Path => PPrints.pathRepr(x)
//    case x: os.RelPath => PPrints.relPathRepr(x)
    case t: History => pprint.Tree.Lazy(ctx => Iterator(t.mkString(Util.newLine)))
    case t: GrepResult => pprint.Tree.Lazy(ctx => Iterator(GrepResult.grepResultRepr(t, ctx)))
    case t: scala.xml.Elem => pprint.Tree.Lazy(_ => Iterator(t.toString))
  }

  def reprSection(s: String, cfg: pprint.Tree.Ctx): fansi.Str = {
    val validIdentifier = "([a-zA-Z_][a-zA-Z_0-9]+)".r

    if (validIdentifier.findFirstIn(s) == Some(s)) {
      cfg.literalColor('\'' + s)
    } else {
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

}
