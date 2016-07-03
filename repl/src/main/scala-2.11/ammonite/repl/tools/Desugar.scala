package ammonite.repl.tools

import sourcecode.Compat._
import scala.language.experimental.macros


class Desugared(s: String){
  override def toString() = s
}
object desugar{
  def transformer(c: Context)(expr: c.Expr[Any]): c.Expr[Desugared] = {
    import c.universe._
    c.Expr[Desugared](q"ammonite.repl.tools.desugar.impl(${showCode(expr.tree)})")
  }

  def impl(s: String)(implicit colors: ammonite.repl.util.CodeColors): Desugared = {

    new Desugared(
      ammonite.repl.frontend.Highlighter.defaultHighlight(
        s.toVector,
        colors.comment,
        colors.`type`,
        colors.literal,
        colors.keyword,
        fansi.Attr.Reset
      ).mkString
    )
  }

  def apply(expr: Any): Desugared = macro transformer
}