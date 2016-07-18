package ammonite.tools

import sourcecode.Compat._
import scala.language.experimental.macros


class Desugared(s: String){
  override def toString() = s
}
object desugar{
  def transformer(c: Context)(expr: c.Expr[Any]): c.Expr[Desugared] = {
    import c.universe._
    c.Expr[Desugared](q"ammonite.frontend.ReplBridge.repl.Internal.desugar(${showCode(expr.tree)})")
  }

  def apply(expr: Any): Desugared = macro transformer
}