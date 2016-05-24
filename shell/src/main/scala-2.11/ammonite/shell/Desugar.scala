package ammonite.shell

import org.scalafmt.{ScalafmtRunner, ScalafmtStyle}
import sourcecode.Compat._
import scala.language.experimental.macros

object desugar{
  def transformer(c: Context)(expr: c.Expr[Any]): c.Expr[String] = {
    import c.universe._
    c.Expr[String](q"ammonite.shell.desugar.impl(${showCode(expr.tree)})")
  }

  def impl(s: String) = {
    ammonite.repl.frontend.Highlighter.defaultHighlight(
      org.scalafmt.Scalafmt.format(
        s,
        style = ScalafmtStyle.default.copy(
          continuationIndentCallSite = 2,
          continuationIndentDefnSite = 2,
          configStyleArguments = false
        ),
        runner = ScalafmtRunner.statement
      ).get.toVector,
      Console.BLUE,
      Console.GREEN,
      Console.GREEN,
      Console.YELLOW,
      Console.RESET
    ).mkString
  }

  def apply(expr: Any): String = macro transformer
}