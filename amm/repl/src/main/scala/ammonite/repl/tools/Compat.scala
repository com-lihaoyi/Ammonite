package ammonite.repl.tools

import sourcecode.Compat.Context

object Compat{
  def showCode(c: Context)(tree: c.Tree): String = {
    c.universe.showCode(tree)
  }
  def companion(c: Context)(sym: c.universe.ClassSymbol): c.Tree = {
    import c.universe._
    q"classOf[${sym.companion}]"
  }
}