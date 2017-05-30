package ammonite.repl.tools

import sourcecode.Compat.Context

object Compat{
  def showCode(c: Context)(tree: c.Tree): String = {
    tree.toString
  }
  def companion(c: Context)(sym: c.universe.ClassSymbol): c.Tree = {
    import c.universe._
    q"_root_.java.lang.Class.forName(${sym.fullName.toString})"
  }
}