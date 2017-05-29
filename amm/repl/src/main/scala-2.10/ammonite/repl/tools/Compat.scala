package ammonite.repl.tools

import sourcecode.Compat.Context

object Compat{
  def showCode(c: Context)(tree: c.Tree): String = {
    tree.toString
  }
}