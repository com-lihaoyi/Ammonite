package ammonite.repl.frontend

import jline.console.completer.Completer

import scala.tools.nsc.interpreter._

class Completer(previousImportBlock: => String,
                compilerComplete: (Int, String) => (Int, Seq[String]))
                extends jline.console.completer.Completer {
  def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
    val buf   = if (_buf == null) "" else _buf
    val prevImports = previousImportBlock
    val prev = prevImports + "\n" + "object Foo{\n"
    import collection.JavaConversions._
    val (completionBase, completions) = compilerComplete(
      cursor + prev.length,
      prev + buf + "\n}"
    )
    candidates.addAll(completions)
    completionBase - prev.length
  }
}
