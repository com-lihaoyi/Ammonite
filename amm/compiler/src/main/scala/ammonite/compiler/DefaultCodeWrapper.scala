package ammonite.compiler

import ammonite.compiler.iface.CodeWrapper
import ammonite.util._
import ammonite.util.Util.{CodeSource, normalizeNewlines}

object DefaultCodeWrapper extends CodeWrapper {
  private val userCodeNestingLevel = 1
  def apply(
      code: String,
      source: CodeSource,
      imports: Imports,
      printCode: String,
      indexedWrapperName: Name,
      extraCode: String
  ) = {
    val top = CodeWrapper.packageDirectives(source.pkgName) + normalizeNewlines(s"""$imports

object ${indexedWrapperName.backticked}{\n""")
    val bottom = normalizeNewlines(s"""\ndef $$main() = { $printCode }
  override def toString = "${indexedWrapperName.encoded}"
  $extraCode
}
""")

    (top, bottom, userCodeNestingLevel)
  }
}
