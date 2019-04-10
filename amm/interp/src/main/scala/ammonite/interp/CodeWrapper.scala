package ammonite.interp

import ammonite.util._
import ammonite.util.Util.{CodeSource, normalizeNewlines}

trait CodeWrapper{
  def wrapperPath: Seq[Name] = Nil
  def apply(
    code: String,
    source: CodeSource,
    imports: Imports,
    printCode: String,
    indexedWrapperName: Name,
    extraCode: String
  ): (String, String, Int)
}
object CodeWrapper extends CodeWrapper{
  private val userCodeNestingLevel = 1
  def apply(
    code: String,
    source: CodeSource,
    imports: Imports,
    printCode: String,
    indexedWrapperName: Name,
    extraCode: String
  ) = {
    import source.pkgName
    val top = normalizeNewlines(s"""
package ${pkgName.head.encoded}
package ${Util.encodeScalaSourcePath(pkgName.tail)}
$imports

object ${indexedWrapperName.backticked}{\n"""
      )
      val bottom = normalizeNewlines(s"""\ndef $$main() = { $printCode }
  override def toString = "${indexedWrapperName.encoded}"
  $extraCode
}
""")

    (top, bottom, userCodeNestingLevel)
  }
}
