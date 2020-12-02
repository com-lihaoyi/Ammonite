package ammonite.compiler

import ammonite.compiler.iface.{CodeSource, Imports}
import ammonite.util.InterfaceExtensions._
import ammonite.util.{Name, ScalaCodeWrapper, Util}
import ammonite.util.Util.normalizeNewlines

object ObjectCodeWrapper extends ScalaCodeWrapper{
  private val userCodeNestingLevel = 1
  def apply(
    code: String,
    source: CodeSource,
    imports: Imports,
    printCode: String,
    indexedWrapperName: Name,
    extraCode: String
  ) = {
    val pkgName = source.pkgName
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
