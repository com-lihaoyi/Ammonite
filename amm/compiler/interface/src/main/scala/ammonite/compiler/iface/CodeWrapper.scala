package ammonite.compiler.iface

import ammonite.util.{Imports, Name}
import ammonite.util.Util.CodeSource

abstract class CodeWrapper {
  def wrapperPath: Seq[Name] = Nil
  def apply(
    code: String,
    source: CodeSource,
    imports: Imports,
    printCode: String,
    indexedWrapper: Name,
    extraCode: String
  ): (String, String, Int)
}
