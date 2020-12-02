package ammonite.util

import java.util.{Map => JMap}

import ammonite.compiler.iface.{CodeSource, CodeWrapper, Imports}
import ammonite.util.InterfaceExtensions._
import ammonite.util.Name
import ammonite.util.Util.{entry, normalizeNewlines}

trait ScalaCodeWrapper extends CodeWrapper {

  final override def rawWrapperPath: Array[String] =
    wrapperPath.map(_.raw).toArray
  final override def wrap(
    code: String,
    source: CodeSource,
    imports: Imports,
    printCode: String,
    indexedWrapper: String,
    extraCode: String
  ): JMap.Entry[String, JMap.Entry[String, Integer]] = {
    val (top, bottom, nestingLevel) = apply(
      code,
      source,
      imports,
      printCode,
      Name(indexedWrapper),
      extraCode
    )
    entry(top, entry(bottom, nestingLevel))
  }

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