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

  def wrapCode(
      codeSource: CodeSource,
      indexedWrapperName: Name,
      code: String,
      printCode: String,
      imports: Imports,
      extraCode: String,
      markScript: Boolean
  ) = {

    // we need to normalize topWrapper and bottomWrapper in order to ensure
    // the snippets always use the platform-specific newLine
    val extraCode0 =
      if (markScript) extraCode + "/*</generated>*/"
      else extraCode
    val (topWrapper, bottomWrapper, userCodeNestingLevel) =
      apply(code, codeSource, imports, printCode, indexedWrapperName, extraCode0)
    val (topWrapper0, bottomWrapper0) =
      if (markScript) (topWrapper + "/*<script>*/", "/*</script>*/ /*<generated>*/" + bottomWrapper)
      else (topWrapper, bottomWrapper)
    val importsLen = topWrapper0.length

    (topWrapper0 + code + bottomWrapper0, importsLen, userCodeNestingLevel)
  }

}
