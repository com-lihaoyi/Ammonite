package ammonite.main

import ammonite.util.{ImportData, Imports}

private[main] trait DefaultsScalaVersionSpecific {
  val replImports = Imports(
    ImportData("""ammonite.repl.ReplBridge.value.{
      codeColorsImplicit,
      tprintColorsImplicit,
      show,
      typeOf
    }""")
  )
}
