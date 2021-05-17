package ammonite.compiler.test

import dotty.tools.dotc.{Compiler => DottyCompiler}

object CompilerTestExtensions {
  implicit class DottyCompilerExtensions(private val self: DottyCompiler) extends AnyVal {
    // scala.tools.nsc.Global in Scala 2 has that.
    // We only add it here so that code calling that compiles, and
    // make sure this method is not called at the end when the
    // Scala 3 amm-compiler module is used.
    def useOffsetPositions: Boolean = ???
  }
}
