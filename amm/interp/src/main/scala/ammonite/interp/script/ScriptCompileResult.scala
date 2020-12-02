package ammonite.interp.script

import ammonite.compiler.iface.Compiler

final case class ScriptCompileResult(
  diagnostics: Seq[Diagnostic],
  errorOrOutput: Either[String, Seq[Compiler.Output]]
)
