package ammonite.interp.script

import ammonite.compiler.{Compiler => AmmCompiler}

final case class ScriptCompileResult(
  diagnostics: Seq[Diagnostic],
  errorOrOutput: Either[String, Seq[AmmCompiler.Output]]
)
