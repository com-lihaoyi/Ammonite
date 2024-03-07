package ammonite.interp.script

import ammonite.compiler.iface.Compiler.Output

final case class ScriptCompileResult(
    diagnostics: Seq[Diagnostic],
    errorOrOutput: Either[String, Seq[Output]]
)
