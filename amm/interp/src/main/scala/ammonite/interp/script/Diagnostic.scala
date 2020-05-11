package ammonite.interp.script

final case class Diagnostic(
  severity: String,
  start: Position,
  end: Position,
  message: String
)
