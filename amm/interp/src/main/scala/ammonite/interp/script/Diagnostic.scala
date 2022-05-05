package ammonite.interp.script

import ammonite.util.Position

final case class Diagnostic(
  severity: String,
  start: Position,
  end: Position,
  message: String
)
