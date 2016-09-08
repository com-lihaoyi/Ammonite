package ammonite.util

import reflect.internal.util.Position

sealed trait LogMessage {
  val message: String
  val position: Position
}

final case class Info(message: String, position: Position) extends LogMessage

final case class Warning(message: String, position: Position) extends LogMessage

final case class Error(message: String, position: Position) extends LogMessage
