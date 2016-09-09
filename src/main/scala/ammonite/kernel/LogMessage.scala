package ammonite.kernel

sealed trait LogMessage {
  val msg: String
}

final case class LogError(msg: String) extends LogMessage
final case class LogWarning(msg: String) extends LogMessage
final case class LogInfo(msg: String) extends LogMessage
