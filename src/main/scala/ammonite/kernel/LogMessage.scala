package ammonite.kernel

import java.io.{StringWriter, PrintWriter}

sealed trait LogMessage {
  val msg: String
}

object LogMessage {
  def fromThrowable(t: Throwable): LogError = {
    val sw = new StringWriter();
    val pw = new PrintWriter(sw);
    t.printStackTrace(pw);
    val msg = sw.toString();
    sw.close()
    pw.close()
    LogError(msg)
  }
}

final case class LogError(msg: String) extends LogMessage
final case class LogWarning(msg: String) extends LogMessage
final case class LogInfo(msg: String) extends LogMessage
