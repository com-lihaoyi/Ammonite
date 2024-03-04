package ammonite.main
import java.security.Permission

/**
 * This security manager is assigned as default one when we run user scripts with the --watch option
 * This allows to trap sys.exit by throwing a special TrapExitException
 */
object TrapExitSecurityManager extends SecurityManager {

  override def checkExit(status: Int): Unit = throw new TrapExitException(status)

  override def checkPermission(perm: Permission): Unit = {}

  private class TrapExitException(status: Int) extends RuntimeException {
    override def toString: String = s"script exited with status $status"
    override def getStackTrace: Array[StackTraceElement] = Array.empty
  }
}
