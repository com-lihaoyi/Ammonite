package ammonite.interp.api

import scala.util.control.ControlThrowable

/**
  * Thrown to exit the REPL cleanly
  */
case class AmmoniteExit(value: Any) extends ControlThrowable
