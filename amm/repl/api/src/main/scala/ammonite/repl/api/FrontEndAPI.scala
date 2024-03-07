package ammonite.repl.api

import ammonite.interp.api.APIHolder

trait FrontEndAPI {
  def apply(name: String): FrontEnd
}

object FrontEndBridge extends APIHolder[FrontEndAPI]
