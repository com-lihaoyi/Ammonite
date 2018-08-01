package ammonite

import ammonite.runtime.APIHolder

trait TestReplApi {
  def message: String
}

object TestReplBridge extends APIHolder[TestReplApi]
