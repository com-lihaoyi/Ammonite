package ammonite.shell

import ammonite.interp.api.InterpAPI
import ammonite.repl.api.ReplAPI
import ammonite.repl.ReplExtras.ReplAPIExtensions

/**
 * Created by haoyi on 9/1/15.
 */
object Configure {

  def apply(interp: InterpAPI, repl: ReplAPI, wd: => ammonite.ops.Path) = {
    if (scala.util.Properties.isWin) {
      repl.frontEnd() = ammonite.repl.FrontEnds.JLineWindows
      repl.colors = ammonite.repl.api.Colors.BLACKWHITE
    } else {
      repl.frontEnd() = ammonite.repl.AmmoniteFrontEnd(
        // TODO Add that back somehow
        // extraFilters = ammonite.shell.PathComplete.pathCompleteFilter(wd, repl.colors())
      )
    }

    repl.prompt = {
      sys.props("user.name") +
      "-" +
      wd.segments.toSeq.lastOption.getOrElse("") +
      "@ "
    }
  }
}
