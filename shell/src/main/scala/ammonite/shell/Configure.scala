package ammonite.shell

import ammonite.interp.api.InterpAPI
import ammonite.repl.api.ReplAPI

/**
 * Created by haoyi on 9/1/15.
 */
object Configure {

  def apply(interp: InterpAPI, repl: ReplAPI, wd: => ammonite.ops.Path) = {
    if (scala.util.Properties.isWin) {
      repl.frontEnd() = ammonite.repl.FrontEnds.JLineWindows
      interp.colors() = ammonite.util.Colors.BlackWhite
    } else {
      repl.frontEnd() = ammonite.repl.AmmoniteFrontEnd(
        ammonite.shell.PathComplete.pathCompleteFilter(wd, interp.colors())
      )
    }

    repl.prompt.bind(
      sys.props("user.name") +
      "-" +
      wd.segments.toSeq.lastOption.getOrElse("") +
      "@ "
    )
  }
}
