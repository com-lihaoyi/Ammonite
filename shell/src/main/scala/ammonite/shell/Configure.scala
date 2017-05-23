package ammonite.shell

import ammonite.repl.ReplAPI

/**
 * Created by haoyi on 9/1/15.
 */
object Configure {

  def apply(repl: ReplAPI, wd: => ammonite.ops.Path) = {
    if (scala.util.Properties.isWin) {
      repl.frontEnd() = ammonite.repl.FrontEnd.JLineWindows
      repl.colors() = ammonite.util.Colors.BlackWhite
    } else {
      repl.frontEnd() = ammonite.repl.AmmoniteFrontEnd(
        ammonite.shell.PathComplete.pathCompleteFilter(wd, repl.colors())
      )
    }

    repl.prompt.bind(
      sys.props("user.name") +
      "-" +
      wd.segments.lastOption.getOrElse("") +
      "@ "
    )
  }
}
