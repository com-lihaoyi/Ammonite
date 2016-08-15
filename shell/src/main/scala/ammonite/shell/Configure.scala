package ammonite.shell

import ammonite.frontend.ReplAPI

/**
 * Created by haoyi on 9/1/15.
 */
object Configure {
  def apply(repl: ReplAPI, wd: => ammonite.ops.Path) = {
    repl.frontEnd() = ammonite.frontend.AmmoniteFrontEnd(
      ammonite.shell.PathComplete.pathCompleteFilter(wd, repl.colors())
    )

    repl.prompt.bind(
      sys.props("user.name") +
      "-" +
      wd.segments.lastOption.getOrElse("") +
      "@ "
    )
  }
}
