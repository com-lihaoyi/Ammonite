package ammonite.tools

import ammonite.repl.frontend.ReplAPI

/**
 * Created by haoyi on 9/1/15.
 */
object Configure {
  def apply(repl: ReplAPI, wd: => ammonite.ops.Path) = {
    repl.frontEnd() = ammonite.repl.frontend.AmmoniteFrontEnd(
      ammonite.tools.PathComplete.pathCompleteFilter(wd, repl.colors())
    )

    repl.prompt.bind(
      Console.RESET + sys.props("user.name") +
        Console.MAGENTA + "-" +
        Console.RESET + wd.segments.lastOption.getOrElse("").toString +
        Console.MAGENTA + "@ "
    )
  }
}
