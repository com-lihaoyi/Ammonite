
val sess = ammonite.tools.ShellSession()
import sess._
import ammonite.ops._

frontEnd() = ammonite.repl.frontend.AmmoniteFrontEnd(
  ammonite.tools.PathComplete.pathCompleteFilter(wd, colors())
)

prompt.bind(
  Console.RESET + sys.props("user.name") +
  Console.MAGENTA + "@" +
  Console.RESET + wd.segments.lastOption.getOrElse("").toString +
  Console.MAGENTA + "> "
)

@
