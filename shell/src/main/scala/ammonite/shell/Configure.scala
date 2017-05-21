package ammonite.shell

import ammonite.ops.{CommandResult, LsSeq}
import ammonite.repl.{FrontEndUtils, ReplAPI}

/**
 * Created by haoyi on 9/1/15.
 */
object Configure {
  val pprintHandlers: PartialFunction[Any, pprint.Tree] = {
    case x: LsSeq => PPrints.lsSeqRepr(x)
    case x: ammonite.ops.Path => PPrints.pathRepr(x)
    case x: ammonite.ops.RelPath => PPrints.relPathRepr(x)
    case x: CommandResult => PPrints.commandResultRepr(x)
  }
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


    repl.pprinter() = repl.pprinter().copy(
      additionalHandlers = pprintHandlers orElse repl.pprinter().additionalHandlers
    )
  }
}
