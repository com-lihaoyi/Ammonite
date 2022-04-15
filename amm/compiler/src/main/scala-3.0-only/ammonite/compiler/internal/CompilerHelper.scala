package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.reporting.{Diagnostic, MessageRendering}
import dotty.tools.dotc.typer.FrontEnd

object CompilerHelper {
  def frontEndPhases = List(
    List(new FrontEnd)
  )
  def messageAndPos(messageRenderer: MessageRendering, diagnostic: Diagnostic)(implicit ctx: Context) =
    messageRenderer.messageAndPos(
      diagnostic.msg,
      diagnostic.pos,
      messageRenderer.diagnosticLevel(diagnostic)
    )
}
