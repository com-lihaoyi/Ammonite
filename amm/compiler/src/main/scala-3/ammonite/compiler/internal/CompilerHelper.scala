package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.reporting.{Diagnostic, MessageRendering}
import dotty.tools.dotc.typer.TyperPhase

object CompilerHelper {
  def frontEndPhases = List(
    List(new Parser),
    List(new TyperPhase)
  )
  def messageAndPos(messageRenderer: MessageRendering, diagnostic: Diagnostic)(implicit
      ctx: Context
  ) =
    messageRenderer.messageAndPos(diagnostic)
}
