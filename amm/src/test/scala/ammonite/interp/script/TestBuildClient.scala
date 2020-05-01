package ammonite.interp.script

import java.util.concurrent.ConcurrentHashMap

import ch.epfl.scala.bsp4j.{Diagnostic => BDiagnostic, _}

import scala.collection.JavaConverters._

class TestBuildClient extends BuildClient {

  private val diagnostics0 =
    new ConcurrentHashMap[
      (BuildTargetIdentifier, TextDocumentIdentifier),
      Seq[BDiagnostic]
    ]

  def diagnostics(
    targetId: BuildTargetIdentifier,
    document: TextDocumentIdentifier
  ): Option[Seq[BDiagnostic]] =
    Option(diagnostics0.get((targetId, document)))

  def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit =
    diagnostics0.put(
      (params.getBuildTarget, params.getTextDocument),
      params.getDiagnostics.asScala.toVector
    )

  def onBuildLogMessage(params: LogMessageParams): Unit = ()
  def onBuildShowMessage(params: ShowMessageParams): Unit = ()
  def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ()
  def onBuildTaskFinish(params: TaskFinishParams): Unit = ()
  def onBuildTaskProgress(params: TaskProgressParams): Unit = ()
  def onBuildTaskStart(params: TaskStartParams): Unit = ()
}
