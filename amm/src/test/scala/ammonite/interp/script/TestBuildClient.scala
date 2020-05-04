package ammonite.interp.script

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}

import ch.epfl.scala.bsp4j.{Diagnostic => BDiagnostic, _}

import scala.collection.JavaConverters._

class TestBuildClient extends BuildClient {

  import TestBuildClient._

  private val diagnostics0 =
    new ConcurrentHashMap[
      (BuildTargetIdentifier, TextDocumentIdentifier),
      Seq[BDiagnostic]
    ]

  private val taskEvents0 = new ConcurrentLinkedQueue[TaskEvent]

  def diagnostics(
    targetId: BuildTargetIdentifier,
    document: TextDocumentIdentifier
  ): Option[Seq[BDiagnostic]] =
    Option(diagnostics0.get((targetId, document)))

  def taskEvents(): Seq[TaskEvent] =
    taskEvents0.iterator().asScala.toVector

  def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit =
    diagnostics0.put(
      (params.getBuildTarget, params.getTextDocument),
      params.getDiagnostics.asScala.toVector
    )

  def onBuildTaskStart(params: TaskStartParams): Unit =
    taskEvents0.add(TaskEvent.Start(params))
  def onBuildTaskProgress(params: TaskProgressParams): Unit =
    taskEvents0.add(TaskEvent.Progress(params))
  def onBuildTaskFinish(params: TaskFinishParams): Unit =
    taskEvents0.add(TaskEvent.Finish(params))

  def onBuildLogMessage(params: LogMessageParams): Unit = ()
  def onBuildShowMessage(params: ShowMessageParams): Unit = ()
  def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ()
}

object TestBuildClient {

  sealed abstract class TaskEvent extends Product with Serializable
  object TaskEvent {
    final case class Start(params: TaskStartParams) extends TaskEvent
    final case class Progress(params: TaskProgressParams) extends TaskEvent
    final case class Finish(params: TaskFinishParams) extends TaskEvent
  }

}
