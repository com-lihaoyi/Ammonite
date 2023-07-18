package ammonite.interp.script

import java.util.concurrent.CompletableFuture
import ch.epfl.scala.bsp4j._
import scala.collection.JavaConverters._

private[script] trait DummyBuildServerImplems extends BuildServer with ScalaBuildServer {
  override def buildTargetDependencyModules(dmp: DependencyModulesParams): CompletableFuture[DependencyModulesResult] =
    CompletableFuture.completedFuture(new DependencyModulesResult(List.empty.asJava))

  override def buildTargetOutputPaths(opp: OutputPathsParams): CompletableFuture[OutputPathsResult] =
    CompletableFuture.completedFuture(new OutputPathsResult(List.empty.asJava))

  override def buildTargetResources(params: ResourcesParams): CompletableFuture[ResourcesResult] = {
    val items = params.getTargets.asScala.toList.map { target =>
      new ResourcesItem(target, List.empty[String].asJava)
    }
    val result = new ResourcesResult(items.asJava)
    CompletableFuture.completedFuture(result)
  }

  override def buildTargetRun(params: RunParams): CompletableFuture[RunResult] = {
    val result = new RunResult(StatusCode.ERROR)
    result.setOriginId(params.getOriginId)
    CompletableFuture.completedFuture(result)
  }

  override def buildTargetTest(params: TestParams): CompletableFuture[TestResult] = {
    val result = new TestResult(StatusCode.ERROR)
    result.setOriginId(params.getOriginId)
    CompletableFuture.completedFuture(result)
  }

  override def buildTargetScalaMainClasses(
    params: ScalaMainClassesParams
  ): CompletableFuture[ScalaMainClassesResult] = {
    val items = params.getTargets.asScala.map { target =>
      new ScalaMainClassesItem(target, List.empty[ScalaMainClass].asJava)
    }
    val result = new ScalaMainClassesResult(items.asJava)
    CompletableFuture.completedFuture(result)
  }

  override def buildTargetScalaTestClasses(
    params: ScalaTestClassesParams
  ): CompletableFuture[ScalaTestClassesResult] = {
    val items = params.getTargets.asScala.map { target =>
      new ScalaTestClassesItem(target, List.empty[String].asJava)
    }
    val result = new ScalaTestClassesResult(items.asJava)
    CompletableFuture.completedFuture(result)
  }

  override def debugSessionStart(dsp: DebugSessionParams): CompletableFuture[DebugSessionAddress] =
    CompletableFuture.completedFuture(new DebugSessionAddress(""))

  override def onBuildExit(): Unit = ()
  override def onBuildInitialized(): Unit = ()

  override def workspaceReload(): CompletableFuture[Object] =
    CompletableFuture.completedFuture(new {})
}
