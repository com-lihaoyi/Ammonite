package ammonite.interp.script

import java.util.concurrent.CompletableFuture
import ch.epfl.scala.bsp4j._
import scala.collection.JavaConverters._

private[script] trait DummyBuildServerImplems extends BuildServer with ScalaBuildServer {

  def buildShutdown(): CompletableFuture[Object] =
    CompletableFuture.completedFuture(null)

  def buildTargetResources(params: ResourcesParams): CompletableFuture[ResourcesResult] = {
    val items = params.getTargets.asScala.toList.map { target =>
      new ResourcesItem(target, List.empty[String].asJava)
    }
    val result = new ResourcesResult(items.asJava)
    CompletableFuture.completedFuture(result)
  }

  def buildTargetRun(params: RunParams): CompletableFuture[RunResult] = {
    val result = new RunResult(StatusCode.ERROR)
    result.setOriginId(params.getOriginId)
    CompletableFuture.completedFuture(result)
  }
  def buildTargetTest(params: TestParams): CompletableFuture[TestResult] = {
    val result = new TestResult(StatusCode.ERROR)
    result.setOriginId(params.getOriginId)
    CompletableFuture.completedFuture(result)
  }
  def buildTargetCleanCache(params: CleanCacheParams): CompletableFuture[CleanCacheResult] = {
    val result = new CleanCacheResult("", false)
    CompletableFuture.completedFuture(result)
  }

  def onBuildExit(): Unit = ()
  def onBuildInitialized(): Unit = ()

  def buildTargetScalaMainClasses(
    params: ScalaMainClassesParams
  ): CompletableFuture[ScalaMainClassesResult] = {
    val items = params.getTargets.asScala.map { target =>
      new ScalaMainClassesItem(target, List.empty[ScalaMainClass].asJava)
    }
    val result = new ScalaMainClassesResult(items.asJava)
    CompletableFuture.completedFuture(result)
  }
  def buildTargetScalaTestClasses(
    params: ScalaTestClassesParams
  ): CompletableFuture[ScalaTestClassesResult] = {
    val items = params.getTargets.asScala.map { target =>
      new ScalaTestClassesItem(target, List.empty[String].asJava)
    }
    val result = new ScalaTestClassesResult(items.asJava)
    CompletableFuture.completedFuture(result)
  }

}
