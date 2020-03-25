package ammonite.interp.script

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture

import ch.epfl.scala.bsp4j.{Position => BPosition, _}
import utest._

import scala.collection.JavaConverters._
import scala.compat.java8.FutureConverters
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AmmoniteBuildServerTests extends TestSuite {

  implicit class CompletableFutureOps[T](private val f: CompletableFuture[T]) extends AnyVal {
    def asScala: Future[T] =
      FutureConverters.toScala(f)
  }

  val scriptBase = os.pwd/'amm/'src/'test/'resources/'bsp

  val wd = os.temp.dir(deleteOnExit = true)

  // copying scripts to wd, so that the directories created under wd / .ammonite are shorter
  for (elem <- os.list(scriptBase))
    os.copy(elem, wd / elem.last, createFolders = true)

  val sbv = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")

  override def utestAfterAll(): Unit =
    os.remove.all(wd)

  val tests = Tests {

    "simple" - {
      val runner = new BspScriptRunner(wd / "simple" / "main.sc")

      val expectedClassPath = List(
        s"com/softwaremill/sttp/client/core_$sbv/2.0.6/core_$sbv-2.0.6.jar",
        s"com/softwaremill/sttp/model/core_$sbv/1.0.2/core_$sbv-1.0.2.jar"
      ).map("https/repo1.maven.org/maven2/" + _)

      for {
        List(scalacOptionsItem) <- runner.init()

        _ = {
          val classPath = scalacOptionsItem.getClasspath.asScala.toList
          for (p <- expectedClassPath)
            assert(classPath.exists(_.endsWith("/" + p)))
        }

        _ <- runner.compile(StatusCode.OK)

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics.isEmpty)

      } yield ()
    }

    "errored" - {
      val runner = new BspScriptRunner(wd / "error" / "foo.sc")

      val expectedDiagnostics = List(
        new Diagnostic(
          new Range(new BPosition(0, 0), new BPosition(0, 2)),
          "not found: value aa"
        ),
        new Diagnostic(
          new Range(new BPosition(3, 0), new BPosition(3, 2)),
          "not found: value zz"
        )
      )
      expectedDiagnostics.foreach(_.setSeverity(DiagnosticSeverity.ERROR))

      for {
        _ <- runner.init()

        _ <- runner.compile(StatusCode.ERROR)

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics == expectedDiagnostics)

      } yield ()
    }

    "multi" - {
      val dir = wd / "multi"
      val scriptNames = Seq("main", "lib1", "lib2")
      val runner = new BspScriptRunner(scriptNames.map(n => dir / s"$n.sc"): _*)

      val expectedClassPath = List(
        s"io/circe/circe-core_$sbv/0.12.3/circe-core_$sbv-0.12.3.jar",
        s"io/circe/circe-generic_$sbv/0.12.3/circe-generic_$sbv-0.12.3.jar",
        s"io/circe/circe-parser_$sbv/0.12.3/circe-parser_$sbv-0.12.3.jar",
        s"io/circe/circe-numbers_$sbv/0.12.3/circe-numbers_$sbv-0.12.3.jar",
        s"io/circe/circe-jawn_$sbv/0.12.3/circe-jawn_$sbv-0.12.3.jar"
      ).map("https/repo1.maven.org/maven2/" + _)

      for {
        scalacOptionsItems <- runner.init()
        scalacOptionsItem = scalacOptionsItems
          .find(_.getTarget == runner.expectedBuildTargetId)
          .getOrElse(
            throw new Exception(
              "scalac options item not found for " +
                runner.expectedBuildTargetId
            )
          )

        _ = scalacOptionsItems.foreach { item =>
          val classDir = Paths.get(new URI(item.getClassDirectory))
          val srcDir = os.Path(classDir.resolve("../src").normalize)
          val relPath = os.Path(Paths.get(new URI(item.getTarget.getUri))).relativeTo(wd)
          val expectedScalaFile = srcDir /
            "ammonite" /
            "$file" /
            relPath.segments.init /
            s"${relPath.last.stripSuffix(".sc")}.scala"
          assert(os.isFile(expectedScalaFile))
        }

        _ = {
          val classPath = scalacOptionsItem.getClasspath.asScala.toList
          for (p <- expectedClassPath)
            assert(classPath.exists(_.endsWith("/" + p)))
        }

        _ <- runner.compile(StatusCode.OK)

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics.isEmpty)

      } yield ()
    }

  }

  class BspScriptRunner(script: os.Path*) {

    val server = new AmmoniteBuildServer(initialScripts = script)

    val client = new TestBuildClient
    server.onConnectWithClient(client)

    private val rootUri = wd.toNIO.toUri.toASCIIString
    private val initializeParams = new InitializeBuildParams(
      "",
      "",
      "",
      rootUri,
      new BuildClientCapabilities(List("scala").asJava)
    )

    val scriptUri = script.head.toNIO.toUri.toASCIIString
    val expectedBuildTargetId = new BuildTargetIdentifier(scriptUri)
    private val expectedTargetIds = script.map { s =>
      val uri = s.toNIO.toUri.toASCIIString
      new BuildTargetIdentifier(uri)
    }

    private val expectedSourcesItems = script
      .map(_.toNIO.toUri.toASCIIString)
      .sorted
      .map { uri =>
        val targetId = new BuildTargetIdentifier(uri)
        new SourcesItem(
          targetId,
          List(new SourceItem(uri, SourceItemKind.FILE, false))
            .asJava
        )
      }

    def init(): Future[List[ScalacOptionsItem]] =
      for {
        initResp <- server.buildInitialize(initializeParams).asScala

        buildTargetsResp <- server.workspaceBuildTargets().asScala

        targetIds = buildTargetsResp.getTargets.asScala.toList.map(_.getId)
        _ = assert(targetIds.sortBy(_.getUri) == expectedTargetIds.sortBy(_.getUri))

        sourcesResp <- server
          .buildTargetSources(new SourcesParams(targetIds.asJava))
          .asScala
        _ = assert(
          sourcesResp.getItems.asScala.sortBy(_.getTarget.getUri) == expectedSourcesItems
        )

        scalacOptionsResp <- server
          .buildTargetScalacOptions(new ScalacOptionsParams(targetIds.asJava))
          .asScala
        _ = {
          assert(scalacOptionsResp.getItems.size == script.length)
          val item = scalacOptionsResp
            .getItems
            .asScala
            .find(_.getTarget == expectedBuildTargetId)
            .getOrElse {
              throw new Exception(
                s"scalac options item not found for $expectedBuildTargetId"
              )
            }
          val options = item.getOptions.asScala.toSet
          val classPath = item.getClasspath.asScala.toList
          val classDir = os.Path(Paths.get(new URI(item.getClassDirectory)))

          assert(options.contains("-Yrangepos"))
          assert(options.exists(_.startsWith("-P:semanticdb:sourceroot:")))
          assert(options.exists(_.startsWith("-P:semanticdb:targetroot:")))
          assert(classDir.startsWith(wd / ".ammonite"))
        }

      } yield scalacOptionsResp.getItems.asScala.toList

    def compile(expectedStatusCode: StatusCode): Future[Unit] =
      for {
        compileResp <- server
          .buildTargetCompile(
            new CompileParams(Seq(expectedBuildTargetId).asJava)
          )
          .asScala
        _ = assert(compileResp.getStatusCode == expectedStatusCode)
      } yield ()

    def diagnostics() =
      client
        .diagnostics(expectedBuildTargetId, new TextDocumentIdentifier(scriptUri))
        .getOrElse {
          throw new Exception(s"No diagnostics found for $expectedBuildTargetId / $scriptUri")
        }
  }

}
