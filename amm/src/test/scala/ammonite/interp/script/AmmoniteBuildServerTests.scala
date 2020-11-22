package ammonite.interp.script

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture

import ch.epfl.scala.bsp4j.{Diagnostic => BDiagnostic, Position => BPosition, _}
import utest._

import scala.collection.JavaConverters._
import scala.compat.java8.FutureConverters
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.meta.internal.semanticdb.TextDocument

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

    "don't delete directories in target" - {
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

        classDirectory = os.Path(Paths.get(new URI(scalacOptionsItem.getClassDirectory)))

        customDir = classDirectory / "dont-remove"
        _ = os.makeDir.all(customDir)
        _ = assert(os.isDir(customDir))

        _ <- runner.compile(StatusCode.OK)

        _ = assert(os.isDir(customDir))

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics.isEmpty)

      } yield ()
    }

    "caching" - {
      val runner = new BspScriptRunner(
        Seq("main", "lib").map(name => wd / "import-file" / s"$name.sc"): _*
      )

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

        _ <- runner.compile(StatusCode.OK)

        classDirectory = os.Path(Paths.get(new URI(scalacOptionsItem.getClassDirectory)))

        _ = {
          val clsFile = classDirectory / "ammonite" / "$file" / "import$minusfile" / "main.class"
          assert(os.isDir(classDirectory))
          assert(os.isFile(clsFile))
          os.remove.all(classDirectory)
          assert(!os.exists(classDirectory))
        }

        _ <- runner.compile(StatusCode.OK)

        _ = {
          // Cache hit, nothing should be re-compiled / written to disk.
          // We may be even more cautious in the future though, and re-compile
          // and write things again if ever stuff is missing on disk. We'll
          // have to evolve the logic in this test then.
          assert(!os.exists(classDirectory))
        }

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics.isEmpty)

      } yield ()
    }

    "cache clean-up" - {

      val tmpDir = os.temp.dir(prefix = "ammonite-bsp-tests-")

      val script1 = tmpDir / "script1.sc"
      val script2 = tmpDir / "script2.sc"
      os.write(script1, "val value = 1")
      os.write(script2, "val value = 2")

      val script1Uri = script1.toNIO.toUri.toASCIIString
      val script2Uri = script2.toNIO.toUri.toASCIIString

      val runner = new BspScriptRunner(tmpDir, Seq(script1, script2))

      val f = for {
        scalacOptionsItems <- runner.init()

        scalacOptionsItem1 = scalacOptionsItems
          .find(_.getTarget.getUri == script1Uri)
          .getOrElse(sys.error(s"scalac options item not found for $script1Uri"))
        scalacOptionsItem2 = scalacOptionsItems
          .find(_.getTarget.getUri == script2Uri)
          .getOrElse(sys.error(s"scalac options item not found for $script2Uri"))

        _ <- runner.compile(StatusCode.OK, script1Uri)
        _ <- runner.compile(StatusCode.OK, script2Uri)

        classDirectory1 = os.Path(Paths.get(new URI(scalacOptionsItem1.getClassDirectory)))
        classDirectory2 = os.Path(Paths.get(new URI(scalacOptionsItem2.getClassDirectory)))

        _ = {
          val clsFile1 = classDirectory1 / "ammonite" / "$file" / "script1.class"
          assert(os.isDir(classDirectory1))
          assert(os.isFile(clsFile1))
        }

        _ = {
          val clsFile2 = classDirectory2 / "ammonite" / "$file" / "script2.class"
          assert(os.isDir(classDirectory2))
          assert(os.isFile(clsFile2))
        }

        // Recompiling script1 with a cache miss.
        // Should trigger a clean-up, removing the script2 stuff.
        _ = {
          os.remove(script2)
          os.remove.all(classDirectory1)
        }

        _ = os.write.over(script1, "val value = 3")
        _ <- runner.compile(StatusCode.OK, script1Uri)

        _ = assert(os.isDir(classDirectory1))
        _ = assert(!os.isDir(classDirectory2))

        _ = {
          // Write back script2, and update script1.
          // script1 class directory should be wiped when compiling script2,
          // as the content of script1 changed.
          os.write(script2, "val value = 2")
          os.write.over(script1, "val value = 4")
        }

        _ <- runner.compile(StatusCode.OK, script2Uri)

        _ = assert(!os.isDir(classDirectory1))

      } yield ()

      f.onComplete { _ =>
        os.remove.all(tmpDir)
      }

      f
    }

    "router imports" - {

      val tmpDir = os.temp.dir(prefix = "ammonite-bsp-tests-")

      val script = tmpDir / "script.sc"
      os.write(
        script,
        """@main(doc = "Main stuff")
          |def doMainStuff() = {
          |  thingOne()
          |  otherTing()
          |}
          |
          |@main(doc = "Do the first thingy")
          |def thingOne() = {
          |  println("1.")
          |}
          |
          |@main(doc = "Do the other thing")
          |def otherTing() = {
          |  println("hello")
          |}
          |""".stripMargin
      )

      val scriptUri = script.toNIO.toUri.toASCIIString

      val runner = new BspScriptRunner(tmpDir, Seq(script))

      val f = for {
        scalacOptionsItems <- runner.init()

        scalacOptionsItem1 = scalacOptionsItems
          .find(_.getTarget.getUri == scriptUri)
          .getOrElse(sys.error(s"scalac options item not found for $scriptUri"))

        _ <- runner.compile(StatusCode.OK, scriptUri)

        _ = os.write.over(script, "val value = zz")
        _ <- runner.compile(StatusCode.ERROR, scriptUri)

        _ = {
          val expectedDiagnostics = List(
            new BDiagnostic(
              new Range(new BPosition(0, 12), new BPosition(0, 14)),
              "not found: value zz"
            )
          )
          expectedDiagnostics.foreach(_.setSeverity(DiagnosticSeverity.ERROR))

          val diagnostics = runner.diagnostics()

          assert(diagnostics == expectedDiagnostics)
        }

      } yield ()

      f.onComplete { _ =>
        os.remove.all(tmpDir)
      }

      f
    }

    "build changed notification" - {

      val tmpDir = os.temp.dir(prefix = "ammonite-bsp-tests-")

      val script = tmpDir / "script.sc"
      os.write(script, "val value = 1")

      val scriptUri = script.toNIO.toUri.toASCIIString

      val runner = new BspScriptRunner(tmpDir, Seq(script))

      val f = for {
        scalacOptionsItems <- runner.init()

        scalacOptionsItem = scalacOptionsItems
          .find(_.getTarget.getUri == scriptUri)
          .getOrElse(sys.error(s"scalac options item not found for $scriptUri"))

        _ <- runner.compile(StatusCode.OK)
        _ = runner.client.clearDidChangeNotifications()

        _ = os.write.over(script, "val value = 3")
        _ <- runner.compile(StatusCode.OK)

        notifications0 = runner.client.didChangeNotifications()
        _ = assert(notifications0.isEmpty)

        _ = os.write.over(script, "import sys.process._")
        _ <- runner.compile(StatusCode.OK)

        notifications1 = runner.client.didChangeNotifications()
        _ = assert(notifications1.isEmpty)

        _ = os.write.over(script, "import $ivy.`com.chuusai::shapeless:2.3.3`; val value = 3")
        _ <- runner.compile(StatusCode.OK)

        notifications = runner.client.didChangeNotifications()
        _ = runner.client.clearDidChangeNotifications()

        _ = os.write.over(
          script,
          "import $ivy.`com.chuusai::shapeless:2.3.3`; import sys.process._"
        )
        _ <- runner.compile(StatusCode.OK)

        notifications2 = runner.client.didChangeNotifications()
        _ = assert(notifications2.isEmpty)
      } yield {

        val notification = notifications match {
          case Seq() => sys.error("Got no notification")
          case Seq(notif) => notif
          case _ => sys.error(s"Got too many notifications: $notifications")
        }

        assert(notification.getKind == BuildTargetEventKind.CHANGED)
        assert(notification.getTarget.getUri == scriptUri)
      }

      f.onComplete { _ =>
        os.remove.all(tmpDir)
      }

      f
    }

    "dash in name" - {
      val runner = new BspScriptRunner(wd / "dash-1" / "main-1.sc")

      for {
        List(scalacOptionsItem) <- runner.init()

        _ <- runner.compile(StatusCode.OK)

        _ = {
          val classDir = Paths.get(new URI(scalacOptionsItem.getClassDirectory))
          val srcDir = os.Path(classDir.resolve("../src").normalize)
          val relPath = os.Path(Paths.get(new URI(scalacOptionsItem.getTarget.getUri)))
            .relativeTo(wd)
          val expectedScalaFile = srcDir /
            "ammonite" /
            "$file" /
            "dash-1" /
            "main-1.scala"
          assert(os.isFile(expectedScalaFile))
        }

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics.isEmpty)

      } yield ()
    }

    "errored" - {
      "compilation" - {
        val runner = new BspScriptRunner(wd / "error" / "foo.sc")

        val expectedDiagnostics = List(
          new BDiagnostic(
            new Range(new BPosition(0, 0), new BPosition(0, 2)),
            "not found: value aa"
          ),
          new BDiagnostic(
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

      "import file" - {
        val runner = new BspScriptRunner(wd / "error" / "invalid-import-file.sc")

        val expectedDiagnostics = List(
          new BDiagnostic(
            new Range(new BPosition(1, 7), new BPosition(1, 27)),
            "Cannot resolve $file import: ./error/nope/nope/nope.sc"
          )
        )
        expectedDiagnostics.foreach(_.setSeverity(DiagnosticSeverity.ERROR))

        for {
          _ <- runner.init()
          _ <- runner.compile(StatusCode.ERROR)

          diagnostics = runner.diagnostics()
          _ = diagnostics.foreach { diag =>
            diag.setMessage(diag.getMessage.replace(wd.toString, "."))
          }
          _ = assert(diagnostics == expectedDiagnostics)
        } yield ()
      }

      "import file and compilation" - {
        val runner = new BspScriptRunner(wd / "error" / "error-and-invalid-import-file.sc")

        val expectedDiagnostics = List(
          new BDiagnostic(
            new Range(new BPosition(0, 7), new BPosition(0, 27)),
            "Cannot resolve $file import: ./error/nope/nope/nope.sc"
          ),
          new BDiagnostic(
            new Range(new BPosition(2, 0), new BPosition(2, 2)),
            "not found: value zz"
          )
        )
        expectedDiagnostics.foreach(_.setSeverity(DiagnosticSeverity.ERROR))

        for {
          _ <- runner.init()
          _ <- runner.compile(StatusCode.ERROR)

          diagnostics = runner.diagnostics()
          _ = diagnostics.foreach { diag =>
            diag.setMessage(diag.getMessage.replace(wd.toString, "."))
          }
          _ = assert(diagnostics == expectedDiagnostics)
        } yield ()
      }
    }

    "multi" - {
      val dir = wd / "multi"
      val scriptNames = Seq("main", "lib1", "lib2", "lib3")
      val runner = new BspScriptRunner(scriptNames.map(n => dir / s"$n.sc"): _*)

      val expectedClassPath = List(
        s"io/circe/circe-core_$sbv/0.12.3/circe-core_$sbv-0.12.3.jar",
        s"io/circe/circe-generic_$sbv/0.12.3/circe-generic_$sbv-0.12.3.jar",
        s"io/circe/circe-parser_$sbv/0.12.3/circe-parser_$sbv-0.12.3.jar",
        s"io/circe/circe-numbers_$sbv/0.12.3/circe-numbers_$sbv-0.12.3.jar",
        s"io/circe/circe-jawn_$sbv/0.12.3/circe-jawn_$sbv-0.12.3.jar"
      ).map("https/repo1.maven.org/maven2/" + _) ++ List(
        "https/jitpack.io/com/github/jupyter/jvm-repr/0.4.0/jvm-repr-0.4.0.jar"
      )

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

          def classDir(name: String): String = {
            val uri = (dir / s"$name.sc").toNIO.toUri.toASCIIString
            val options = scalacOptionsItems
              .find(_.getTarget.getUri == uri)
              .getOrElse(sys.error(s"no scalac options found for $uri"))
            options.getClassDirectory
          }

          val expectedDirsInClasspath = Seq("lib1", "lib2").map(classDir)
          val wdUri = wd.toNIO.toUri.toASCIIString
          for (p <- expectedDirsInClasspath)
            assert(classPath.contains(p))
        }

        _ <- runner.compile(StatusCode.OK)

        _ = {
          val events = runner.client.taskEvents()
          val messages = events.collect {
            case TestBuildClient.TaskEvent.Start(params)
                if params.getDataKind == TaskDataKind.COMPILE_TASK =>
              params.getMessage
            case TestBuildClient.TaskEvent.Finish(params)
                if params.getDataKind == TaskDataKind.COMPILE_REPORT =>
              params.getMessage
          }
          val expectedMessages = Seq(
            "Compiling multi/lib1.sc",
            "Compiled multi/lib1.sc",
            "Compiling multi/lib2.sc",
            "Compiled multi/lib2.sc",
            "Compiling multi/lib3.sc",
            "Compiled multi/lib3.sc",
            "Compiling multi/main.sc",
            "Compiled multi/main.sc"
          )
          assert(messages == expectedMessages)
        }

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics.isEmpty)

      } yield ()
    }

    "semanticdb" - {
      val scriptPath = os.RelPath("semdb/main.sc")
      val otherScriptPath = os.RelPath("semdb/other.sc")
      val runner = new BspScriptRunner(wd / scriptPath, wd / otherScriptPath)

      val expectedClassPath = List(
        s"com/softwaremill/sttp/client/core_$sbv/2.0.6/core_$sbv-2.0.6.jar",
        s"com/softwaremill/sttp/model/core_$sbv/1.0.2/core_$sbv-1.0.2.jar"
      ).map("https/repo1.maven.org/maven2/" + _)

      val otherScriptUri = (wd / otherScriptPath).toNIO.toUri.toASCIIString

      def semanticDb(
        scalacOptionsItem: ScalacOptionsItem,
        scriptPath: os.RelPath
      ): TextDocument = {
        import scala.meta.internal.semanticdb._

        val classDirectory = os.Path(Paths.get(new URI(scalacOptionsItem.getClassDirectory)))
        val semanticDbPath = classDirectory /
          "META-INF" /
          "semanticdb" /
          scriptPath.segments.init /
          s"${scriptPath.segments.last}.semanticdb"
        assert(os.isFile(semanticDbPath))

        val docs = TextDocuments.parseFrom(os.read.bytes(semanticDbPath))
        val doc = docs.documents match {
          case Seq(doc0) => doc0
          case _ =>
            throw new Exception(
              s"Invalid number of text documents in semanticdb: " +
                docs.documents.length
            )
        }

        val scriptContent = os.read(wd / scriptPath)
        assert(doc.text == scriptContent)

        doc
      }

      def checkMainScript(scalacOptionsItem: ScalacOptionsItem, scriptPath: os.RelPath): Unit = {
        import scala.meta.internal.semanticdb._

        val doc = semanticDb(scalacOptionsItem, scriptPath)

        val quickRequestOccurrence = new SymbolOccurrence(
          Some(new Range(3, 0, 3, 12)),
          "sttp/client/SttpApi#quickRequest.",
          SymbolOccurrence.Role.REFERENCE
        )

        assert(doc.occurrences.contains(quickRequestOccurrence))
      }

      def checkOtherScript(scalacOptionsItem: ScalacOptionsItem, scriptPath: os.RelPath): Unit = {
        import scala.meta.internal.semanticdb._

        val doc = semanticDb(scalacOptionsItem, scriptPath)

        val listOccurrence = new SymbolOccurrence(
          Some(new Range(0, 11, 0, 15)),
          "scala/collection/immutable/List.",
          SymbolOccurrence.Role.REFERENCE
        )

        assert(doc.occurrences.contains(listOccurrence))
      }

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
        otherScalacOptionsItem = scalacOptionsItems
          .find(_.getTarget.getUri == otherScriptUri)
          .getOrElse(
            throw new Exception(
              "scalac options item not found for " +
                runner.expectedBuildTargetId
            )
          )

        _ <- runner.compile(StatusCode.OK)

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics.isEmpty)

        _ = checkMainScript(scalacOptionsItem, scriptPath)

        _ <- runner.compile(StatusCode.OK, otherScriptUri)
        _ = checkOtherScript(otherScalacOptionsItem, otherScriptPath)

      } yield ()
    }

    "single comment with no line feed" - {
      val runner = new BspScriptRunner(wd / "comment" / "main.sc")

      for {
        _ <- runner.init()

        _ <- runner.compile(StatusCode.OK)

        diagnostics = runner.diagnostics()
        _ = assert(diagnostics.isEmpty)

      } yield ()
    }


  }

  class BspScriptRunner(wd: os.Path, script: Seq[os.Path]) {

    def this(script: os.Path*) =
      this(wd, script)

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

    def compile(expectedStatusCode: StatusCode, scriptUri: String = null): Future[Unit] =
      for {
        compileResp <- server
          .buildTargetCompile(
            new CompileParams(Seq(
              Option(scriptUri)
                .map(new BuildTargetIdentifier(_))
                .getOrElse(expectedBuildTargetId)
            ).asJava)
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
