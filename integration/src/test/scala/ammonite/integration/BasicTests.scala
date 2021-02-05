package ammonite.integration

import utest._
import ammonite.util.Util
import TestUtils._
/**
 * Run a small number of scripts using the Ammonite standalone executable,
 * to make sure that this works. Otherwise it tends to break since the
 * standalone executable has a pretty different classloading environment
 * from the "run in SBT on raw class files" that the rest of the tests use.
 *
 * These are also the only tests that cover all the argument-parsing
 * and configuration logic inside, which the unit tests don't cover since
 * they call the REPL programmatically
 */
object BasicTests extends TestSuite{

  val tests = Tests {
    println("Running BasicTest")

    def execWithJavaOptsSet(name: os.RelPath, home: os.Path) = os.proc(
      executable,
      "--thin",
      "--no-remote-logging",
      "-h",
      home,
      replStandaloneResources/name
    ).call(
      env = Map("JAVA_OPTS" -> "-verbose:class"),
      stderr = os.Pipe
    )

    test("hello"){
      val evaled = exec(os.rel / 'basic/"Hello.sc")
      assert(evaled.out.trim == "Hello World")
    }

    //make sure scripts with symbols in path names work fine
    test("scriptWithSymbols"){
      if (!Util.windowsPlatform){
        val dirAddr =
          os.pwd/'target/'test/'resources/'ammonite/'integration/'basic
        val weirdScriptName = "script%#.@*+叉燒.sc"
        val scriptAddr = dirAddr/weirdScriptName
        os.remove.all(scriptAddr)
        os.write(scriptAddr, """println("Script Worked!!")""", createFolders = true)
        val evaled = os.proc(
          executable,
          "--thin",
          "-s",
          scriptAddr
          // Somehow this is being set of travis and causing weird errors/warnings
        ).call(env = Map("_JAVA_OPTIONS" -> null))
        assert(evaled.out.trim == "Script Worked!!" && evaled.err.string.isEmpty)
      }
    }
    test("scalacNotLoadedByCachedScripts"){
      val tmpDir = os.temp.dir()
      val evaled1 = execWithJavaOptsSet(
        os.rel/'basic/"Print.sc",
        tmpDir
      )
      val evaled2 = execWithJavaOptsSet(
        os.rel/'basic/"Print.sc",
        tmpDir
      )
      val compilerPackage =
        if (isScala2) "scala.tools.nsc"
        else "dotty.tools.dotc"
      val count1 = substrCount(evaled1.out.trim, compilerPackage)
      val count2 = substrCount(evaled2.out.trim, compilerPackage)
      //These numbers might fail in future but basic point is to keep count2
      //very low whereas count1 will be inevitably bit higher
      if (isScala2) {
        assert(count1 > 10)
        assert(count2 < 5)
      } else {
        assert(count1 > 100)
        assert(count2 < 15)
      }
    }
    test("fastparseNotLoadedByCachedScritps"){
      val parserPackage =
        if (isScala2) "fastparse"
        else "dotty.tools.dotc.parsing"
      val tmpDir = os.temp.dir()
      val evaled1 = execWithJavaOptsSet(
        os.rel/'basic/"Print.sc",
        tmpDir
      )
      assert(evaled1.out.trim.contains(parserPackage))

      val evaled2 = execWithJavaOptsSet(
        os.rel/'basic/"Print.sc",
        tmpDir
        )
      assert(!evaled2.out.trim.contains(parserPackage))
    }


    test("scriptInSomeOtherDir"){
      val scriptAddr = os.temp.dir()/"script.sc"
      os.remove.all(scriptAddr)
      os.write(scriptAddr, """println("Worked!!")""")
      val evaled = os.proc(
        executable,
        "--thin",
        scriptAddr
      ).call()
      assert(evaled.out.trim == "Worked!!" )
    }

    test("complex"){
      // Spire not published for 2.12
      if (scala.util.Properties.versionNumberString.contains("2.11")) {
        val evaled = exec(os.rel / 'basic / "Complex.sc")
        assert(evaled.out.trim.contains("Spire Interval [0, 10]"))
      }
    }


    def shellTest() = {
      // make sure you can load the example-predef.sc, have it pull stuff in
      // from ivy, and make use of `cd!` and `wd` inside the executed script.
      val res = os.proc(
        executable,
        "--thin",
        "--no-home-predef",
        "--predef",
        exampleBarePredef,
        "-c",
        """val x = wd
        |@
        |cd! "amm"/"src"
        |@
        |println(wd relativeTo x)""".stripMargin,
        "-s"
      ).call()

      val output = res.out.trim

      if (!Util.windowsPlatform)
        // seems the script is run only until the first '@' on Windows
        assert(output == "amm/src")
    }
    test("shell"){
      // FIXME In Scala 3.0.0-M1, etting errors like
      //   java.lang.AssertionError: assertion failed:
      //     duplicate type CC#31508; previous was type CC#31500
      if (isScala2)
        shellTest()
      else
        "Disabled in Scala 3"
    }

    // Ensure we can load the source code of the built-in Java standard library
    test("source"){
      // This fails on windows because for some reason the windows subprocess
      // interface eats the double-quotes inside the `-c` argument, even though
      // the argument is being passed programmatically and not through any shell =(
      //
      // For some reason this fails on travis/Scala2.10/Java7. I cannot reproduce
      // it locally on OSX/Scala2.10/Java8, but Scala2.10/Java7 is legacy anyway
      // so it's probably fine if this doesn't work.
      //
      // Also disabled on Java 9 due to unavailability of Java lib sources
      if (!Util.windowsPlatform && !Util.java9OrAbove && isScala2) {
        os.proc(
          executable,
          "--thin",
          "-c",
          """val loc = source.load(new String().substring(_: Int))
            |val snip = Predef.augmentString(loc.fileContent)
            |  .lines
            |  .slice(loc.lineNum-15, loc.lineNum+15)
            |  .mkString("\n")
            |
            |assert(snip.contains("public String substring(int beginIndex)"))
          """.stripMargin
        ).call()
      }
    }

    // Ensure we can load the source code of external libraries, which needs to
    // get pulled down together with the library code when you `import $ivy`
    test("sourceExternal"){
      // Re-enable when source support is added in Scala 3
      if (isScala2)
        exec(os.rel/'basic / "SourceDownload.sc")
      else
        "Disabled in Scala 3"
    }

    test("classloaders"){
      val evaled = exec(os.rel / 'basic / "Resources.sc")
      assert(evaled.out.string.contains("1745"))
    }
    test("testSilentScriptRunning"){
      val evaled1 = exec(os.rel / 'basic/"Hello.sc")
      // check Compiling Script is being printed

      assert(evaled1.err.string.contains("Compiling"))
      val evaled2 = execSilent(os.rel / 'basic/"Hello.sc")
      // make sure with `-s` flag script running is silent
      assert(!evaled2.err.string.contains("Compiling"))
    }
    test("testSilentRunningWithExceptions"){
      val errorMsg = intercept[os.SubprocessException]{
        exec(os.rel / 'basic/"Failure.sc")
      }.result.err.string

      val expected =
        if (isScala2) "not found: value x"
        else "Not found: x"
      assert(errorMsg.contains(expected))
    }
    test("testSilentIvyExceptions"){
      val errorMsg = intercept[os.SubprocessException]{
        exec(os.rel / 'basic/"wrongIvyCordinates.sc")
      }.result.err.string


      assert(errorMsg.contains("Failed to resolve ivy dependencies"))
    }
    test("testIvySnapshotNoCache"){

      // test disabled on windows because sbt not available
      if (!Util.windowsPlatform) {
        val buildRoot = os.pwd/'target/"some-dummy-library"
        os.copy.over(intTestResources/"some-dummy-library", buildRoot)
        val dummyScala = buildRoot/'src/'main/'scala/'dummy/"Dummy.scala"
        // using the same home to share the ivymap cache across runs
        val home = os.temp.dir()

        val sbv = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")
        val previous = os.home / ".ivy2" / "local" / "com.lihaoyi" / ("some-dummy-library_" + sbv)
        os.remove.all(previous)

        def publishJarAndRunScript(
          theThing: String,
          script: String,
          version: String,
          firstRun: Boolean = false
        ): Unit = {
          // 1. edit code
          os.write.over(
            dummyScala,
            s"""package dummy
              object Dummy{def thing="$theThing"}
           """.stripMargin,
            createFolders = true
          )

          // 2. build & publish code locally
          val compatScalaVersion = {
            val likelyVersion =
              if (scalaVersion.startsWith("3."))
                // should be the right 2.13 version
                scala.util.Properties.versionNumberString
              else
                scalaVersion
            if (likelyVersion.contains("-bin-")) {
              val next = likelyVersion.take(likelyVersion.indexOf("-bin-"))
              val components = next.split('.')
              (components.init :+ (components.last.toInt - 1).max(0).toString).mkString(".")
            } else
              likelyVersion
          }
          os.proc("sbt", "-J-Xmx1g", "-batch", "-no-colors", "publishLocal").call(
            env = Map(
              "SCALA_VERSION" -> compatScalaVersion,
              "FIRST_RUN" -> s"$firstRun",
              "VERSION" -> version
            ),
            cwd = buildRoot
          )

          // 3. use published artifact in a script
          val evaled = execWithHome(home, os.rel / 'basic / script)
          assert(evaled.out.string.contains(theThing))
        }

        publishJarAndRunScript("thing1", "ivyResolveSnapshot1.sc", "0.1-SNAPSHOT", firstRun = true)
        // if ever the artifact list is cached in the first run, things will fail in the second
        // (as the snapshot artifact doesn't have the same dependencies)
        publishJarAndRunScript("thing2", "ivyResolveSnapshot2.sc", "0.1-SNAPSHOT")

        publishJarAndRunScript("thing1", "ivyResolveItv1.sc", "0.2.1", firstRun = true)
        // if ever the artifact list is cached in the first run, things will fail in the second
        publishJarAndRunScript("thing2", "ivyResolveItv2.sc", "0.2.2")
      }
    }

    // Most of the logic around main methods is tested in `MainTests.scala`
    // in our unit test suite, but test a few cases as integration tests
    // to make sure things work end-to-end
    test("multiMain"){
      def positiveArgsTest() = {
        val evaled = exec(os.rel / 'basic/"MultiMain.sc", "functionB", "2", "foo")

        val out = evaled.out.string
        assert(out == ("Hello! foofoo ." + Util.newLine))
      }
      def specifyMainTest() = {
        val evaled = intercept[os.SubprocessException](exec(os.rel / 'basic/"MultiMain.sc"))

        val out = evaled.result.err.string
        val expected = Util.normalizeNewlines(
          s"""Need to specify a sub command: mainA, functionB""".stripMargin
        )
        assert(out.contains(expected))
      }
      test("positiveArgs"){
        if (isScala2) positiveArgsTest()
        else "Disabled in Scala 3"
      }
      test("specifyMain"){
        if (isScala2) specifyMainTest()
        else "Disabled in Scala 3"
      }
    }

    test("BSP"){
      val jsonrpc = """{"jsonrpc": "2.0", "id": 1, "method": "build/shutdown", "params": null}"""
        .getBytes("UTF-8")
      val input = Array(
        s"Content-Length: ${jsonrpc.length}",
        "\r\n" * 2
      ).flatMap(_.getBytes("UTF-8")) ++ jsonrpc
      val res = os.proc(TestUtils.executable, "--bsp").call(
        stdin = input
      )
      assert(res.exitCode == 0)
    }
  }
}
