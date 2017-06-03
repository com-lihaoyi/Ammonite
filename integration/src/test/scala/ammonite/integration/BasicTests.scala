package ammonite.integration

import utest._
import ammonite.ops._
import ammonite.ops.ImplicitWd._
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
  override def utestTruncateLength = 60000

  val tests = TestSuite {
    println("Running BasicTest")

    def execWithJavaOptsSet(name: RelPath, home: Path) = %%bash(
      executable,
      "--no-remote-logging",
      "-h",
      home,
      replStandaloneResources/name,
      JAVA_OPTS = "-verbose:class"
    )

    'hello{
      val evaled = exec('basic/"Hello.sc")
      assert(evaled.out.trim == "Hello World")
    }

    //make sure scripts with symbols in path names work fine
    'scriptWithSymbols {
      if (!Util.windowsPlatform){
        val dirAddr =
          pwd/'target/'test/'resources/'ammonite/'integration/'basic
        val weirdScriptName = "script%#.@*+叉燒.sc"
        val scriptAddr = dirAddr/weirdScriptName
        rm(scriptAddr)
        write(scriptAddr, """println("Script Worked!!")""")
        val evaled = %%bash(
          executable,
          "-s",
          scriptAddr
        )
        assert(evaled.out.trim == "Script Worked!!" && evaled.err.string.isEmpty)
      }
    }
    'scalacNotLoadedByCachedScripts{
      val tmpDir = tmp.dir()
      val evaled1 = execWithJavaOptsSet(
        'basic/"Print.sc",
        tmpDir
      )
      val evaled2 = execWithJavaOptsSet(
       'basic/"Print.sc",
        tmpDir
      )
      val count1 = substrCount(evaled1.out.trim, "scala.tools.nsc")
      val count2 = substrCount(evaled2.out.trim, "scala.tools.nsc")
      //These numbers might fail in future but basic point is to keep count2
      //very low whereas count1 will be inevitably bit higher
      assert(count1 > 10)
      assert(count2 < 5)
    }
    'fastparseNotLoadedByCachedScritps{
      val tmpDir = tmp.dir()
      val evaled1 = execWithJavaOptsSet(
        'basic/"Print.sc",
        tmpDir
      )
      assert(evaled1.out.trim.contains("fastparse"))

      val evaled2 = execWithJavaOptsSet(
        'basic/"Print.sc",
        tmpDir
        )
      assert(!evaled2.out.trim.contains("fastparse"))
    }


    'scriptInSomeOtherDir{
      val scriptAddr = tmp.dir()/"script.sc"
      rm(scriptAddr)
      write(scriptAddr, """println("Worked!!")""")
      val evaled = %% bash(
        executable,
        scriptAddr
        )
      assert(evaled.out.trim == "Worked!!" )
    }

    'complex {
      // Spire not published for 2.12
      if (!scala.util.Properties.versionNumberString.contains("2.12")) {
        val evaled = exec('basic / "Complex.sc")
        assert(evaled.out.trim.contains("Spire Interval [0, 10]"))
      }
    }


    'shell {
      // make sure you can load the example-predef.sc, have it pull stuff in
      // from ivy, and make use of `cd!` and `wd` inside the executed script.
      val res = %%bash(
        executable,
        "--predef-file",
        exampleBarePredef,
        "-c",
        """val x = wd
        |@
        |cd! 'amm/'src
        |@
        |println(wd relativeTo x)""".stripMargin,
        "-s"
      )

      val output = res.out.trim
      assert(output == "amm/src")
    }

    'source{
      // This fails on windows because for some reason the windows subprocess
      // interface eats the double-quotes inside the `-c` argument, even though
      // the argument is being passed programmatically and not through any shell =(
      //
      // For some reason this fails on travis/Scala2.10/Java7. I cannot reproduce
      // it locally on OSX/Scala2.10/Java8, but Scala2.10/Java7 is legacy anyway
      // so it's probably fine if this doesn't work.
      if (!Util.windowsPlatform && !scala.util.Properties.versionNumberString.contains("2.10")) {
        %%bash(
          executable,
          "-c",
          """val loc = source.load(new String().substring(_: Int))
            |val snip = loc.fileContent
            |  .lines
            |  .slice(loc.lineNum-15, loc.lineNum+15)
            |  .mkString("\n")
            |
            |assert(snip.contains("public String substring(int beginIndex)"))
          """.stripMargin
        )
      }
    }

    'classloaders{
      val evaled = exec('basic / "Resources.sc")
      assert(evaled.out.string.contains("1745"))
    }
    'testSilentScriptRunning{
      val evaled1 = exec('basic/"Hello.sc")
      // check Compiling Script is being printed

      assert(evaled1.err.string.contains("Compiling"))
      val evaled2 = execSilent('basic/"Hello.sc")
      // make sure with `-s` flag script running is silent
      assert(!evaled2.err.string.contains("Compiling"))
    }
    'testSilentRunningWithExceptions{
      val errorMsg = intercept[ShelloutException]{
        exec('basic/"Failure.sc")
      }.result.err.string

      assert(errorMsg.contains("not found: value x"))
    }
    'testSilentIvyExceptions{
      val errorMsg = intercept[ShelloutException]{
        exec('basic/"wrongIvyCordinates.sc")
      }.result.err.string


      assert(errorMsg.contains("Failed to resolve ivy dependencies"))
    }
    'testIvySnapshotNoCache{

      // test disabled on windows because sbt not available
      if (!Util.windowsPlatform && !scala.util.Properties.versionNumberString.contains("2.12")) {
        val buildRoot = pwd/'target/"some-dummy-library"
        cp.over(intTestResources/"some-dummy-library", buildRoot)
        val dummyScala = buildRoot/'src/'main/'scala/'dummy/"Dummy.scala"

        def publishJarAndRunScript(theThing: String) = {
          // 1. edit code
          write.over(dummyScala,
            s"""package dummy
              object Dummy{def thing="$theThing"}
           """.stripMargin)

          // 2. build & publish code locally
          %%("sbt", "+package", "+publishLocal")(buildRoot)

          // 3. use published artifact in a script
          val evaled = exec('basic/"ivyResolveSnapshot.sc")
          assert(evaled.out.string.contains(theThing))
        }

        publishJarAndRunScript("thing1")
        publishJarAndRunScript("thing2")
      }
    }

    // Most of the logic around main methods is tested in `MainTests.scala`
    // in our unit test suite, but test a few cases as integration tests
    // to make sure things work end-to-end
    'multiMain{
      'positiveArgs{
        val evaled = exec('basic/"MultiMain.sc", "functionB", "2", "foo")

        val out = evaled.out.string
        assert(out == ("Hello! foofoo ." + Util.newLine))
      }
      'specifyMain{
        val evaled = intercept[ShelloutException](exec('basic/"MultiMain.sc"))

        val out = evaled.result.err.string
        val expected = Util.normalizeNewlines(
          s"""Need to specify a subcommand to call when running MultiMain.sc
             |
                |Available subcommands:
             |
                |def mainA()
             |def functionB(i: Int, s: String, path: ammonite.ops.Path = $pwd)
             |""".stripMargin
        )
        assert(out.contains(expected))
      }
    }
  }
}
