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
  println("StandaloneTests")
  val tests = TestSuite {

    'hello{
      val evaled = exec('basic/"Hello.sc")
      assert(evaled.out.trim == "Hello World")
    }

    //make sure scripts with symbols in path names work fine
    'scriptWithSymbols {
      if (!Util.windowsPlatform){
        val dirAddr =
          cwd/'target/'test/'resources/'ammonite/'integration/'basic
        val weirdScriptName = "script%#.@*+叉燒.sc"
        val scriptAddr = dirAddr/weirdScriptName
        rm(scriptAddr)
        write(scriptAddr, """println("Script Worked!!")""")
        val evaled = %%bash(
          executable,
          scriptAddr
          )
        assert(evaled.out.trim == "Script Worked!!" && evaled.err.string.isEmpty)
      }
    }

    'scriptInSomeOtherDir{
      val scriptAddr = tmp.dir()/"script.sc"
      rm(scriptAddr)
      write(scriptAddr, """println("Worked!!")""")
      val evaled = %% bash(
        executable,
        scriptAddr
        )
      assert(evaled.out.trim == "Worked!!" && evaled.err.string.isEmpty)
    }

    'complex {
      val evaled = exec('basic / "Complex.sc")
      assert(evaled.out.trim.contains("Spire Interval [0, 10]"))
    }


    'shell {
      // make sure you can load the example-predef.sc, have it pull stuff in
      // from ivy, and make use of `cd!` and `wd` inside the executed script.
      val res = %% bash(
        executable,
        "--predef-file",
        exampleBarePredef,
        "-c",
        """val x = wd
        |@
        |cd! 'amm/'src
        |@
        |println(wd relativeTo x)""".stripMargin
      )

      val output = res.out.trim
      assert(output == "amm/src")
    }

    'classloaders{
      val evaled = exec('basic / "Resources.sc")
      assert(evaled.out.string.contains("1745"))
    }

    'playframework- {
      if (!Util.windowsPlatform) {
        if (scalaVersion.startsWith("2.11.") && javaVersion.startsWith("1.8.")){
          val evaled = exec('basic/"PlayFramework.sc")
          assert(evaled.out.string.contains("Hello bar"))
        }
      }
    }

    'main{
      'single{
        val evaled = exec('basic/"Main.sc")
        val out = evaled.out.string
        assert(out.contains("Hello! 1"))
      }
      'multiple{
        'positiveNoArgs{
          val evaled = exec('basic/"MultiMain.sc", "mainA")
          val out = evaled.out.string
          assert(out == "Hello! 1" + Util.newLine)
        }
        'positiveArgs{
          val evaled = exec('basic/"MultiMain.sc", "functionB", "2", "foo")
          val out = evaled.out.string
          assert(out == "Hello! foofoo ." + Util.newLine)
        }
        'specifyMain{
          val evaled = intercept[ShelloutException]{exec('basic/"MultiMain.sc")}.result
          val out = evaled.err.string
          val expected = Util.normalizeNewlines(
            """Need to specify a main method to call when running MultiMain.sc
              |
              |Available main methods:
              |
              |def mainA()
              |def functionB(i: Int, s: String, path: ammonite.ops.Path)
              |""".stripMargin
          )
          assert(out == expected)
        }
        'cantFindMain{
          val evaled = intercept[ShelloutException]{
            exec('basic/"MultiMain.sc", "doesntExist")
          }.result
          val out = evaled.err.string
          val expected = Util.normalizeNewlines(
            """Unable to find method: doesntExist
              |
              |Available main methods:
              |
              |def mainA()
              |def functionB(i: Int, s: String, path: ammonite.ops.Path)
              |""".stripMargin
          )
          assert(out == expected)
        }
      }
    }

    'args{
      'full{
        val evaled = exec('basic/"Args.sc", "3", "Moo", (cwd/'omg/'moo).toString)
        assert(evaled.out.string.contains("Hello! MooMooMoo omg/moo."))
      }
      'default{
        val evaled = exec('basic/"Args.sc", "3", "Moo")
        assert(evaled.out.string.contains("Hello! MooMooMoo ."))
      }
      'tooFew{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "3")
        }.result.err.string

        assert(errorMsg.contains(
          Util.normalizeNewlines(
            """The following arguments failed to be parsed:
              |(s: String) was missing
              |expected arguments: (i: Int, s: String, path: ammonite.ops.Path)"""
              .stripMargin
          )
        ))
      }
      'tooMany{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "3", "4", "5", "6")
        }.result.err.string

        assert(errorMsg.contains(
          Util.normalizeNewlines(
            """Too many args were passed to this script: "6"
              |expected arguments: (i: Int, s: String, path: ammonite.ops.Path)""".stripMargin
          )
        ))
      }
      'cantParse{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "foo", "moo")
        }.result.err.string

        val exMsg = """java.lang.NumberFormatException: For input string: "foo""""
        assert(errorMsg.contains(
          Util.normalizeNewlines(
            s"""The following arguments failed to be parsed:
               |(i: Int) failed to parse input "foo" with $exMsg
               |expected arguments: (i: Int, s: String, path: ammonite.ops.Path)"""
              .stripMargin
          )
        ))
        // Ensure we're properly truncating the random stuff we don't care about
        // which means that the error stack that gets printed is short-ish
        assert(errorMsg.lines.length < 12)

      }
    }
    'http{
      'shorten {
        val res = exec('basic / "HttpApi.sc", "shorten", "https://www.github.com")
        assert(res.out.trim.startsWith("https://git.io")) 
      }
      'releases{
        val res = exec('basic / "HttpApi.sc", "listReleases", "lihaoyi/Ammonite")
        assert(res.out.trim.contains("0.7.0"))
        assert(res.out.trim.contains("0.4.0"))
      }
    }
  }
}
