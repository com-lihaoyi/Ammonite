package test.ammonite.ops

import ammonite.ops._
import utest._
import utest.framework.TestSuite

object ShelloutTests extends TestSuite{
  val scriptFolder = cwd/'ops/'src/'test/'resources/'scripts
  val tests = TestSuite {
    'implicitWd{
      import ammonite.ops.ImplicitWd._
      'basic{
        val listed = (%%ls "ops/src/test/resources/testdata").out.toSet
        val expected = Set(
          "folder1", "folder2", "File.txt"
        )
        assert(listed == expected)
      }
      'chained{
        assert((%%git 'init).out.mkString.contains("Reinitialized existing Git repository"))
        assert((%%git "init").out.mkString.contains("Reinitialized existing Git repository"))
        assert((%%ls cwd).out.mkString.contains("readme.md"))
      }
      'basicList{
        val files = List("readme.md", "build.sbt")
        val output = (%%ls files).out.mkString
        assert(files.forall(output.contains))
      }
      'listMixAndMatch{
        val stuff = List("I", "am", "bovine")
        val result = %%echo("Hello,", stuff, "hear me roar")
        assert(result.out.mkString.contains("Hello, " + stuff.mkString(" ") + " hear me roar"))
      }
      'failures{
        intercept[RuntimeException]{ %%ls "does-not-exist" }
      }

      'filebased{


        assert(%%(scriptFolder/'echo, 'HELLO) == Seq("HELLO"))

        val res: CommandResult =
          %%(root/'bin/'bash, "-c", "echo 'Hello'$ENV_ARG", ENV_ARG=123)

        assert(res.mkString == "Hello123")
      }
      'filebased2{
        val echoRoot = Path(%%which 'echo mkString)
        assert(echoRoot == root/'bin/'echo)

        assert(%%(echoRoot, 'HELLO) == Seq("HELLO"))
      }

      'envArgs{
        val res0 = %%bash("-c", "echo \"Hello$ENV_ARG\"", ENV_ARG=12)
        assert(res0 == Seq("Hello12"))

        val res1 = %%bash("-c", "echo \"Hello$ENV_ARG\"", ENV_ARG=12)
        assert(res1 == Seq("Hello12"))

        val res2 = %%bash("-c", "echo 'Hello$ENV_ARG'", ENV_ARG=12)
        assert(res2 == Seq("Hello$ENV_ARG"))

        val res3 = %%bash("-c", "echo 'Hello'$ENV_ARG", ENV_ARG=123)
        assert(res3 == Seq("Hello123"))
      }

    }
    'workingDirectory{
      implicit var wd = cwd
      val listed1 = %%ls

      wd /= up

      val listed2 = %%ls

      assert(listed2 != listed1)
    }
    'customWorkingDir{
      val res1 = %.ls()(cwd) // explicitly
      // or implicitly
      import ammonite.ops.ImplicitWd._
      val res2 = %ls
    }
    'fileCustomWorkingDir{
      val output = %%.apply(scriptFolder/'echo_with_wd, 'HELLO)(root/'usr)
      assert(output == Seq("HELLO /usr"))
    }
  }
}
