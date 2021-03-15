package test.ammonite.ops

import ammonite.ops._
import utest._

object ShelloutTests extends TestSuite{
  val scriptFolder = pwd/'ops/'src/'test/'resources/'scripts

  val tests = Tests {
    test("implicitWd"){
      import ammonite.ops.ImplicitWd._
      test("lines"){
        val res = %%('ls, "ops/src/test/resources/testdata")
        assert(res.out.lines == Seq("File.txt", "folder1", "folder2"))
      }
      test("string"){
        val res = %%('ls, "ops/src/test/resources/testdata")
        assert(res.out.string == "File.txt\nfolder1\nfolder2\n")
      }
      test("bytes"){
        if(Unix()){
          val res = %%('echo, "abc")
          val listed = res.out.bytes
          //        assert(listed == "File.txt\nfolder\nfolder2\nFile.txt".getBytes)
          listed.toSeq
        }
      }
      test("chained"){
        assert(%%('git, 'init).out.string.contains("Reinitialized existing Git repository"))
        assert(%%('git, "init").out.string.contains("Reinitialized existing Git repository"))
        assert(%%('ls, pwd).out.string.contains("readme.md"))
      }
      test("basicList"){
        val files = List("readme.md", "build.sbt")
        val output = %%('ls, files).out.string
        assert(files.forall(output.contains))
      }
      test("listMixAndMatch"){
        val stuff = List("I", "am", "bovine")
        val result = %%('echo, "Hello,", stuff, "hear me roar")
        assert(result.out.string.contains("Hello, " + stuff.mkString(" ") + " hear me roar"))
      }
      test("failures"){
        val ex = intercept[ShelloutException]{ %%('ls, "does-not-exist") }
        val res: CommandResult = ex.result
        assert(
          res.exitCode != 0,
          res.err.string.contains("No such file or directory")
        )
      }

      test("filebased"){
        if(Unix()){
          assert(%%(scriptFolder/'echo, 'HELLO).out.lines.mkString == "HELLO")

          val res: CommandResult =
            %%(root/'bin/'bash, "-c", "echo 'Hello'$ENV_ARG", ENV_ARG=123)

          assert(res.out.string.trim == "Hello123")
        }
      }
      test("filebased2"){
        if(Unix()){
          val res = %%('which, 'echo)
          val echoRoot = Path(res.out.string.trim)
          assert(echoRoot == root/'bin/'echo || echoRoot == root/'usr/'bin/'echo)

          assert(%%(echoRoot, 'HELLO).out.lines == Seq("HELLO"))
        }
      }

      test("envArgs"){
        val res0 = %%('bash, "-c", "echo \"Hello$ENV_ARG\"", ENV_ARG=12)
        assert(res0.out.lines == Seq("Hello12"))

        val res1 = %%('bash, "-c", "echo \"Hello$ENV_ARG\"", ENV_ARG=12)
        assert(res1.out.lines == Seq("Hello12"))

        val res2 = %%('bash, "-c", "echo 'Hello$ENV_ARG'", ENV_ARG=12)
        assert(res2.out.lines == Seq("Hello$ENV_ARG"))

        val res3 = %%('bash, "-c", "echo 'Hello'$ENV_ARG", ENV_ARG=123)
        assert(res3.out.lines == Seq("Hello123"))
      }

    }
    test("workingDirectory"){
      implicit var wd = pwd
      val listed1 = %%('ls)

      wd /= up

      val listed2 = %%('ls)

      assert(listed2 != listed1)
    }
    test("customWorkingDir"){
      val res1 = %.ls()(pwd) // explicitly
      // or implicitly
      import ammonite.ops.ImplicitWd._
      val res2 = %ls
    }
    test("fileCustomWorkingDir"){
      if(Unix()){
        val output = %%.apply(scriptFolder/'echo_with_wd, 'HELLO)(root/'usr)
        assert(output.out.lines == Seq("HELLO /usr"))
      }
    }
  }
}
