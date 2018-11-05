package test.ammonite.ops

import ammonite.ops._
import utest._

object ShelloutTests extends TestSuite{
  val scriptFolder = pwd/'ops/'src/'test/'resources/'scripts

  val listCmd = if(scala.util.Properties.isWin) "dir" else "ls"

  val tests = Tests {
    'implicitWd{
      import ammonite.ops.ImplicitWd._
      'lines{
        val res = %%(listCmd, pwd/'ops/'src/'test/'resources/'testdata)
        Seq("File.txt", "folder1", "folder2").foreach(f =>
          assert(res.out.lines.exists(_.contains(f)))
        )
      }
      'string{
        val res = %%(listCmd, pwd/'ops/'src/'test/'resources/'testdata)
        "File.txt,folder1,folder2,".split(",").foreach(f =>
          assert(res.out.string.contains(f))
        )
      }
      'bytes{
        if(Unix()){
          val res = %%('echo, "abc")
          val listed = res.out.bytes
          //        assert(listed == "File.txt\nfolder\nfolder2\nFile.txt".getBytes)
          listed.toSeq
        }
      }
      'chained{
        assert(%%('git, 'init).out.string.contains("Reinitialized existing Git repository"))
        assert(%%('git, "init").out.string.contains("Reinitialized existing Git repository"))
        assert(%%(listCmd, pwd).out.string.contains("readme.md"))
      }
      'basicList{
        val files = List("readme.md", "build.sbt")
        val output = %%(listCmd, files).out.string
        assert(files.forall(output.contains))
      }
      'listMixAndMatch{
        val stuff = List("I", "am", "bovine")
        val result = %%('echo, "Hello,", stuff, "hear me roar")
        assert(result.out.string.contains("Hello, " + stuff.mkString(" ") + " hear me roar"))
      }
      'failures{
        val ex = intercept[ShelloutException]{ %%(listCmd, "does-not-exist") }
        val res: CommandResult = ex.result
        assert(
          res.exitCode != 0,
          res.err.string.contains("No such file or directory") || // unix
          res.err.string.contains("File Not Found") // win
        )
      }

      'filebased{
        if(Unix()){
          assert(%%(scriptFolder/'echo, 'HELLO).out.lines.mkString == "HELLO")

          val res: CommandResult =
            %%(root/'bin/'bash, "-c", "echo 'Hello'$ENV_ARG", ENV_ARG=123)

          assert(res.out.string.trim == "Hello123")
        }
      }
      'filebased2{
        if(Unix()){
          val res = %%('which, 'echo)
          val echoRoot = Path(res.out.string.trim)
          assert(echoRoot == root/'bin/'echo)

          assert(%%(echoRoot, 'HELLO).out.lines == Seq("HELLO"))
        }
      }

      'envArgs{
        if(Unix()){
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

    }
    'workingDirectory{
      implicit var wd = pwd
      val listed1 = %%(listCmd)

      wd /= up

      val listed2 = %%(listCmd)

      assert(listed2 != listed1)
    }
    'customWorkingDir{
      if(Unix()){
        val res1 = %.ls()(pwd) // explicitly
        // or implicitly
        import ammonite.ops.ImplicitWd._
        val res2 = %ls
      }
    }
    'fileCustomWorkingDir - {
      if(Unix()){
        val output = %%.apply(scriptFolder/'echo_with_wd, 'HELLO)(root/'usr)
        assert(output.out.lines == Seq("HELLO /usr"))
      }
    }
  }
}
