package test.ammonite.ops

import utest._
import utest.framework.TestSuite
import ammonite.shell._

object ShelloutTests extends TestSuite{
  val tests = TestSuite {
    'basic{
      assert(
        (%ls).output.toSet ==
        Set("build.sbt", "core", "media", "project", "readme.md", "repl", "target"),
        (%ls "core/target/scala-2.11/test-classes/testdata").output.toSet ==
        Set("folder1", "folder2", "File.txt")
      )
    }
    'chained{
      assert((%git %branch).output.mkString.contains("master"))
    }
    'failures{
      intercept[RuntimeException]{ %ls "does-not-exist" }
    }
  }
}
