package test.ammonite.ops

import ammonite.ops._
import utest._
import utest.framework.TestSuite


object ShelloutTests extends TestSuite{
  val tests = TestSuite {
    'basic{
      assert(
        (%ls).output.toSet ==
        Set(
          "build.sbt", "ops", "pprint", "tools", "media",
          "project", "readme.md", "repl", "target"
        ),
        (%ls "ops/target/scala-2.11/test-classes/testdata").output.toSet ==
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
