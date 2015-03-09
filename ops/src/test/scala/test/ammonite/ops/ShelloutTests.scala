package test.ammonite.ops

import ammonite.ops._
import utest._
import utest.framework.TestSuite


object ShelloutTests extends TestSuite{
  val tests = TestSuite {
    'basic{
      val listed = (%ls).output.toSet
      val expected = Set(
        "build.sbt", "ops", "pprint", "tools",
        "project", "readme", "readme.md", "repl", "target"
      )
      assert(
        listed == expected,
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
