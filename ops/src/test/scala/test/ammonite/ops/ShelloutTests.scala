package test.ammonite.ops

import ammonite.ops._
import utest._
import utest.framework.TestSuite


object ShelloutTests extends TestSuite{
  val tests = TestSuite {
    'basic{
      val listed = (%%ls "ops/src/test/resources/testdata").output.toSet
      val expected = Set(
        "folder1", "folder2", "File.txt"
      )
      assert(listed == expected)
    }
    'chained{
      assert((%%git 'init).output.mkString.contains("Reinitialized existing Git repository"))
      assert((%%git "init").output.mkString.contains("Reinitialized existing Git repository"))
      assert((%%ls cwd).output.mkString.contains("readme.md"))
    }
    'failures{
      intercept[RuntimeException]{ %%ls "does-not-exist" }
    }
  }
}
