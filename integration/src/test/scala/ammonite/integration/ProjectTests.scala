package ammonite.integration

import ammonite.integration.TestUtils._
import ammonite.ops.ImplicitWd._
import ammonite.ops._
import ammonite.util.Util
import utest._

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
object ProjectTests extends TestSuite{

  val tests = Tests {
    println("Running ProjectTest")

    'playframework- {
      if (!Util.windowsPlatform) {
        if (scalaVersion.startsWith("2.11.") && javaVersion.startsWith("1.8.")){
          val evaled = exec('basic/"PlayFramework.sc")
          assert(evaled.out.string.contains("Hello bar"))
        }
      }
    }

    'spark - {
      // Note that this script screws up if you try to run it within SBT! It has to
      // be run as an integration test, or via `sbt amm/test:assembly && amm/target/amm`
      if (!Util.windowsPlatform) {
        if (scalaVersion.startsWith("2.11.") && javaVersion.startsWith("1.8.")){
          val evaled = exec('basic/"Spark.sc")
          assert(evaled.out.string.contains("List(10, 20, 30, 40, 50)"))
        }
      }
    }

    'spark2 - {
      // Note that this script screws up if you try to run it within SBT! It has to
      // be run as an integration test, or via `sbt amm/test:assembly && amm/target/amm`
      if (!Util.windowsPlatform) {
        if (scalaVersion.startsWith("2.11.") && javaVersion.startsWith("1.8.")){
          val evaled = exec('basic/"Spark2.sc")
          assert(evaled.out.string.contains("fake db write"))
        }
      }
    }

    'httpApi{
      'addPost {
        val res = exec('basic / "HttpApi.sc", "addPost", "title", "some text")
        assert(res.out.trim.startsWith("101"))
      }
      'comments{
        val res = exec('basic / "HttpApi.sc", "comments", "40")
        assert(res.out.trim.contains("totam vel saepe aut"))
        assert(res.out.trim.contains("aperiam et omnis totam"))
      }
    }
  }
}
