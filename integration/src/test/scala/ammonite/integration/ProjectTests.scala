package ammonite.integration

import ammonite.integration.TestUtils._
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
object ProjectTests extends TestSuite {

  val tests = Tests {
    println("Running ProjectTest")

    test("playframework") {
      if (!Util.windowsPlatform) {
        if (scalaVersion.startsWith("2.11.") && javaVersion.startsWith("1.8.")) {
          val evaled = exec(os.rel / "basic" / "PlayFramework.sc")
          assert(evaled.out.text().contains("Hello bar"))
        }
      }
    }
//    Disabled due to travis instability
//    test("spark"){
//      // Note that this script screws up if you try to run it within SBT! It has to
//      // be run as an integration test, or via `sbt amm/test:assembly && amm/target/amm`
//      if (!Util.windowsPlatform) {
//        if (scalaVersion.startsWith("2.11.") && javaVersion.startsWith("1.8.")){
//          val evaled = exec('basic/"Spark.sc")
//          assert(evaled.out.string.contains("List(10, 20, 30, 40, 50)"))
//        }
//      }
//    }

    test("spark2") {
      // Note that this script screws up if you try to run it within SBT! It has to
      // be run as an integration test, or via `sbt amm/test:assembly && amm/target/amm`
      if (!Util.windowsPlatform) {
        if (scalaVersion.startsWith("2.11.") && javaVersion.startsWith("1.8.")) {
          val evaled = exec(os.rel / "basic" / "Spark2.sc")
          assert(evaled.out.text().contains("fake db write"))
        }
      }
    }
  }
}
