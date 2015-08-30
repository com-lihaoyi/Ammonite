package ammonite.repl

import utest._
import ammonite.ops._
import ammonite.ops.ImplicitWd._
import utest.framework.TestSuite

/**
 * Run a small number of scripts using the Ammonite standalone executable,
 * to make sure that this works. Otherwise it tends to break since the
 * standalone executable has a pretty different classloading environment
 * from the "run in SBT on raw class files" that the rest of the tests use.
 *
 * Need to call sbt repl/assembly beforehand to make these pass
 */
object StandaloneTests extends TestSuite{
  // Prepare standalone executable
  val scalaVersion = scala.util.Properties.versionNumberString
  val tests = TestSuite {
    val ammVersion = ammonite.Constants.version
    val executableName = s"ammonite-repl-$ammVersion-$scalaVersion"
    val Seq(executable) = ls.rec! cwd |? (_.last == executableName)
    val resources = cwd/'repl/'src/'test/'resource/'standalone

    def exec(name: String) = (%%bash(executable, resources/name)).mkString("\n")
    'hello{
      val evaled = exec("Hello.scala")
      assert(evaled == "Hello World")
    }
    'complex{
      val evaled = exec("Complex.scala")
      assert(evaled.contains("Spire Interval [0, 10]"))
    }
  }
}
