package ammonite.repl

import utest._
import ammonite.ops._
import ammonite.ops.ImplicitWd.cwd

/**
 * Run a small number of scripts using the Ammonite standalone executable,
 * to make sure that this works. Otherwise it tends to break since the
 * standalone executable has a pretty different classloading environment
 * from the "run in SBT on raw class files" that the rest of the tests use.
 *
 * Requires `sbt/assembly` to be run before-hand to work.
 */
object StandaloneTests extends TestSuite{
  val tests = TestSuite {
    val scalaVersion = scala.util.Properties.versionNumberString
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
      assert(
        evaled.contains("Int GCD 6"),
        evaled.contains("Long GCD 6")
      )
    }
  }
}
