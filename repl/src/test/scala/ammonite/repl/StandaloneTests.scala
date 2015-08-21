package ammonite.repl

import utest._
import ammonite.ops._
import ammonite.ops.ImplicitWd._
/**
 * Run a small number of scripts using the Ammonite standalone executable,
 * to make sure that this works. Otherwise it tends to break since the
 * standalone executable has a pretty different classloading environment
 * from the "run in SBT on raw class files" that the rest of the tests use.
 */
object StandaloneTests extends TestSuite{
  // Prepare standalone executable
  val scalaVersion = scala.util.Properties.versionNumberString
  // This is kinda sketchy to call SBT from inside SBT, but by
  // this point the host SBT project will have completed most of
  // its heavy lifting and will just be running tests, so it
  // should be safe...
  println("StandaloneTests running repl/assembly")
  %sbt("++"+scalaVersion, "repl/assembly")
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
