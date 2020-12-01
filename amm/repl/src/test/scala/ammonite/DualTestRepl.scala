package ammonite

import ammonite.compiler.CodeClassWrapper
import ammonite.runtime.Evaluated
import ammonite.util.Res

/**
  * Wraps several [[TestRepl]], and runs its tests against all of them.
  */
class DualTestRepl(runtime: TestReplRuntime = TestRepl.sharedRuntime) { dual =>

  def scala2_12 = runtime.scala2_12
  def userScalaVersion = runtime.userScalaVersion

  def predef: (String, Option[os.Path]) = ("", None)

  val repls = Seq(
    new TestRepl(runtime) {
      override def predef = dual.predef
    },
    new TestRepl(runtime) {
      override def predef = dual.predef
      override def codeWrapper = CodeClassWrapper
    }
  )

  def interps = repls.map(_.interp)

  def session(sess: String): Unit =
    repls.foreach(_.session(sess))
  def result(input: String, expected: Res[Evaluated]): Unit =
    repls.foreach(_.result(input, expected))
  def fail(input: String,
           failureCheck: String => Boolean = _ => true): Unit =
    repls.foreach(_.fail(input, failureCheck))
}
