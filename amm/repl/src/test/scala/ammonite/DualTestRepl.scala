package ammonite

import ammonite.interp.CodeClassWrapper
import ammonite.repl.api.SourceBridge
import ammonite.repl.SourceAPIImpl
import ammonite.util.{Evaluated, Res}

/**
  * Wraps several [[TestRepl]], and runs its tests against all of them.
  */
class DualTestRepl { dual =>

  if (SourceBridge.value0 == null)
    SourceBridge.value0 = new SourceAPIImpl {}

  def predef: (String, Option[os.Path]) = ("", None)

  val repls = Seq(
    new TestRepl {
      override def predef = dual.predef
    },
    new TestRepl {
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
