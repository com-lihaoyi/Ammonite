package ammonite

import ammonite.compiler.CodeClassWrapper
import ammonite.util.{Evaluated, Res}

/**
  * Wraps several [[TestRepl]], and runs its tests against all of them.
  */
class DualTestRepl { dual =>

  def predef: (String, Option[os.Path]) = ("", None)

  val compilerBuilder = ammonite.compiler.CompilerBuilder
  val repls = Seq(
    new TestRepl(compilerBuilder) {
      override def predef = dual.predef
    },
    new TestRepl(compilerBuilder) {
      override def predef = dual.predef
      override def codeWrapper = CodeClassWrapper
    }
  )

  def scalaVersion = compilerBuilder.scalaVersion
  def scala2 = scalaVersion.startsWith("2.")
  def scala2_12 = scalaVersion.startsWith("2.12.")

  def interps = repls.map(_.interp)

  def session(sess: String): Unit =
    repls.foreach(_.session(sess))
  def result(input: String, expected: Res[Evaluated]): Unit =
    repls.foreach(_.result(input, expected))
  def fail(input: String,
           failureCheck: String => Boolean = _ => true): Unit =
    repls.foreach(_.fail(input, failureCheck))

  def notFound(name: String): String =
    repls.headOption.fold("")(_.notFound(name))
}
