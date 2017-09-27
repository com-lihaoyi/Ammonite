
package ammonite.main

import ammonite.util.Util

/**
  * Constants used in the default configuration for the Ammonite REPL
  */
object Defaults{

  val welcomeBanner = {
    def ammoniteVersion = ammonite.Constants.version
    def scalaVersion = scala.util.Properties.versionNumberString
    def javaVersion = System.getProperty("java.version")
    val link = "www.patreon.com/lihaoyi"
    Util.normalizeNewlines(
      s"""Welcome to the Ammonite Repl $ammoniteVersion
          |(Scala $scalaVersion Java $javaVersion)
          |If you like Ammonite, please support our development at $link""".stripMargin
    )
  }

  // Need to import stuff from ammonite.ops manually, rather than from the
  // ammonite.ops.Extensions bundle, because otherwise they result in ambiguous
  // imports if someone else imports maunally
  val predefString = s"""
    |import ammonite.ops.{
    |  PipeableImplicit,
    |  FilterMapExtImplicit,
    |  FilterMapArraysImplicit,
    |  FilterMapIteratorsImplicit,
    |  FilterMapGeneratorsImplicit,
    |  SeqFactoryFunc,
    |  RegexContextMaker,
    |  Callable1Implicit
    |}
    |import ammonite.runtime.tools._
    |import ammonite.repl.tools._
    |import ammonite.runtime.tools.IvyConstructor.{ArtifactIdExt, GroupIdExt}
    |import ammonite.interp.InterpBridge.value.exit
    |""".stripMargin

  val replPredef = """
    |import ammonite.repl.ReplBridge.value.{
    |  codeColorsImplicit,
    |  tprintColorsImplicit,
    |  pprinterImplicit,
    |  show,
    |  typeOf
    |}
  """.stripMargin
  def ammoniteHome = ammonite.ops.Path(System.getProperty("user.home"))/".ammonite"

}
