
package ammonite.main

import java.io.InputStream

import ammonite.util.Util

import scala.io.Codec

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

  def alreadyLoadedDependencies(
    resourceName: String = "amm-dependencies.txt"
  ): Seq[coursier.Dependency] = {

    var is: InputStream = null

    try {
      is = Thread.currentThread().getContextClassLoader.getResourceAsStream(resourceName)
      if (is == null)
        throw new Exception(s"Resource $resourceName not found")
      scala.io.Source.fromInputStream(is)(Codec.UTF8)
        .mkString
        .split('\n')
        .filter(_.nonEmpty)
        .map(l => l.split(':') match {
          case Array(org, name, ver) =>
            coursier.Dependency(coursier.Module(org, name), ver)
          case other =>
            throw new Exception(s"Cannot parse line '$other' from resource $resourceName")
        })
    } finally {
      if (is != null)
        is.close()
    }
  }

}
