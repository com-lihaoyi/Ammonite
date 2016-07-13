package ammonite.integration

import ammonite.ops._
import ImplicitWd._

/**
  * Created by haoyi on 6/4/16.
  */
object TestUtils {
  val windowsPlatform = System.getProperty("os.name").startsWith("Windows")
  val newLine = windowsPlatform match{
    case true => "\\r\\n"
    case false => "\\n"
  }
  val scalaVersion = scala.util.Properties.versionNumberString
  val javaVersion = scala.util.Properties.javaVersion
  val ammVersion = ammonite.Constants.version
  val executableName = s"ammonite-$ammVersion-$scalaVersion"
  val Seq(executable) = ls.rec! cwd |? (_.last == executableName)
  val replStandaloneResources = cwd/'integration/'src/'test/'resources/'ammonite/'integration
  val shellAmmoniteResources = cwd/'shell/'src/'main/'resources/'ammonite/'shell
  val emptyPrefdef = shellAmmoniteResources/"empty-predef.sc"
  val exampleBarePredef = shellAmmoniteResources/"example-predef-bare.sc"

  //we use an empty predef file here to isolate the tests from external forces.
  def exec(name: RelPath, args: String*) = {
    %%bash(
      executable,
      "--home",
      tmp.dir(),
      replStandaloneResources / name,
      args
    )
  }
}
