package ammonite.integration

import ammonite.ops._

/**
  * Created by haoyi on 6/4/16.
  */
object TestUtils {
  val scalaVersion = scala.util.Properties.versionNumberString
  val javaVersion = scala.util.Properties.javaVersion
  val ammVersion = ammonite.Constants.version
  val executableName = s"ammonite-repl-$ammVersion-$scalaVersion"
  val Seq(executable) = ls.rec! cwd |? (_.last == executableName)
  val replStandaloneResources = cwd/'integration/'src/'test/'resources/'ammonite/'integration
  val shellAmmoniteResources = cwd/'shell/'src/'main/'resources/'ammonite/'shell
  val emptyPrefdef = shellAmmoniteResources/"empty-predef.scala"
  val exampleBarePredef = shellAmmoniteResources/"example-predef-bare.scala"
}
