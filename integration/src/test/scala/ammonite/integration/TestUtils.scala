package ammonite.integration

import ammonite.ops._
import ImplicitWd._

/**
  * Created by haoyi on 6/4/16.
  */
object TestUtils {
  val scalaVersion = scala.util.Properties.versionNumberString
  val javaVersion = scala.util.Properties.javaVersion
  val ammVersion = ammonite.Constants.version
  val executableName = s"ammonite-$ammVersion-$scalaVersion"
  val Seq(executable) = ls.rec! cwd |? (_.last == executableName)
  val intTestResources = cwd/'integration/'src/'test/'resources
  val replStandaloneResources = intTestResources/'ammonite/'integration
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

  /**
    *Counts number of non-overlapping occurances of `subs` in `s`
    */
  def substrCount(s: String, subs: String, count: Int = 0, ptr: Int = 0): Int = {
    s.indexOf(subs, ptr) match{
      case -1 => count
      case x => substrCount(s, subs, count+1, x + subs.length)
    }
  }
}
