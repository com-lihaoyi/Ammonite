
package ammonite.main
import acyclic.file
import ammonite.util.Util

/**
  * Constants used in the default configuration for the Ammonite REPL
  */
object Defaults{

  val welcomeBanner = {
    def ammoniteVersion = ammonite.Constants.version
    def scalaVersion = scala.util.Properties.versionNumberString
    def javaVersion = System.getProperty("java.version")
    Util.normalizeNewlines(
      s"""Welcome to the Ammonite Repl $ammoniteVersion
          |(Scala $scalaVersion Java $javaVersion)""".stripMargin
    )
  }
  val ignoreUselessImports = """
    |notify => _,
    |  wait => _,
    |  equals => _,
    |  asInstanceOf => _,
    |  synchronized => _,
    |  notifyAll => _,
    |  isInstanceOf => _,
    |  == => _,
    |  != => _,
    |  getClass => _,
    |  ne => _,
    |  eq => _,
    |  ## => _,
    |  hashCode => _,
    |  _
    |"""

  val predefString = s"""
    |import ammonite.repl.ReplBridge.{value => repl}
    |import ammonite.runtime.InterpBridge.{value => interp}
    |import ammonite.ops.Extensions.{
    |  $ignoreUselessImports
    |}
    |import ammonite.runtime.tools._
    |import ammonite.repl.tools._
    |import ammonite.runtime.tools.IvyConstructor.{ArtifactIdExt, GroupIdExt}
    |import ammonite.repl.ReplBridge.value.{
    |  Internal => _,
    |  $ignoreUselessImports
    |}
    |import ammonite.main.Router.{doc, main}
    |import ammonite.main.Scripts.pathScoptRead
    |""".stripMargin


  def ammoniteHome = ammonite.ops.Path(System.getProperty("user.home"))/".ammonite"

}