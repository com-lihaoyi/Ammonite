
package ammonite.main

/**
  * Constants used in the default configuration for the Ammonite REPL
  */
object Defaults{

  val welcomeBanner = {
    def ammoniteVersion = ammonite.Constants.version
    def scalaVersion = scala.util.Properties.versionNumberString
    def javaVersion = System.getProperty("java.version")
    s"""Welcome to the Ammonite Repl $ammoniteVersion
        |(Scala $scalaVersion Java $javaVersion)""".stripMargin
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
    |import ammonite.frontend.ReplBridge.repl
    |import ammonite.ops.Extensions.{
    |  $ignoreUselessImports
    |}
    |import ammonite.tools._
    |import ammonite.tools.IvyConstructor.{ArtifactIdExt, GroupIdExt}
    |import ammonite.frontend.Resolver
    |import ammonite.frontend.ReplBridge.repl.{
    |  Internal => _,
    |  $ignoreUselessImports
    |}
    |import ammonite.main.Router.{doc, export}
    |import ammonite.main.Scripts.pathScoptRead
    |""".stripMargin


  def ammoniteHome = ammonite.ops.Path(System.getProperty("user.home"))/".ammonite"

}