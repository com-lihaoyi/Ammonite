
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
          |(Scala $scalaVersion Java $javaVersion)
          |If you like Ammonite, please support our development at www.patreon.com/lihaoyi""".stripMargin
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

  // Need to import stuff from ammonite.ops manually, rather than from the
  // ammonite.ops.Extensions bundle, because otherwise they result in ambiguous
  // imports if someone else imports maunally
  val predefString = s"""
    |import ammonite.ops.{
    |  Pipeable,
    |  FilterMapExt,
    |  FilterMapArrays,
    |  FilterMapIterators,
    |  FilterMapGenerators,
    |  SeqFactoryFunc,
    |  ChainableConversions,
    |  RegexContextMaker,
    |  Callable1,
    |  Callable2
    |}
    |import ammonite.runtime.tools._
    |import ammonite.repl.tools._
    |import ammonite.runtime.tools.IvyConstructor.{ArtifactIdExt, GroupIdExt}
    |import ammonite.repl.ReplBridge.value.{exit, codeColors, tprintColors, show, typeOf}
    |import ammonite.main.Router.{doc, main}
    |import ammonite.main.Scripts.pathScoptRead
    |""".stripMargin


  def ammoniteHome = ammonite.ops.Path(System.getProperty("user.home"))/".ammonite"

}
