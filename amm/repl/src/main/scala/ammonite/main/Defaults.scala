
package ammonite.main

import java.io.InputStream

import ammonite.util.{ImportData, Imports, Util}
import coursierapi.Dependency

import scala.io.Codec

/**
  * Constants used in the default configuration for the Ammonite REPL
  */
object Defaults{

  val welcomeBanner = {
    def ammoniteVersion = ammonite.Constants.version
    def scalaVersion = scala.util.Properties.versionNumberString
    def javaVersion = System.getProperty("java.version")
    Util.normalizeNewlines(
      s"Welcome to the Ammonite Repl $ammoniteVersion (Scala $scalaVersion Java $javaVersion)"
    )
  }

  val replImports = Imports(
    ImportData("""ammonite.repl.ReplExtras.{
      ReplAPIExtensions,
      codeColorsImplicit,
      pprinterImplicit,
      show,
      tprintColorsImplicit,
      typeOf
    }""")
  )
  def ammoniteHome = os.Path(System.getProperty("user.home"))/".ammonite"

  def alreadyLoadedDependencies(
    resourceName: String = "amm-dependencies.txt"
  ): Seq[Dependency] = {

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
            Dependency.of(org, name, ver)
          case other =>
            throw new Exception(s"Cannot parse line '$other' from resource $resourceName")
        })
    } finally {
      if (is != null)
        is.close()
    }
  }

}
