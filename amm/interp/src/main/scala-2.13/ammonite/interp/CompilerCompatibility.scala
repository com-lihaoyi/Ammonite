package ammonite.interp

import scala.reflect.io.FileZipArchive
import scala.tools.nsc
import scala.tools.nsc.classpath.{AggregateClassPath, ZipAndJarClassPathFactory}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interactive.{InteractiveAnalyzer, Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.typechecker.Analyzer

object CompilerCompatibility {
  def analyzer(g: Global, cl: ClassLoader): Analyzer { val global: g.type } = {
    new { val global: g.type = g } with Analyzer {
      override def defaultMacroClassloader = global.findMacroClassLoader
    }
  }

  def interactiveAnalyzer(g: InteractiveGlobal,
                          cl: ClassLoader): InteractiveAnalyzer { val global: g.type } = {
    new { val global: g.type = g } with InteractiveAnalyzer {
    }
  }

  def importInfo(g: Global)(t: g.Import) =
    new g.analyzer.ImportInfo(t, 0, false)

  def initGlobal(settings: Settings,
                 reporter: AbstractReporter,
                 jcp: AggregateClassPath,
                 evalClassloader: ClassLoader,
                 createPlugins: Global => List[Plugin]): Global = {

    new nsc.Global(settings, reporter) { g =>
      override lazy val plugins = createPlugins(g)

      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = jcp
      override def findMacroClassLoader() = evalClassloader
      override lazy val platform: ThisPlatform = new GlobalPlatform {
        override val global = g
        override val settings = g.settings
        override val classPath = jcp
      }

      override lazy val analyzer = CompilerCompatibility.analyzer(g, evalClassloader)
    }
  }

  def createZipJarFactory(arc: FileZipArchive, settings: Settings) = {
    ZipAndJarClassPathFactory.create(arc, settings, new scala.tools.nsc.CloseableRegistry())
  }
}
