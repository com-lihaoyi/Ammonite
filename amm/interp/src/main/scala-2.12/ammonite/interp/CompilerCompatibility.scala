package ammonite.interp

import ammonite.runtime.Classpath

import scala.reflect.internal.util.Position
import scala.reflect.io.FileZipArchive
import scala.tools.nsc
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.classpath.{AggregateClassPath, ZipAndJarClassPathFactory}
import scala.tools.nsc.interactive.{InteractiveAnalyzer, Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.typechecker.Analyzer

object CompilerCompatibility extends ExtraCompilerCompatibility {
  def analyzer(g: Global, cl: ClassLoader): Analyzer { val global: g.type } = {
    new { val global: g.type = g } with Analyzer {
      override def findMacroClassLoader() = cl
    }
  }

  def interactiveAnalyzer(g: InteractiveGlobal,
                          cl: ClassLoader): InteractiveAnalyzer { val global: g.type } = {
    new { val global: g.type = g } with InteractiveAnalyzer {
      override def findMacroClassLoader() = cl
    }
  }
  def importInfo(g: Global)(t: g.Import) =
    new g.analyzer.ImportInfo(t, 0)



  def initGlobal(settings: Settings,
                 reporter: AbstractReporter,
                 jcp: AggregateClassPath,
                 evalClassloader: ClassLoader,
                 createPlugins: Global => List[Plugin]): Global = {

    new nsc.Global(settings, reporter) { g =>
      override lazy val plugins = createPlugins(g)

      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = jcp

      override lazy val platform: ThisPlatform = new GlobalPlatform {
        override val global = g
        override val settings = g.settings
        override val classPath = jcp
      }

      override lazy val analyzer = CompilerCompatibility.analyzer(g, evalClassloader)
    }
  }
}
