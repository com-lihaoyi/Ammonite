package ammonite.interp

import scala.reflect.io.{FileZipArchive, VirtualDirectory}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util._
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.backend.JavaPlatform
import ammonite.runtime.Classpath

object GlobalInitCompat {
  def initInteractiveGlobal(settings: Settings,
                            reporter: AbstractReporter,
                            jcp: JavaClassPath,
                            evalClassloader: ClassLoader) = {
    new nsc.interactive.Global(settings, reporter) { g =>
      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = platform.classPath

      override lazy val platform: ThisPlatform = new JavaPlatform {
        val global: g.type = g

        override def classPath = jcp
      }
      override lazy val analyzer = CompilerCompatibility.interactiveAnalyzer(g, evalClassloader)
    }
  }
  def initGlobal(settings: Settings,
                 reporter: AbstractReporter,
                 jcp: JavaClassPath,
                 evalClassloader: ClassLoader,
                 createPlugins: Global => List[Plugin]): nsc.Global = {
    new nsc.Global(settings, reporter) { g =>
      override lazy val plugins = createPlugins(g)

      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = platform.classPath
      val x = new JavaPlatform{
        val global: g.type = g
        override def classPath = jcp
      }
      override lazy val platform: ThisPlatform = x

      override lazy val analyzer = CompilerCompatibility.analyzer(g, evalClassloader)
    }
  }
  /**
    * Code to initialize random bits and pieces that are needed
    * for the Scala compiler to function, common between the
    * normal and presentation compiler
    */
  def initGlobalClasspath(dirDeps: Seq[java.io.File],
                          jarDeps: Seq[java.net.URL],
                          dynamicClasspath: VirtualDirectory,
                          settings: Settings) = {

    val jCtx = new JavaContext()

    val jarCP =
      jarDeps.filter(x => x.getPath.endsWith(".jar") || Classpath.canBeOpenedAsJar(x))
        .map { x =>
          val arc =
            if (x.getProtocol == "file")
              new FileZipArchive(java.nio.file.Paths.get(x.toURI).toFile)
            else
              new internal.CustomURLZipArchive(x)
          new DirectoryClassPath(arc, jCtx)
        }
        .toVector

    val dirCP =
      dirDeps.map(x => new DirectoryClassPath(new PlainDirectory(new Directory(x)), jCtx))

    val dynamicCP = Seq(new DirectoryClassPath(dynamicClasspath, jCtx))
    val jcp = new JavaClassPath(jarCP ++ dirCP ++ dynamicCP, jCtx)

    jcp
  }
}
