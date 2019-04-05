package ammonite.interp

import ammonite.runtime.Classpath
import scala.reflect.io.{AbstractFile, FileZipArchive, VirtualDirectory}
import scala.tools.nsc
import scala.tools.nsc.classpath._
import scala.tools.nsc.{CustomZipAndJarFileLookupFactory, Global, Settings}
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter

object GlobalInitCompat{

  def initInteractiveGlobal(settings: Settings,
                            reporter: AbstractReporter,
                            jcp: AggregateClassPath,
                            evalClassloader: ClassLoader): InteractiveGlobal = {
    new nsc.interactive.Global(settings, reporter) { g =>
      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = jcp

      override lazy val platform: ThisPlatform = new GlobalPlatform {
        override val global = g
        override val settings = g.settings
        override val classPath = jcp
      }

      override lazy val analyzer = CompilerCompatibility.interactiveAnalyzer(g, evalClassloader)
    }
  }

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

  /**
    * Code to initialize random bits and pieces that are needed
    * for the Scala compiler to function, common between the
    * normal and presentation compiler
    */
  def initGlobalClasspath(dirDeps: Seq[java.io.File],
                          jarDeps: Seq[java.net.URL],
                          dynamicClasspath: VirtualDirectory,
                          settings: Settings) = {

    val jarCP =
      jarDeps.filter(x => x.getPath.endsWith(".jar") || Classpath.canBeOpenedAsJar(x))
        .map { x =>
          if (x.getProtocol == "file") {
            val arc = new FileZipArchive(java.nio.file.Paths.get(x.toURI).toFile)
            ZipAndJarClassPathFactory.create(arc, settings)
          } else {
            val arc = new internal.CustomURLZipArchive(x)
            CustomZipAndJarFileLookupFactory.create(arc, settings)
          }
        }
        .toVector

    val dirCP = dirDeps.map(x => new DirectoryClassPath(x))
    val dynamicCP = new VirtualDirectoryClassPath(dynamicClasspath){

      override def getSubDir(packageDirName: String): Option[AbstractFile] = {
        val pathParts = packageDirName.split('/')
        var file: AbstractFile = dir
        for (dirPart <- pathParts) {
          file = file.lookupName(dirPart, directory = true)
          if (file == null) return None
        }
        Some(file)

      }
      override def findClassFile(className: String): Option[AbstractFile] = {
        val relativePath = FileUtils.dirPath(className)
        val pathParts = relativePath.split('/')
        var file: AbstractFile = dir
        for (dirPart <- pathParts.init) {
          file = file.lookupName(dirPart, directory = true)
          if (file == null) return None
        }

        file.lookupName(pathParts.last + ".class", directory = false) match {
          case null => None
          case file => Some(file)
        }
      }

    }

    val jcp = new AggregateClassPath(jarCP ++ dirCP ++ Seq(dynamicCP))


    jcp
  }
}
