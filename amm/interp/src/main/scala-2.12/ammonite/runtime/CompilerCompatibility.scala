package ammonite.runtime

import ammonite.util.ImportData

import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io.{AbstractFile, FileZipArchive, VirtualDirectory}
import scala.tools.nsc
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.classpath._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.typechecker.Analyzer

object CompilerCompatibility {
  def analyzer(g: Global, cl: ClassLoader): Analyzer { val global: g.type } =
    new { val global: g.type = g } with Analyzer {
      override def findMacroClassLoader() = cl
    }

  type InteractiveAnalyzer = scala.tools.nsc.interactive.InteractiveAnalyzer

  def interactiveAnalyzer(g: InteractiveGlobal, cl: ClassLoader)
                         : InteractiveAnalyzer { val global: g.type } =
    new { val global: g.type = g } with InteractiveAnalyzer {
      override def findMacroClassLoader() = cl
    }

  def trees(g: Global)(parser: g.syntaxAnalyzer.UnitParser): Seq[Global#Tree] =
    parser.parseStatsOrPackages()

  def pluginInit(plugin: Plugin, options: List[String], error: String => Unit): Boolean =
    plugin.init(options, error)

}
object GlobalInitCompat{
  def initInteractiveGlobal(settings: Settings,
                            reporter: AbstractReporter,
                            jcp: AggregateClassPath,
                            evalClassloader: ClassLoader) = {
    new nsc.interactive.Global(settings, reporter) { g =>
      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = jcp

      override lazy val analyzer = CompilerCompatibility.interactiveAnalyzer(g, evalClassloader)
    }
  }
  def initGlobal(settings: Settings,
                 reporter: AbstractReporter,
                 plugins0: Vector[(String, Class[_])],
                 jcp: AggregateClassPath,
                 evalClassloader: ClassLoader,
                 importsLen: => Int,
                 updateLastImports: Seq[ImportData] => Unit) = {
    new nsc.Global(settings, reporter) { g =>
      override lazy val plugins = List(new AmmonitePlugin(g, updateLastImports, importsLen)) ++ {
        for {
          (name, cls) <- plugins0
          plugin = Plugin.instantiate(cls, g)
          initOk =
          try CompilerCompatibility.pluginInit(plugin, Nil, g.globalError)
          catch { case ex: Exception =>
            Console.err.println(s"Warning: disabling plugin $name, initialization failed: $ex")
            false
          }
          if initOk
        } yield plugin
      }

      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = jcp

      override lazy val analyzer = CompilerCompatibility.analyzer(g, evalClassloader)
    }
  }
  /**
    * Code to initialize random bits and pieces that are needed
    * for the Scala compiler to function, common between the
    * normal and presentation compiler
    */
  def initGlobalBits(classpath: Seq[java.io.File],
                     dynamicClasspath: VirtualDirectory,
                     errorLogger: => String => Unit,
                     warningLogger: => String => Unit,
                     infoLogger: => String => Unit,
                     settings: Settings) = {
    val vd = new io.VirtualDirectory("(memory)", None)
    val settingsX = settings

    val (dirDeps, jarDeps) = classpath.partition(_.isDirectory)

    val jarCP =
      jarDeps.filter(x => x.getName.endsWith(".jar") || Classpath.canBeOpenedAsJar(x))
        .map(x => ZipAndJarClassPathFactory.create(new FileZipArchive(x), settingsX))
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

    if (Classpath.traceClasspathIssues) {
      settings.Ylogcp.value = true
      println("jardeps")
      jarDeps.foreach(p => println(s"${p.getName.takeRight(4)} $p"))
      println("finished")
    }

    settings.outputDirs.setSingleOutput(vd)

    settings.nowarnings.value = true
    val reporter = new AbstractReporter {
      def displayPrompt(): Unit = ???

      def display(pos: Position, msg: String, severity: Severity) = {
        severity match{
          case ERROR =>
            Classpath.traceClasspathProblem(s"ERROR: $msg")
            errorLogger(Position.formatMessage(pos, msg, false))
          case WARNING =>
            warningLogger(Position.formatMessage(pos, msg, false))
          case INFO =>
            infoLogger(Position.formatMessage(pos, msg, false))
        }
      }

      val settings = settingsX
    }
    (reporter, vd, jcp)
  }
}
