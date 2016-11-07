package ammonite.runtime

import ammonite.util.ImportData

import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io.{AbstractFile, FileZipArchive, VirtualDirectory}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util._
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.backend.JavaPlatform
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
                 plugins0: Vector[(String, Class[_])],
                 jcp: JavaClassPath,
                 evalClassloader: ClassLoader,
                 importsLen: => Int,
                 updateLastImports: Seq[ImportData] => Unit): nsc.Global = {
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
  def initGlobalBits(classpath: Seq[java.io.File],
                     dynamicClasspath: VirtualDirectory,
                     errorLogger: => String => Unit,
                     warningLogger: => String => Unit,
                     infoLogger: => String => Unit,
                     settings: Settings) = {
    val vd = new io.VirtualDirectory("(memory)", None)
    val settingsX = settings
    val jCtx = new JavaContext()
    val (dirDeps, jarDeps) = classpath.partition(_.isDirectory)

    val jarCP =
      jarDeps.filter(x => x.getName.endsWith(".jar") || Classpath.canBeOpenedAsJar(x))
        .map(x => new DirectoryClassPath(new FileZipArchive(x), jCtx))
        .toVector

    val dirCP =
      dirDeps.map(x => new DirectoryClassPath(new PlainDirectory(new Directory(x)), jCtx))

    val dynamicCP = Seq(new DirectoryClassPath(dynamicClasspath, jCtx))
    val jcp = new JavaClassPath(jarCP ++ dirCP ++ dynamicCP, jCtx)

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
