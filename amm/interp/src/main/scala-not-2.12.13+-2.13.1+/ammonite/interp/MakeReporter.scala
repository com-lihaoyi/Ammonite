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

object MakeReporter{

  type Reporter = AbstractReporter

  def makeReporter(errorLogger: (Position, String) => Unit,
                   warningLogger: (Position, String) => Unit,
                   infoLogger: (Position, String) => Unit,
                   outerSettings: Settings): Reporter =
    new AbstractReporter {
      def displayPrompt(): Unit = ???

      def display(pos: Position, msg: String, severity: Severity) = {
        severity match{
          case ERROR =>
            Classpath.traceClasspathProblem(s"ERROR: $msg")
            errorLogger(pos, msg)
          case WARNING =>
            warningLogger(pos, msg)
          case INFO =>
            infoLogger(pos, msg)
        }
      }

      val settings = outerSettings
    }
}