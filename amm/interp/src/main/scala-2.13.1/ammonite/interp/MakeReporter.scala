package ammonite.interp

import ammonite.runtime.Classpath

import scala.reflect.internal.util.Position
import scala.reflect.io.FileZipArchive
import scala.tools.nsc
import scala.tools.nsc.classpath.{AggregateClassPath, ZipAndJarClassPathFactory}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interactive.{InteractiveAnalyzer, Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.FilteringReporter
import scala.tools.nsc.typechecker.Analyzer

object MakeReporter {
  def makeReporter(errorLogger: => String => Unit,
                   warningLogger: => String => Unit,
                   infoLogger: => String => Unit,
                   outerSettings: Settings) = {
    new FilteringReporter {
      def displayPrompt(): Unit = ???
      def doReport(pos: scala.reflect.internal.util.Position, msg: String, severity: Severity): Unit = {
        info0(pos, msg, severity, false)
      }
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

      val settings = outerSettings
    }
  }
}
