package ammonite.compiler

import java.net.URL
import java.nio.file.Path

import ammonite.compiler.iface.{
  Compiler => ICompiler,
  CompilerBuilder => ICompilerBuilder
}
import ammonite.util.Frame

import scala.collection.mutable
import scala.reflect.internal.util.{NoPosition, Position}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings


case class CompilerBuilder() extends ICompilerBuilder {
  def create(
    initialClassPath: Seq[URL],
    classPath: Seq[URL],
    dynamicClassPath: Seq[(String, Array[Byte])],
    evalClassLoader: ClassLoader,
    pluginClassLoader: ClassLoader,
    reporter: Option[ICompilerBuilder.Message => Unit],
    settings: Seq[String],
    classPathWhiteList: Set[Seq[String]],
    lineNumberModifier: Boolean
  ): ICompiler = {

    val vd = new VirtualDirectory("(memory)", None)
    Compiler.addToClasspath(dynamicClassPath, vd)

    val scalacSettings = {
      // not 100% sure error collection is correct (duplicates?)
      val errors = new mutable.ListBuffer[String]
      val settings0 = new Settings(err => errors += err)
      val (_, unparsed) = settings0.processArguments(settings.toList, processAll = true)
      for (arg <- unparsed)
        errors += s"Unrecognized argument: $arg"
      // TODO Report the errors via reporter?
      settings0
    }

    val scalacReporterOpt = reporter.map { f =>
      def report(pos: Position, message: String, severity: String) = {
        val (start, end) =
          if (pos == NoPosition) (0, 0)
          else (pos.start, pos.end)
        val msg = ICompilerBuilder.Message(severity, start, end, message)
        f(msg)
      }
      MakeReporter.makeReporter(
        (pos, msg) => report(pos, msg, "ERROR"),
        (pos, msg) => report(pos, msg, "WARNING"),
        (pos, msg) => report(pos, msg, "INFO"),
        scalacSettings
      )
    }

    Compiler(
      classPath,
      vd,
      evalClassLoader,
      pluginClassLoader,
      () => (),
      scalacReporterOpt,
      scalacSettings,
      classPathWhiteList.map(_.toSeq).toSet,
      initialClassPath,
      lineNumberModifier
    )
  }

  def newManager(
    rtCacheDir: Option[Path],
    headFrame: => Frame,
    dependencyCompleter: => Option[String => (Int, Seq[String])],
    whiteList: Set[Seq[String]],
    initialClassLoader: ClassLoader
  ): CompilerLifecycleManager =
    new CompilerLifecycleManager(
      rtCacheDir,
      headFrame,
      dependencyCompleter,
      whiteList,
      initialClassLoader
    )

  def scalaVersion = CompilerBuilder.scalaVersion
}

object CompilerBuilder {
  def scalaVersion = scala.util.Properties.versionNumberString
}
