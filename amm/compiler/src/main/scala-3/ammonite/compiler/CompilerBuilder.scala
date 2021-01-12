package ammonite.compiler

import java.net.URL
import java.nio.file.Path

import ammonite.compiler.iface.{
  Compiler => ICompiler,
  CompilerBuilder => ICompilerBuilder,
  CompilerLifecycleManager => ICompilerLifecycleManager,
  _
}
import ammonite.util.Frame
import dotty.tools.io.AbstractFile

object CompilerBuilder extends ICompilerBuilder:

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
    val tempDir = AbstractFile.getDirectory(os.temp.dir().toNIO)
    Compiler.addToClasspath(dynamicClassPath, tempDir)
    new Compiler(
      tempDir,
      initialClassPath,
      classPath,
      evalClassLoader,
      classPathWhiteList,
      settings = settings,
      reporter = reporter
    )
  }

  def scalaVersion = dotty.tools.dotc.config.Properties.versionNumberString

  def newManager(
    rtCacheDir: Option[Path],
    headFrame: => Frame,
    dependencyCompleter: => Option[String => (Int, Seq[String])],
    whiteList: Set[Seq[String]],
    initialClassLoader: ClassLoader
  ): ICompilerLifecycleManager =
    new CompilerLifecycleManager(
      rtCacheDir,
      headFrame,
      dependencyCompleter,
      whiteList,
      initialClassLoader
    )
