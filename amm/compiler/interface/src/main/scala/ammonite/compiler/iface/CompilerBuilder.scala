package ammonite.compiler.iface

import java.net.URL
import java.nio.file.Path

import ammonite.util.Frame

abstract class CompilerBuilder {

  def newManager(
    rtCacheDir: Option[Path],
    headFrame: => Frame,
    dependencyCompleter: => Option[String => (Int, Seq[String])],
    whiteList: Set[Seq[String]],
    initialClassLoader: ClassLoader,
    settings: Seq[String]
  ): CompilerLifecycleManager

  def create(
    initialClassPath: Seq[URL],
    classPath: Seq[URL],
    dynamicClassPath: Seq[(String, Array[Byte])],
    evalClassLoader: ClassLoader,
    pluginClassLoader: ClassLoader,
    reporter: Option[CompilerBuilder.Message => Unit],
    settings: Seq[String],
    classPathWhiteList: Set[Seq[String]],
    lineNumberModifier: Boolean
  ): Compiler

  def scalaVersion: String
}

object CompilerBuilder {

  case class Message(
    severity: String,
    start: Int,
    end: Int,
    message: String
  )

}
