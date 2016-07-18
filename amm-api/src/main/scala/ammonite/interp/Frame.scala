package ammonite.interp

import ammonite.util.Imports

/**
  * Represents a single "frame" of the `sess.save`/`sess.load` stack/tree.
  *
  * Exposes `imports` and `classpath` as readable but only writable
  * in particular ways: `imports` can only be updated via `mergeImports`,
  * while `classpath` can only be added to.
  */
trait Frame {
  def classloader: SpecialClassLoader
  def pluginClassloader: SpecialClassLoader
  def imports: Imports
  def classpath: Seq[java.io.File]
  def addImports(additional: Imports): Unit
  def addClasspath(additional: Seq[java.io.File]): Unit
}