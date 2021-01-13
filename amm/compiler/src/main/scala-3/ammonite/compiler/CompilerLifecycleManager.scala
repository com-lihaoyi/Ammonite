package ammonite.compiler

import ammonite.util.Util._
import ammonite.util.{Classpath, ImportTree, Printer}
import ammonite.util.Util.ClassFiles

import java.nio.file.Path

import dotty.tools.dotc
import dotc.{Compiler => DottyCompiler}
import dotc.core.Contexts.FreshContext
import dotty.tools.io.AbstractFile
import scala.collection.mutable


/**
  * Wraps up the `Compiler` and `Pressy`, ensuring that they get properly
  * initialized before use. Mostly deals with ensuring the object lifecycles
  * are properly dealt with; `Compiler` and `Pressy` are the ones which deal
  * with the compiler's nasty APIs
  *
  * Exposes a simple API where you can just call methods like `compilerClass`
  * `configureCompiler` any-how and not worry about ensuring the necessary
  * compiler objects are initialized, or worry about initializing them more
  * than necessary
  */
class CompilerLifecycleManager(
  rtCacheDir: Option[Path],
  headFrame: => ammonite.util.Frame,
  dependencyCompleteOpt: => Option[String => (Int, Seq[String])],
  classPathWhitelist: Set[Seq[String]],
  initialClassLoader: ClassLoader
) extends ammonite.compiler.iface.CompilerLifecycleManager {

  def scalaVersion = dotc.config.Properties.versionNumberString

  def forceInit(): Unit = init(force = true)
  def init(): Unit = init(force = false)


  private[this] object Internal{
    val dynamicClasspath = AbstractFile.getDirectory(os.temp.dir().toNIO)
    var compiler: ammonite.compiler.Compiler = null
    val onCompilerInit = mutable.Buffer.empty[DottyCompiler => Unit]
    val onSettingsInit = mutable.Buffer.empty[FreshContext => Unit] // TODO Pass a SettingsState too
    var preConfiguredSettingsChanged: Boolean = false
    var compilationCount = 0
    var (lastFrame, lastFrameVersion) = (headFrame, headFrame.version)
  }


  import Internal._


  // Public to expose it in the REPL so people can poke at it at runtime
  // Not for use within Ammonite! Use one of the other methods to ensure
  // that `Internal.compiler` is properly initialized before use.
  def compiler: ammonite.compiler.Compiler = Internal.compiler
  def compilationCount = Internal.compilationCount

  // def pressy: Pressy = Internal.pressy

  def preprocess(fileName: String) = synchronized{
    init()
    compiler.preprocessor(fileName)
  }


  // We lazily force the compiler to be re-initialized by setting the
  // compilerStale flag. Otherwise, if we re-initialized the compiler eagerly,
  // we end up sometimes re-initializing it multiple times unnecessarily before
  // it gets even used once. Empirically, this cuts down the number of compiler
  // re-initializations by about 2/3, each of which costs about 30ms and
  // probably creates a pile of garbage

  def init(force: Boolean = false) = synchronized{
    if (compiler == null ||
        (headFrame ne lastFrame) ||
        headFrame.version != lastFrameVersion ||
        Internal.preConfiguredSettingsChanged ||
        force) {

      lastFrame = headFrame
      lastFrameVersion = headFrame.version

      val initialClassPath = Classpath.classpath(initialClassLoader, rtCacheDir)
      val headFrameClassPath =
        Classpath.classpath(headFrame.classloader, rtCacheDir)

      Internal.compiler = new Compiler(
        Internal.dynamicClasspath,
        initialClassPath,
        headFrameClassPath,
        headFrame.classloader,
        classPathWhitelist,
        dependencyCompleteOpt = dependencyCompleteOpt,
        contextInit = c => onSettingsInit.foreach(_(c))
      )
      onCompilerInit.foreach(_(compiler.compiler))

      Internal.preConfiguredSettingsChanged = false
    }
  }

  def complete(
    offset: Int,
    previousImports: String,
    snippet: String
  ): (Int, Seq[String], Seq[String]) = synchronized{
    init()
    Internal.compiler.complete(offset, previousImports, snippet)
  }

  def compileClass(
    processed: ammonite.compiler.iface.Preprocessor.Output,
    printer: Printer,
    fileName: String
  ): Option[ammonite.compiler.iface.Compiler.Output] = synchronized{
    // Enforce the invariant that every piece of code Ammonite ever compiles,
    // gets run within the `ammonite` package. It's further namespaced into
    // things like `ammonite.$file` or `ammonite.$sess`, but it has to be
    // within `ammonite`
    assert(processed.code.trim.startsWith("package ammonite"))

    init()
    val compiled = compiler.compile(
      processed.code.getBytes(scala.util.Properties.sourceEncoding),
      printer,
      processed.prefixCharLength,
      processed.userCodeNestingLevel,
      fileName
    )
    Internal.compilationCount += 1
    compiled
  }

  def configureCompiler(callback: DottyCompiler => Unit) = synchronized{
    onCompilerInit.append(callback)
    if (compiler != null){
      callback(compiler.compiler)
    }
  }

  def preConfigureCompiler(callback: FreshContext => Unit) =
    synchronized {
      onSettingsInit.append(callback)
      preConfiguredSettingsChanged = true
    }

  def addToClasspath(classFiles: ClassFiles): Unit = synchronized {
    Compiler.addToClasspath(classFiles, dynamicClasspath)
  }
  def shutdownPressy() = () // N/A in Scala 3
}
