package ammonite.compiler

import java.util.{Map => JMap}

import ammonite.compiler._
import ammonite.util.InterfaceExtensions._
import ammonite.util.Util._
import ammonite.util.{Classpath, Printer}

import java.nio.file.Path
import java.util.function.{Function, Supplier}

import scala.collection.mutable
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings


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
class CompilerLifecycleManager extends ammonite.compiler.iface.CompilerLifecycleManager {

  private var rtCacheDir = Option.empty[Path]
  private var headFrameSupplier: Supplier[ammonite.compiler.iface.Frame] =
    null
  private var dependencyCompleteOpt =
    () => Option.empty[String => (Int, Seq[String])]
  private var classPathWhitelist = Set.empty[Seq[String]]
  private var initialClassLoader: ClassLoader = null

  private def headFrame =
    if (headFrameSupplier == null) sys.error("headFrame not set")
    else headFrameSupplier.get()

  def setRTCacheDir(dir: Path): Unit = {
    rtCacheDir = Option(dir)
  }
  def setHeadFrame(frame: Supplier[ammonite.compiler.iface.Frame]): Unit = {
    headFrameSupplier = frame
  }
  def setDependencyCompleter(
    completer: Supplier[Function[String, JMap.Entry[Integer, Array[String]]]]
  ): Unit = {
    dependencyCompleteOpt = { () =>
      val fOpt = Option(completer.get())
      fOpt.map { f =>
        input =>
          val e = f(input)
          (e.getKey, e.getValue.toSeq)
      }
    }
  }
  def setClassPathWhiteList(list: Array[Array[String]]): Unit = {
    classPathWhitelist = list.iterator.map(_.toVector).toSet
  }
  def setInitialClassLoader(cl: ClassLoader): Unit = {
    initialClassLoader = cl
  }

  def userScalaVersion = scala.util.Properties.versionNumberString

  def forceInit(): Unit = init(force = true)
  def init(): Unit = init(force = false)


  private[this] object Internal{
    val dynamicClasspath = new VirtualDirectory("(memory)", None)
    var compiler: ammonite.compiler.iface.Compiler = null
    val onCompilerInit = mutable.Buffer.empty[scala.tools.nsc.Global => Unit]
    val onSettingsInit = mutable.Buffer.empty[scala.tools.nsc.Settings => Unit]
    var preConfiguredSettingsChanged: Boolean = false
    var pressy: Pressy = _
    var compilationCount = 0
    var (lastFrame, lastFrameVersion) = (headFrame, headFrame.version)
  }


  import Internal._


  // Public to expose it in the REPL so people can poke at it at runtime
  // Not for use within Ammonite! Use one of the other methods to ensure
  // that `Internal.compiler` is properly initialized before use.
  def compiler = Internal.compiler
  def compilationCount = Internal.compilationCount

  def pressy: Pressy = Internal.pressy

  def preprocessor(fileName: String) = synchronized{
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
      // Note we not only make a copy of `settings` to pass to the compiler,
      // we also make a *separate* copy to pass to the presentation compiler.
      // Otherwise activating autocomplete makes the presentation compiler mangle
      // the shared settings and makes the main compiler sad
      val settings = Option(compiler)
        .map(_.objCompiler.asInstanceOf[scala.tools.nsc.Global].settings.copy)
        .getOrElse(new Settings)
      onSettingsInit.foreach(_(settings))

      val initialClassPath = Classpath.classpath(initialClassLoader, rtCacheDir)
      val headFrameClassPath =
        Classpath.classpath(headFrame.classloader, rtCacheDir, stopAt = initialClassLoader)

      Internal.compiler = Compiler(
        headFrameClassPath,
        dynamicClasspath,
        headFrame.classloader,
        headFrame.pluginClassloader,
        () => shutdownPressy(),
        None,
        settings,
        classPathWhitelist,
        initialClassPath
      )

      onCompilerInit.foreach(_(compiler.objCompiler.asInstanceOf[scala.tools.nsc.Global]))

      // Pressy is lazy, so the actual presentation compiler won't get instantiated
      // & initialized until one of the methods on it is actually used
      Internal.pressy = Pressy(
        headFrameClassPath,
        dynamicClasspath,
        headFrame.classloader,
        settings.copy(),
        dependencyCompleteOpt(),
        classPathWhitelist,
        initialClassPath
      )

      Internal.preConfiguredSettingsChanged = false
    }
  }

  def completions(offset: Int, previousImports: String, snippet: String) = synchronized{
    init()
    val (idx, completions, something) = pressy.complete(offset, previousImports, snippet)
    entry(idx, entry(completions.toArray, something.toArray))
  }

  def compileClassOrNull(
    processed: ammonite.compiler.iface.Preprocessor.Output,
    printer: ammonite.compiler.iface.Logger,
    fileName: String
  ): ammonite.compiler.iface.Compiler.Output = synchronized{
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
    compiled.orNull
  }

  def configureCompiler(callback: scala.tools.nsc.Global => Unit) = synchronized{
    onCompilerInit.append(callback)
    if (compiler != null){
      callback(compiler.objCompiler.asInstanceOf[scala.tools.nsc.Global])
    }
  }

  def preConfigureCompiler(callback: scala.tools.nsc.Settings => Unit) =
    synchronized {
      onSettingsInit.append(callback)
      preConfiguredSettingsChanged = true
    }

  def addToClasspath(classFiles: Array[JMap.Entry[String, Array[Byte]]]): Unit = synchronized {
    val actualClassFiles = classFiles.map(e => (e.getKey, e.getValue))
    Compiler.addToClasspath(actualClassFiles, dynamicClasspath)
  }
  def objPressy = pressy.compiler
  // Not synchronized, since it's part of the exit sequence that needs to run
  // if the repl exits while the warmup code is compiling
  def shutdownPressy() = {
    if (pressy != null) pressy.shutdownPressy()
  }
}
