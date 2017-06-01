package ammonite.interp

import java.io.File

import ammonite.runtime._
import ammonite.util.Util._
import ammonite.util._

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
class CompilerLifecycleManager(frames0: Ref[List[Frame]]){



  private[this] object Internal{
    val dynamicClasspath = new VirtualDirectory("(memory)", None)
    var compiler: Compiler = null
    var compilerStale: Boolean = true
    val onCompilerInit = mutable.Buffer.empty[scala.tools.nsc.Global => Unit]
    var pressy: Pressy = _
    var compilationCount = 0
    var frames = frames0

    // We lazily force the compiler to be re-initialized by setting the
    // compilerStale flag. Otherwise, if we re-initialized the compiler eagerly,
    // we end up sometimes re-initializing it multiple times unnecessarily before
    // it gets even used once. Empirically, this cuts down the number of compiler
    // re-initializations by about 2/3, each of which costs about 30ms and
    // probably creates a pile of garbage
    def reInit() = {
      Internal.compilerStale = true
    }
  }

  import Internal._

  val frames = new StableRef[List[Frame]] {
    def apply() = Internal.frames()
    def update(t: List[Frame]) = {
      Internal.frames() = t
      reInit()
    }
  }



  def pressy = Internal.pressy
  def compiler = Internal.compiler
  def compilationCount = Internal.compilationCount
  def preprocess = {
    if (compiler == null) init()
    Preprocessor(compiler.parse)
  }
  def evalClassloader = frames().head.classloader


  def init(force: Boolean = false) = if(Internal.compilerStale || force){
    Internal.compilerStale = false
    // Note we not only make a copy of `settings` to pass to the compiler,
    // we also make a *separate* copy to pass to the presentation compiler.
    // Otherwise activating autocomplete makes the presentation compiler mangle
    // the shared settings and makes the main compiler sad
    val settings = Option(compiler).fold(new Settings)(_.compiler.settings.copy)
    Internal.compiler = Compiler(
      Classpath.classpath ++ frames().head.classpath,
      dynamicClasspath,
      evalClassloader,
      frames().head.pluginClassloader,
      () => pressy.shutdownPressy(),
      settings
    )

    onCompilerInit.foreach(_(compiler.compiler))

    Internal.pressy = Pressy(
      Classpath.classpath ++ frames().head.classpath,
      dynamicClasspath,
      evalClassloader,

      settings.copy()
    )
  }

  def search(target: scala.reflect.runtime.universe.Type) = {
    init()
    compiler.search(target)
  }
  def compileClass(processed: Preprocessor.Output,
                   printer: Printer,
                   fileName: String): Res[(Util.ClassFiles, Imports)] = {
    init()
    for {
      compiled <- Res.Success{
        compiler.compile(processed.code.getBytes, printer, processed.prefixCharLength, fileName)
      }
      _ = Internal.compilationCount += 1
      (classfiles, imports) <- Res[(Util.ClassFiles, Imports)](
        compiled,
        "Compilation Failed"
      )
    } yield {
      (classfiles, imports)
    }
  }

  def configureCompiler(callback: scala.tools.nsc.Global => Unit) = {
    onCompilerInit.append(callback)
    if (compiler != null){
      callback(compiler.compiler)
    }
  }

  def addToClasspath(classFiles: ClassFiles) = {
    Compiler.addToClasspath(classFiles, dynamicClasspath)
    reInit()
  }
  def shutdownPressy() = pressy.shutdownPressy()

  def handleEvalClasspath(jar: File) = {
    frames().head.addClasspath(Seq(jar))
    evalClassloader.add(jar.toURI.toURL)
    reInit()
  }
  def handlePluginClasspath(jar: File) = {
    frames().head.pluginClassloader.add(jar.toURI.toURL)
    reInit()
  }
}



