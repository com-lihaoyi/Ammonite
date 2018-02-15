package ammonite.interp

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
class CompilerLifecycleManager(headFrame: => Frame){



  private[this] object Internal{
    val dynamicClasspath = new VirtualDirectory("(memory)", None)
    var compiler: Compiler = null
    var dynamicClasspathChanged: Boolean = true
    var pressyStale: Boolean = true
    val onCompilerInit = mutable.Buffer.empty[scala.tools.nsc.Global => Unit]
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

  def preprocess(fileName: String) = synchronized{
    if (compiler == null) init()
    Preprocessor(compiler.parse(fileName, _))
  }


  // We lazily force the compiler to be re-initialized by setting the
  // compilerStale flag. Otherwise, if we re-initialized the compiler eagerly,
  // we end up sometimes re-initializing it multiple times unnecessarily before
  // it gets even used once. Empirically, this cuts down the number of compiler
  // re-initializations by about 2/3, each of which costs about 30ms and
  // probably creates a pile of garbage

  def init(force: Boolean = false) = synchronized{
    if((headFrame ne lastFrame) ||
      headFrame.version != lastFrameVersion ||
      Internal.dynamicClasspathChanged ||
      force){

      lastFrame = headFrame
      lastFrameVersion = headFrame.version
      // Note we not only make a copy of `settings` to pass to the compiler,
      // we also make a *separate* copy to pass to the presentation compiler.
      // Otherwise activating autocomplete makes the presentation compiler mangle
      // the shared settings and makes the main compiler sad
      val settings = Option(compiler).fold(new Settings)(_.compiler.settings.copy)
      Internal.compiler = Compiler(
        Classpath.classpath ++ headFrame.classpath,
        dynamicClasspath,
        headFrame.classloader,
        headFrame.pluginClassloader,
        () => shutdownPressy(),
        settings
      )

      onCompilerInit.foreach(_(compiler.compiler))

      // Pressy is lazy, so the actual presentation compiler won't get instantiated
      // & initialized until one of the methods on it is actually used
      Internal.pressy = Pressy(
        Classpath.classpath ++ headFrame.classpath,
        dynamicClasspath,
        headFrame.classloader,

        settings.copy()
      )

      // Do this last; that way, if someone `Ctrl C`s in the middle of the
      // operation, we end up with `compilerStale = true` and a `compiler != null`,
      // which is better than `compilerStale = false` and `compiler == null`
      // because the first case means we redundantly re-initialize the compiler,
      // while the second means we're stuck without a compiler when we need one
      // and everything blows up
      Internal.dynamicClasspathChanged = false
    }
  }

  def complete(offset: Int, previousImports: String, snippet: String) = synchronized{
    init()
    pressy.complete(offset, previousImports, snippet)
  }

  def search(target: scala.reflect.runtime.universe.Type) = synchronized{
    init()
    compiler.search(target)
  }

  def compileClass(processed: Preprocessor.Output,
                   printer: Printer,
                   fileName: String): Res[Compiler.Output] = synchronized{
    // Enforce the invariant that every piece of code Ammonite ever compiles,
    // gets run within the `ammonite` package. It's further namespaced into
    // things like `ammonite.$file` or `ammonite.$sess`, but it has to be
    // within `ammonite`
    assert(processed.code.trim.startsWith("package ammonite"))

    init()
    for {
      compiled <- Res.Success{
        compiler.compile(
          processed.code.getBytes,
          printer,
          processed.prefixCharLength,
          processed.userCodeNestingLevel,
          fileName
        )
      }
      _ = Internal.compilationCount += 1
      output <- Res(compiled, "Compilation Failed")
    } yield output

  }

  def configureCompiler(callback: scala.tools.nsc.Global => Unit) = synchronized{
    onCompilerInit.append(callback)
    if (compiler != null){
      callback(compiler.compiler)
    }
  }

  def addToClasspath(classFiles: ClassFiles) = synchronized{
    Compiler.addToClasspath(classFiles, dynamicClasspath)
    dynamicClasspathChanged = true
  }
  // Not synchronized, since it's part of the exit sequence that needs to run
  // if the repl exits while the warmup code is compiling
  def shutdownPressy() = {
    if (pressy != null) pressy.shutdownPressy()
  }
}



