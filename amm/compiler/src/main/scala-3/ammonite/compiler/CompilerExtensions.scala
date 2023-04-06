package ammonite.compiler

import ammonite.interp.api.InterpAPI
import ammonite.repl.api.ReplAPI

import java.nio.file.Path

object CompilerExtensions {

  implicit class CompilerInterpAPIExtensions(private val api: InterpAPI) extends AnyVal {

    private def compilerManager = api._compilerManager.asInstanceOf[CompilerLifecycleManager]

    /**
      * Configures the current compiler, or if the compiler hasn't been initialized
      * yet, registers the configuration callback and applies it to the compiler
      * when it ends up being initialized later
      */
    def configureCompiler(c: dotty.tools.dotc.Compiler => Unit): Unit =
      compilerManager.configureCompiler(c)

    /**
      * Pre-configures the next compiler context. Useful for tuning options that are
      * used during parsing.
      */
    def preConfigureCompiler(c: dotty.tools.dotc.core.Contexts.FreshContext => Unit): Unit =
      compilerManager.preConfigureCompiler(c)

    /**
      * Directory where the byte code resulting from compiling the user code is written.
      * This is non-empty only if the `--output-directory` or `--tmp-output-directory` options
      * are passed to Ammonite upon launch.
      */
    def outputDir: Option[Path] =
      compilerManager.outputDir
  }

  implicit class CompilerReplAPIExtensions(private val api: ReplAPI) extends AnyVal {

    private def compilerManager = api._compilerManager.asInstanceOf[CompilerLifecycleManager]

    def initialContext: dotty.tools.dotc.core.Contexts.Context =
      compilerManager.compiler.initialCtx
    /**
     * Access the compiler to do crazy things if you really want to!
     */
    def compiler: dotty.tools.dotc.Compiler =
      compilerManager.compiler.compiler
  }

}
