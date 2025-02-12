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
    def configureCompiler(c: scala.tools.nsc.Global => Unit): Unit =
      compilerManager.configureCompiler(c)

    /**
     * Pre-configures the next compiler. Useful for tuning options that are
     * used during parsing such as -Yrangepos
     */
    def preConfigureCompiler(c: scala.tools.nsc.Settings => Unit): Unit =
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

    /**
     * Access the compiler to do crazy things if you really want to!
     */
    def compiler: scala.tools.nsc.Global =
      compilerManager.compiler.compiler

    /**
     * Access the presentation compiler to do even crazier things if you really want to!
     */
    def interactiveCompiler: scala.tools.nsc.interactive.Global =
      compilerManager.pressy.compiler
  }

}
