package ammonite.interp.api

import coursierapi.{Fetch, Repository}

import scala.collection.JavaConverters._
import scala.collection.mutable

object InterpBridge extends APIHolder[InterpAPI]

object InterpExtras {
  implicit class InterpAPIExtensions(private val api: InterpAPI) extends AnyVal {

    /**
     * A generalization of [[watch]], allows watching arbitrary values and not
     * just the contents of file paths.
     */
    def watchValue[T](v: => T): T = {
      api.addWatchValue(() => v)
      v
    }

    /**
     * resolvers to use when loading jars
     */
    def repositories: mutable.Buffer[Repository] =
      api.repositoriesList.asScala

    /**
      * Functions that will be chained and called on the coursier
      * Fetch object right before they are run
      */
    def resolutionHooks: mutable.Buffer[java.util.function.Function[Fetch, Fetch]] =
      api.resolutionHooksList.asScala

    /**
      * Functions that will be chained and called on the
      * exitValue before the repl exits
      */
    def beforeExitHooks: mutable.Buffer[java.util.function.Function[Object, Object]] =
      api.beforeExitHooksList.asScala

    /**
      * Configures the current compiler, or if the compiler hasn't been initialized
      * yet, registers the configuration callback and applies it to the compiler
      * when it ends up being initialized later
      */
    def configureCompiler(c: scala.tools.nsc.Global => Unit): Unit =
      () // TODO

    /**
       * Pre-configures the next compiler. Useful for tuning options that are
       * used during parsing such as -Yrangepos
       */
    def preConfigureCompiler(c: scala.tools.nsc.Settings => Unit): Unit =
      () // TODO
  }

  /**
    * Exit the Ammonite REPL. You can also use Ctrl-D to exit
    */
  def exit: Nothing =
    throw new AmmoniteExit(())
  /**
    * Exit the Ammonite REPL. You can also use Ctrl-D to exit
    */
  def exit(value: Any): Nothing =
    throw new AmmoniteExit(value)
}
