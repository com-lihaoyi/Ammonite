package ammonite.interp.api.doc

import java.util.function.Function

import scala.collection.mutable

trait InterpAPI extends Any {
  /**
   * A generalization of [[watch]], allows watching arbitrary values and not
   * just the contents of file paths.
   */
  def watchValue[T](v: => T): T

  /**
   * resolvers to use when loading jars
   */
  def repositories: mutable.Buffer[coursierapi.Repository]

  /**
    * Functions that will be chained and called on the coursier
    * Fetch object right before they are run
    */
  def resolutionHooks: mutable.Buffer[Function[coursierapi.Fetch, coursierapi.Fetch]]

  /**
    * Functions that will be chained and called on the
    * exitValue before the repl exits
    */
  def beforeExitHooks: mutable.Buffer[Function[Object, Object]]

  /**
    * Configures the current compiler, or if the compiler hasn't been initialized
    * yet, registers the configuration callback and applies it to the compiler
    * when it ends up being initialized later
    */
  def configureCompiler(c: scala.tools.nsc.Global => Unit): Unit

  /**
     * Pre-configures the next compiler. Useful for tuning options that are
     * used during parsing such as -Yrangepos
     */
  def preConfigureCompiler(c: scala.tools.nsc.Settings => Unit): Unit
}