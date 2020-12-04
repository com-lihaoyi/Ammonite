package ammonite.interp.api

import ammonite.compiler.iface.ReplClassLoader
import coursierapi.{Fetch, Repository}

import scala.collection.JavaConverters._
import scala.collection.mutable

object InterpBridge extends APIHolder[InterpAPI]

object InterpExtras {
  implicit class InterpAPIExtensions(private val api: InterpAPI)
    extends AnyVal with ammonite.interp.api.doc.InterpAPI {

    def watchValue[T](v: => T): T = {
      api.addWatchValue(() => v)
      v
    }

    def repositories: mutable.Buffer[Repository] =
      api.repositoriesList.asScala
    def resolutionHooks: mutable.Buffer[java.util.function.Function[Fetch, Fetch]] =
      api.resolutionHooksList.asScala

    def beforeExitHooks: mutable.Buffer[java.util.function.Function[Object, Object]] =
      api.beforeExitHooksList.asScala

    def configureCompiler(c: scala.tools.nsc.Global => Unit): Unit =
      api
        .objCompilerLifeCycleManager
        .asInstanceOf[ammonite.compiler.CompilerLifecycleManager]
        .configureCompiler(c)
    def preConfigureCompiler(c: scala.tools.nsc.Settings => Unit): Unit =
      api
        .objCompilerLifeCycleManager
        .asInstanceOf[ammonite.compiler.CompilerLifecycleManager]
        .preConfigureCompiler(c)
  }

  implicit class ReplClassLoaderExtensions(private val cl: ReplClassLoader) extends AnyVal {
    def inMemoryClasses: Map[String, Array[Byte]] =
      cl.inMemoryClassesMap().asScala.toMap
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
