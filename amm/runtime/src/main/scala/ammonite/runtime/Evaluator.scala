package ammonite.runtime

import java.lang.reflect.InvocationTargetException

import ammonite._
import ammonite.interp.api.AmmoniteExit
import util.Util.newLine
import ammonite.util._

import scala.util.Try

/**
 * Evaluates already-compiled Bytecode.
  *
  * Deals with all the munging of `Classloader`s, `Class[_]` objects,
  * and `Array[Byte]`s representing class files, and reflection necessary
  * to take the already-compile Scala bytecode and execute it in our process.
 */
trait Evaluator{
  def evalMain(wrapperName: String, contextClassloader: SpecialClassLoader): Res[Unit]

  def processLine(wrapperName: String,
                  printer: Printer,
                  silent: Boolean,
                  contextClassLoader: SpecialClassLoader): Res[Unit]
}

object Evaluator{
  private type InvEx = InvocationTargetException
  private type InitEx = ExceptionInInitializerError

  /**
    * We unwrap many of the "common" cases where the user's actual
    * exception is wrapped in a bunch of InvocationTargetException
    * wrappers, since it's the users exception they probably care about
    */
  private val userCodeExceptionHandler: PartialFunction[Throwable, Res.Failing] = {
    // Exit
    case Ex(_: InvEx, _: InitEx, AmmoniteExit(value))  => Res.Exit(value)

    // Interrupted during pretty-printing
    case Ex(e: ThreadDeath)                 =>  interrupted(e)

    // Interrupted during evaluation
    case Ex(_: InvEx, e: ThreadDeath)       =>  interrupted(e)

    case Ex(_: InvEx, _: InitEx, userEx@_*) => Res.Exception(userEx(0), "")
    case Ex(_: InvEx, userEx@_*)            => Res.Exception(userEx(0), "")
    case Ex(userEx@_*)                      => Res.Exception(userEx(0), "")
  }


  def interrupted(e: Throwable) = {
    Thread.interrupted()
    Res.Failure(newLine + "Interrupted! (`repl.lastException.printStackTrace` for details)")
  }

  case object Default extends Evaluator{


    private def loadClass(fullName: String, classLoader: SpecialClassLoader): Res[Class[_]] = {
      Res[Class[_]](
        Try {
          classLoader.findClass(fullName)
        },
        e =>"Failed to load compiled class " + e
      )
    }


    private def evalClass(cls: Class[_], contextClassloader: ClassLoader) =
      Util.withContextClassloader(contextClassloader){

        val (method, instance) =
          try {
            (cls.getDeclaredMethod("$main"), null)
          } catch {
            case e: NoSuchMethodException =>
              // Wrapper with very long names seem to require this
              try {
                val cls0 = contextClassloader.loadClass(cls.getName + "$")
                val inst = cls0.getDeclaredField("MODULE$").get(null)
                (cls0.getDeclaredMethod("$main"), inst)
              } catch {
                case _: ClassNotFoundException | _: NoSuchMethodException =>
                  throw e
              }
          }

        method.invoke(instance)
      }

    private def evalMainResult(
      wrapperName: String,
      contextClassloader: SpecialClassLoader
    ): Res[Any] =
      loadClass(wrapperName, contextClassloader).flatMap { cls =>
        try Res.Success(evalClass(cls, contextClassloader))
        catch Evaluator.userCodeExceptionHandler
      }

    def evalMain(wrapperName: String, contextClassloader: SpecialClassLoader): Res[Unit] =
      evalMainResult(wrapperName, contextClassloader).map(_ => ())

    def processLine(wrapperName: String,
                    printer: Printer,
                    silent: Boolean,
                    contextClassLoader: SpecialClassLoader) = {
      for {
        obj <- evalMainResult(wrapperName, contextClassLoader)
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        // Exhaust the printer iterator now, before exiting the `Catching`
        // block, so any exceptions thrown get properly caught and handled
        val iter = obj.asInstanceOf[Iterator[String]]

        if (!silent) evaluatorRunPrinter(iter.foreach(printer.resultStream.print))
        else evaluatorRunPrinter(iter.foreach(_ => ()))
      }
    }
  }

  case object PassThrough extends Evaluator {
    def evalMain(wrapperName: String, contextClassloader: SpecialClassLoader): Res[Unit] =
      Res.Success(())
    def processLine(wrapperName: String,
                    printer: Printer,
                    silent: Boolean,
                    contextClassLoader: SpecialClassLoader): Res[Unit] =
      Res.Success(())
  }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  private def evaluatorRunPrinter(f: => Unit) = f



}
