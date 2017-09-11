package ammonite.runtime

import java.lang.reflect.InvocationTargetException


import ammonite._
import util.Util.{ClassFiles, newLine}
import ammonite.util._

import scala.util.Try
import scala.util.control.ControlThrowable

/**
 * Evaluates already-compiled Bytecode.
  *
  * Deals with all the munging of `Classloader`s, `Class[_]` objects,
  * and `Array[Byte]`s representing class files, and reflection necessary
  * to take the already-compile Scala bytecode and execute it in our process.
 */
trait Evaluator{
  def loadClass(wrapperName: String, classFiles: ClassFiles): Res[Class[_]]
  def evalMain(cls: Class[_], contextClassloader: ClassLoader): Any


  def processLine(classFiles: ClassFiles,
                  newImports: Imports,
                  printer: Printer,
                  indexedWrapperName: Name,
                  silent: Boolean,
                  contextClassLoader: ClassLoader): Res[Evaluated]

  def processScriptBlock(cls: Class[_],
                         newImports: Imports,
                         wrapperName: Name,
                         pkgName: Seq[Name],
                         contextClassLoader: ClassLoader): Res[Evaluated]
}

object Evaluator{
  /**
    * Thrown to exit the REPL cleanly
    */
  case class AmmoniteExit(value: Any) extends ControlThrowable
  type InvEx = InvocationTargetException
  type InitEx = ExceptionInInitializerError

  /**
    * We unwrap many of the "common" cases where the user's actual
    * exception is wrapped in a bunch of InvocationTargetException
    * wrappers, since it's the users exception they probably care about
    */
  val userCodeExceptionHandler: PartialFunction[Throwable, Res.Failing] = {
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

  def apply(headFrame: => Frame): Evaluator = new Evaluator{ eval =>


    def loadClass(fullName: String, classFiles: ClassFiles): Res[Class[_]] = {
      Res[Class[_]](
        Try {
          for ((name, bytes) <- classFiles.sortBy(_._1)) {
            headFrame.classloader.addClassFile(name, bytes)
          }

          headFrame.classloader.findClass(fullName)
        },
        e =>"Failed to load compiled class " + e
      )
    }


    def evalMain(cls: Class[_], contextClassloader: ClassLoader) =
      Util.withContextClassloader(contextClassloader){
        cls.getDeclaredMethod("$main").invoke(null)
      }

    def processLine(classFiles: Util.ClassFiles,
                    newImports: Imports,
                    printer: Printer,
                    indexedWrapperName: Name,
                    silent: Boolean,
                    contextClassLoader: ClassLoader) = {
      for {
        cls <- loadClass("ammonite.$sess." + indexedWrapperName.backticked, classFiles)
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        // Exhaust the printer iterator now, before exiting the `Catching`
        // block, so any exceptions thrown get properly caught and handled
        val iter = evalMain(cls, contextClassLoader).asInstanceOf[Iterator[String]]

        if (!silent) evaluatorRunPrinter(iter.foreach(printer.resultStream.print))
        else evaluatorRunPrinter(iter.foreach(_ => ()))

        // "" Empty string as cache tag of repl code
        evaluationResult(Seq(Name("ammonite"), Name("$sess"), indexedWrapperName), newImports)
      }
    }


    def processScriptBlock(cls: Class[_],
                           newImports: Imports,
                           wrapperName: Name,
                           pkgName: Seq[Name],
                           contextClassLoader: ClassLoader) = {
      for {
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        evalMain(cls, contextClassLoader)
        val res = evaluationResult(pkgName :+ wrapperName, newImports)
        res
      }
    }

    def evaluationResult(wrapperName: Seq[Name],
                         imports: Imports) = {
      Evaluated(
        wrapperName,
        Imports(
          for(id <- imports.value) yield {
            val filledPrefix =
              if (id.prefix.isEmpty) {
                // For some reason, for things not-in-packages you can't access
                // them off of `_root_`
                wrapperName
              } else {
                id.prefix
              }
            val rootedPrefix: Seq[Name] =
              if (filledPrefix.headOption.exists(_.backticked == "_root_")) filledPrefix
              else Seq(Name("_root_")) ++ filledPrefix

            id.copy(prefix = rootedPrefix)
          }
        )
      )
    }
  }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter(f: => Unit) = f



}
