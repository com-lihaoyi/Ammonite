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
  def evalMain(cls: Class[_]): Any
  def getCurrentLine: String
  def update(newImports: Imports): Unit
  def withContextClassloader[T](t: => T): T
  def processLine(classFiles: ClassFiles,
                  newImports: Imports,
                  printer: Printer,
                  indexedWrapperName: Name): Res[Evaluated]

  def processScriptBlock(cls: Class[_],
                         newImports: Imports,
                         wrapperName: Name,
                         pkgName: Seq[Name]): Res[Evaluated]
  def evalClassloader: SpecialClassLoader
  def pluginClassloader: SpecialClassLoader
  def imports: Imports
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

  def apply(startingLine: Int, frames: => List[Frame]): Evaluator = new Evaluator{ eval =>

    def evalClassloader = frames.head.classloader
    def pluginClassloader= frames.head.pluginClassloader
    def imports = frames.head.imports

    /**
     * The current line number of the REPL, used to make sure every snippet
     * evaluated can have a distinct name that doesn't collide.
     */
    var currentLine = startingLine

    /**
     * Weird indirection only necessary because of
     * https://issues.scala-lang.org/browse/SI-7085
     */
    def getCurrentLine = currentLine.toString.replace("-", "_")

    def loadClass(fullName: String, classFiles: ClassFiles): Res[Class[_]] = {
      Res[Class[_]](Try {
        for ((name, bytes) <- classFiles.sortBy(_._1)) {
          frames.head.classloader.addClassFile(name, bytes)
        }
        val names = classFiles.map(_._1)
        val res = Class.forName(fullName, true, frames.head.classloader)
        res
      }, e => "Failed to load compiled class " + e)
    }


    def evalMain(cls: Class[_]) = withContextClassloader{
      cls.getDeclaredMethod("$main").invoke(null)
    }

    def withContextClassloader[T](t: => T) = {
      val oldClassloader = Thread.currentThread().getContextClassLoader
      try{
        Thread.currentThread().setContextClassLoader(eval.evalClassloader)
        t
      } finally {
        Thread.currentThread().setContextClassLoader(oldClassloader)
      }
    }



    def processLine(classFiles: Util.ClassFiles,
                    newImports: Imports,
                    printer: Printer,
                    indexedWrapperName: Name) = {
      for {
        cls <- loadClass("ammonite.$sess." + indexedWrapperName.backticked, classFiles)
        _ = currentLine += 1
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        // Exhaust the printer iterator now, before exiting the `Catching`
        // block, so any exceptions thrown get properly caught and handled
        val iter = evalMain(cls).asInstanceOf[Iterator[String]]
        evaluatorRunPrinter(iter.foreach(printer.out))

        // "" Empty string as cache tag of repl code
        evaluationResult(Seq(Name("ammonite"), Name("$sess"), indexedWrapperName), newImports)
      }
    }


    def processScriptBlock(cls: Class[_],
                           newImports: Imports,
                           wrapperName: Name,
                           pkgName: Seq[Name]) = {
      for {
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        evalMain(cls)
        val res = evaluationResult(pkgName :+ wrapperName, newImports)
        res
      }
    }

    def update(newImports: Imports) = {
      frames.head.addImports(newImports)
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
