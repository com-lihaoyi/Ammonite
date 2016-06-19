package ammonite.repl.interp

import java.lang.reflect.InvocationTargetException

import acyclic.file
import ammonite.repl.frontend.{SessionChanged, Session, ReplExit}
import ammonite.repl._

import Util.{CompileCache, ClassFiles}

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.util.Try

/**
 * Evaluates already-compiled Bytecode.
  *
  * Deals with all the munging of `Classloader`s, `Class[_]` objects,
  * and `Array[Byte]`s representing class files, and reflection necessary
  * to take the already-compile Scala bytecode and execute it in our process.
 */
trait Evaluator{
  def loadClass(wrapperName: String, classFiles: ClassFiles): Res[Class[_]]
  def getCurrentLine: String
  def update(newImports: Imports): Unit

  def processLine(classFiles: Util.ClassFiles,
                  newImports: Imports,
                  printer: Printer,
                  fileName: String,
                  indexedWrapperName: String): Res[Evaluated]

  def processScriptBlock(cls: Class[_],
                         newImports: Imports,
                         wrapperName: String,
                         pkgName: String): Res[Evaluated]

  def sess: Session

}

object Evaluator{

  def interrupted(e: Throwable) = {
    Thread.interrupted()
    Res.Failure(Some(e), "\nInterrupted!")
  }

  def apply(currentClassloader: ClassLoader,
            startingLine: Int): Evaluator = new Evaluator{ eval =>


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

    /**
     * Performs the conversion of our pre-compiled `Array[Byte]`s into
     * actual classes with methods we can execute.
     */

    def initialFrame = {
      val hash = SpecialClassLoader.initialClasspathHash(currentClassloader)
      def special = new SpecialClassLoader(currentClassloader, hash)
      new Frame(
        special,
        special,
        Imports(Nil),
        Seq()
      )
    }
    var frames = List(initialFrame)

    val namedFrames = mutable.Map.empty[String, List[Frame]]

    object sess extends Session {
      def frames = eval.frames
      def childFrame(parent: Frame) = new Frame(
        new SpecialClassLoader(parent.classloader, parent.classloader.classpathHash),
        new SpecialClassLoader(parent.pluginClassloader, parent.pluginClassloader.classpathHash),
        parent.imports,
        parent.classpath
      )

      def save(name: String = "") = {
        if (name != "") namedFrames(name) = eval.frames
        eval.frames = childFrame(frames.head) :: frames
      }

      def pop(num: Int = 1) = {
        var next = eval.frames
        for(i <- 0 until num){
          if (next.tail != Nil) next = next.tail
        }
        val out = SessionChanged.delta(eval.frames.head, next.head)
        eval.frames = childFrame(next.head) :: next
        out
      }
      def load(name: String = "") = {
        val next = if (name == "") eval.frames.tail else namedFrames(name)
        val out = SessionChanged.delta(eval.frames.head, next.head)
        eval.frames = childFrame(next.head) :: next
        out
      }

      def delete(name: String) = {
        namedFrames.remove(name)
      }
    }




    def loadClass(fullName: String, classFiles: ClassFiles): Res[Class[_]] = {
      Res[Class[_]](Try {
        for ((name, bytes) <- classFiles) {
          sess.frames.head.classloader.newFileDict(name) = bytes
        }
        val names = classFiles.map(_._1)

        val res = Class.forName(fullName, true, sess.frames.head.classloader)
        res
      }, e => "Failed to load compiled class " + e)
    }


    def evalMain(cls: Class[_]) =
      cls.getDeclaredMethod("$main").invoke(null)



    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    val userCodeExceptionHandler: PartialFunction[Throwable, Res.Failing] = {
      // Exit
      case Ex(_: InvEx, _: InitEx, ReplExit(value))  =>
        Res.Exit(value)
      // Interrupted during pretty-printing
      case Ex(e: ThreadDeath)                 =>  interrupted(e)

      // Interrupted during evaluation
      case Ex(_: InvEx, e: ThreadDeath)       =>  interrupted(e)

      case Ex(_: InvEx, _: InitEx, userEx@_*) =>   Res.Exception(userEx(0), "")
      case Ex(_: InvEx, userEx@_*)            =>   Res.Exception(userEx(0), "")
      case Ex(userEx@_*)                      =>   Res.Exception(userEx(0), "")

    }

    def processLine(classFiles: Util.ClassFiles,
                    newImports: Imports,
                    printer: Printer,
                    fileName: String,
                    indexedWrapperName: String) = {
      Timer("eval.processLine compileClass end")
      for {
        cls <- loadClass("$sess." + indexedWrapperName, classFiles)
        _ = Timer("eval.processLine loadClass end")
        _ = currentLine += 1
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        // Exhaust the printer iterator now, before exiting the `Catching`
        // block, so any exceptions thrown get properly caught and handled

        val iter = evalMain(cls).asInstanceOf[Iterator[String]]
        Timer("eval.processLine evaluatorRunPrinter 1")
        evaluatorRunPrinter(iter.foreach(printer.out))
        Timer("eval.processLine evaluatorRunPrinter end")
        evaluationResult("$sess." + indexedWrapperName, newImports)
      }
    }


    def processScriptBlock(cls: Class[_],
                           newImports: Imports,
                           wrapperName: String,
                           pkgName: String) = for {
      _ <- Catching{userCodeExceptionHandler}
    } yield {
      Timer("cachedCompileBlock")
      evalMain(cls)
      Timer("evalMain")
      val res = evaluationResult(pkgName + "." + Parsers.backtickWrap(wrapperName), newImports)
      Timer("evaluationResult")
      res
    }


    def update(newImports: Imports) = {
      frames.head.addImports(newImports)
    }

    def evaluationResult(wrapperName: String,
                         imports: Imports) = {
      Evaluated(
        wrapperName,
        Imports(
          for(id <- imports.value) yield {
            val filledPrefix =
              if (id.prefix == "") {
                // For some reason, for things not-in-packages you can't access
                // them off of `_root_`
                wrapperName
              } else {
                id.prefix
              }
            val rootedPrefix =
              if (filledPrefix.startsWith("_root_.")) filledPrefix
              else "_root_." + filledPrefix

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
