package ammonite.interp

import java.lang.reflect.InvocationTargetException

import acyclic.file
import ammonite.frontend.{ReplExit, Session, SessionChanged}
import ammonite._
import util.Util.{ClassFiles, CompileCache}
import ammonite.util._

import scala.collection.immutable.ListMap
import scala.reflect.io.VirtualDirectory
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
  def evalMain(cls: Class[_]): Any
  def getCurrentLine: String
  def update(newImports: Imports): Unit

  def processLine(classFiles: ClassFiles,
                  newImports: Imports,
                  printer: Printer,
                  fileName: String,
                  indexedWrapperName: Name): Res[Evaluated]

  def processScriptBlock(cls: Class[_],
                         newImports: Imports,
                         wrapperName: Name,
                         pkgName: Seq[Name],
                         tag: String): Res[Evaluated]

  def sess: Session

  def evalCachedClassFiles(cachedData: Seq[ClassFiles],
                           pkg: String,
                           wrapper: String,
                           dynamicClasspath: VirtualDirectory,
                           classFilesList: Seq[String]): Res[Seq[_]]

}

object Evaluator{

  def interrupted(e: Throwable) = {
    Thread.interrupted()
    Res.Failure(Some(e), "\nInterrupted!")
  }

  def apply(currentClassloader: ClassLoader,
            startingLine: Int,
            timer: Timer): Evaluator = new Evaluator{ eval =>


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
      val hash = SpecialClassLoader.initialClasspathSignature(currentClassloader)
      def special = new SpecialClassLoader(currentClassloader, hash)
      new Frame(
        special,
        special,
        Imports(),
        Seq()
      )
    }
    var frames = List(initialFrame)

    val namedFrames = mutable.Map.empty[String, List[Frame]]

    object sess extends Session {
      def frames = eval.frames
      def childFrame(parent: Frame) = new Frame(
        new SpecialClassLoader(
          parent.classloader,
          parent.classloader.classpathSignature
        ),
        new SpecialClassLoader(
          parent.pluginClassloader,
          parent.pluginClassloader.classpathSignature
        ),
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
        for ((name, bytes) <- classFiles.sortBy(_._1)) {
          sess.frames.head.classloader.addClassFile(name, bytes)
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
                    indexedWrapperName: Name) = timer{
      for {
        cls <- loadClass("$sess." + indexedWrapperName.backticked, classFiles)
        _ = currentLine += 1
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        // Exhaust the printer iterator now, before exiting the `Catching`
        // block, so any exceptions thrown get properly caught and handled

        val iter = evalMain(cls).asInstanceOf[Iterator[String]]
        evaluatorRunPrinter(iter.foreach(printer.out))

        // "" Empty string as cache tag of repl code
        evaluationResult(Seq(Name("$sess"), indexedWrapperName), newImports, "")
      }
    }


    def processScriptBlock(cls: Class[_],
                           newImports: Imports,
                           wrapperName: Name,
                           pkgName: Seq[Name],
                           tag: String) = timer{
      for {
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        evalMain(cls)
        val res = evaluationResult(pkgName :+ wrapperName, newImports, tag)
        res
      }
    }

    def evalCachedClassFiles(cachedData: Seq[ClassFiles],
                             pkg: String,
                             wrapper: String,
                             dynamicClasspath: VirtualDirectory,
                             classFilesList: Seq[String]): Res[Seq[_]] = timer{
      val res = timer{
        Res.map(cachedData.zipWithIndex) {
          case (clsFiles, index) =>
            Compiler.addToClasspath(clsFiles, dynamicClasspath)
            eval.loadClass(classFilesList(index), clsFiles)
        }
      }

      val evaled = timer{
        try {
          for {
            r <- res
          } yield r.map(eval.evalMain(_))
        }
        catch userCodeExceptionHandler
      }
      evaled
    }

    def update(newImports: Imports) = {
      frames.head.addImports(newImports)
    }

    def evaluationResult(wrapperName: Seq[Name],
                         imports: Imports,
                         tag: String) = timer{
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
        ),
        tag
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
