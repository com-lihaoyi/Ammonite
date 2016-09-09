package ammonite.runtime

import java.io.OutputStream
import java.lang.reflect.InvocationTargetException

import ammonite._
import util.Util.newLine
import ammonite.util._
import ammonite.kernel.kernel.ClassFiles

import scala.reflect.io.VirtualDirectory

import scalaz.{Name => _, _}
import Scalaz._
import ammonite.kernel.LogError

import java.io.{StringWriter, PrintWriter}

/**
  * Evaluates already-compiled Bytecode.
  *
  * Deals with all the munging of `Classloader`s, `Class[_]` objects,
  * and `Array[Byte]`s representing class files, and reflection necessary
  * to take the already-compile Scala bytecode and execute it in our process.
  */
class Evaluator(currentClassloader: ClassLoader, startingLine: Int) {

  import Evaluator._

  private def evaluatorLoadError(t: Throwable): LogError = {
    val sw = new StringWriter();
    val pw = new PrintWriter(sw);
    t.printStackTrace(pw);
    val msg = sw.toString();
    sw.close()
    pw.close()
    LogError(msg)
  }

  def loadClass(fullName: String, classFiles: ClassFiles): Validation[LogError, Class[_]] = {
    val raw = Validation.fromTryCatchNonFatal {
      for ((name, bytes) <- classFiles.sortBy(_._1)) {
        frames.head.classloader.addClassFile(name, bytes)
      }
      Class.forName(fullName, true, frames.head.classloader)
    }
    raw leftMap evaluatorLoadError
  }

  private def evalMain(cls: Class[_]): Any = cls.getDeclaredMethod("$main").invoke(null)

  /**
    * The current line number of the REPL, used to make sure every snippet
    * evaluated can have a distinct name that doesn't collide.
    */
  private var currentLine = startingLine

  /**
    * Weird indirection only necessary because of
    * https://issues.scala-lang.org/browse/SI-7085
    */
  def getCurrentLine = currentLine.toString.replace("-", "_")

  def update(newImports: Imports): Unit = frames.head.addImports(newImports)

  def processLine(classFiles: ClassFiles,
                  newImports: Imports,
                  printer: PrinterX,
                  fileName: String,
                  indexedWrapperName: Name): Validation[LogError, Evaluated] = {

    val loadedClass = loadClass("$sess." + indexedWrapperName.backticked, classFiles)
    if (loadedClass.isSuccess) {
      currentLine += 1
    }
    loadedClass map { cls =>
      val iter = evalMain(cls).asInstanceOf[Iterator[String]]
      evaluatorRunPrinter(iter.foreach(printer.out))
      evaluationResult(Seq(Name("$sess"), indexedWrapperName), newImports, "")
    }
  }

  def processScriptBlock(cls: Class[_],
                         newImports: Imports,
                         wrapperName: Name,
                         pkgName: Seq[Name],
                         tag: String): Res[Evaluated] = {
    for {
      _ <- Catching { userCodeExceptionHandler }
    } yield {
      evalMain(cls)
      val res = evaluationResult(pkgName :+ wrapperName, newImports, tag)
      res
    }
  }

  /**
    * Performs the conversion of our pre-compiled `Array[Byte]`s into
    * actual classes with methods we can execute.
    */
  def initialFrame = {
    val hash =
      SpecialClassLoader.initialClasspathSignature(currentClassloader)
    def special = new SpecialClassLoader(currentClassloader, hash)
    new Frame(
      special,
      special,
      Imports(),
      Seq()
    )
  }

  var frames = List(initialFrame)

  def evalCachedClassFiles(cachedData: Seq[ClassFiles],
                           pkg: String,
                           wrapper: String,
                           dynamicClasspath: VirtualDirectory,
                           classFilesList: Seq[String]): ValidationNel[LogError, Seq[Any]] = {

    def composedFunction(classFiles: ClassFiles, idx: Int): ValidationNel[LogError, Any] = {
      addToClasspath(classFiles, dynamicClasspath)
      val evaluated = loadClass(classFilesList(idx), classFiles) map (evalMain _)
      evaluated.toValidationNel
    }

    cachedData.zipWithIndex.toList.traverseU(x => composedFunction(x._1, x._2))
  }

}

object Evaluator {

  private val userCodeExceptionHandler: PartialFunction[Throwable, Res.Failing] = {
    // Exit
    case Ex(_: InvEx, _: InitEx, ReplExit(value)) => Res.Exit(value)

    // Interrupted during pretty-printing
    case Ex(e: ThreadDeath) => interrupted(e)

    // Interrupted during evaluation
    case Ex(_: InvEx, e: ThreadDeath) => interrupted(e)

    case Ex(_: InvEx, _: InitEx, userEx @ _ *) =>
      Res.Exception(userEx(0), "")
    case Ex(_: InvEx, userEx @ _ *) => Res.Exception(userEx(0), "")
    case Ex(userEx @ _ *) => Res.Exception(userEx(0), "")
  }

  private def evaluationResult(wrapperName: Seq[Name], imports: Imports, tag: String) = {
    Evaluated(
      wrapperName,
      Imports(
        for (id <- imports.value) yield {
          val filledPrefix =
            if (id.prefix.isEmpty) {
              // For some reason, for things not-in-packages you can't access
              // them off of `_root_`
              wrapperName
            } else {
              id.prefix
            }
          val rootedPrefix: Seq[Name] =
            if (filledPrefix.headOption.exists(_.backticked == "_root_"))
              filledPrefix
            else Seq(Name("_root_")) ++ filledPrefix

          id.copy(prefix = rootedPrefix)
        }
      ),
      tag
    )
  }

  /**
    * Dummy function used to mark this method call in the stack trace,
    * so we can easily cut out the irrelevant part of the trace when
    * showing it to the user.
    */
  private def evaluatorRunPrinter(f: => Unit) = f

  def interrupted(e: Throwable) = {
    Thread.interrupted()
    Res.Failure(Some(e), newLine + "Interrupted!")
  }

  private type InvEx = InvocationTargetException

  private type InitEx = ExceptionInInitializerError

  def writeDeep(d: VirtualDirectory, path: List[String], suffix: String): OutputStream = (path: @unchecked) match {
    case head :: Nil => d.fileNamed(path.head + suffix).output
    case head :: rest =>
      writeDeep(
        d.subdirectoryNamed(head).asInstanceOf[VirtualDirectory],
        rest,
        suffix
      )
  }

  /**
    * Writes files to dynamicClasspath. Needed for loading cached classes.
    */
  def addToClasspath(classFiles: Traversable[(String, Array[Byte])], dynamicClasspath: VirtualDirectory): Unit = {
    for ((name, bytes) <- classFiles) {
      val output =
        writeDeep(dynamicClasspath, name.split('.').toList, ".class")
      output.write(bytes)
      output.close()
    }
  }

}
