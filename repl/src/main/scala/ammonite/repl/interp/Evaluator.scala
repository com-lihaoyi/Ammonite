package ammonite.repl.interp

import java.lang.reflect.InvocationTargetException

import acyclic.file
import ammonite.repl.frontend.{Session, ReplExit}
import ammonite.repl._

import Util.{CompileCache, ClassFiles}

import scala.collection.mutable
import scala.util.Try

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 */
trait Evaluator{
  def loadClass(wrapperName: String, classFiles: ClassFiles): Res[Class[_]]
  def getCurrentLine: String
  def update(newImports: Seq[ImportData]): Unit

  /**
   * Takes the preprocessed `code` and `printCode` and compiles/evals/runs/etc.
   * it to provide a result. Takes `printer` as a callback, instead of returning
   * the `Iterator` as part of the output, because printing can cause side effects
   * (e.g. for Streams which are lazily printed) and can fail with an exception!
   * passing in the callback ensures the printing is still done lazily, but within
   * the exception-handling block of the `Evaluator`
   */
  def processLine(code: String,
                  printCode: String,
                  printer: Iterator[String] => Unit,
                  extraImports: Seq[ImportData] = Seq()): Res[Evaluated]
  def processScriptBlock(code: String, scriptImports: Seq[ImportData]): Res[Evaluated]

  def previousImportBlock: String

  def sess: Session

  /*
   * How many wrappers has this instance compiled
   */ 
  def compilationCount: Int
}

object Evaluator{

  def apply(currentClassloader: ClassLoader,
            compile: => (Array[Byte], String => Unit) => Compiler.Output,
            startingLine: Int,
            cacheLoad: String => Option[CompileCache],
            cacheSave: (String, CompileCache) => Unit,
            addToCompilerClasspath:  => ClassFiles => Unit): Evaluator = new Evaluator{ eval =>

    /**
     * Imports which are required by earlier commands to the REPL. Imports
     * have a specified key, so that later imports of the same name (e.g.
     * defining a variable twice) can kick the earlier import out of the
     * map. Otherwise if you import the same name twice you get compile
     * errors instead of the desired shadowing.
     */
    def previousImports = frames.head.previousImports

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
      Frame(
        special,
        special,
        Map.empty[String, ImportData],
        Seq()
      )
    }
    var frames = List(initialFrame)

    val namedFrames = mutable.Map.empty[String, List[Frame]]

    object sess extends Session {
      def frames = eval.frames
      def childFrame(parent: Frame) = Frame(
        new SpecialClassLoader(parent.classloader, parent.classloader.classpathHash),
        new SpecialClassLoader(parent.pluginClassloader, parent.pluginClassloader.classpathHash),
        parent.previousImports,
        parent.extraJars
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
        Frame.delta(eval.frames.head, next.head)
        eval.frames = childFrame(next.head) :: next
      }
      def load(name: String = "") = {
        val next = if (name == "") eval.frames.tail else namedFrames(name)
        Frame.delta(eval.frames.head, next.head)
        eval.frames = childFrame(next.head) :: next
      }

      def delete(name: String) = {
        namedFrames.remove(name)
      }
    }

    private var _compilationCount = 0
    def compilationCount = _compilationCount

    def compileClass(code: String): Res[(ClassFiles, Seq[ImportData])] = for {
      (output, compiled) <- Res.Success{
        val output = mutable.Buffer.empty[String]
        val c = compile(code.getBytes, output.append(_))
        (output, c)
      }
      _ = _compilationCount += 1
      result <- Res[(ClassFiles, Seq[ImportData])](
        compiled, "Compilation Failed\n" + output.mkString("\n")
      )
    } yield result

    def loadClass(wrapperName: String, classFiles: ClassFiles): Res[Class[_]] = {
      Res[Class[_]](Try {
        for ((name, bytes) <- classFiles) {
//          println("loadClass " + name)
          sess.frames.head.classloader.newFileDict(name) = bytes
        }
        Class.forName(wrapperName , true, sess.frames.head.classloader)
      }, e => "Failed to load compiled class " + e)
    }


    def evalMain(cls: Class[_]) =
      cls.getDeclaredMethod("$main").invoke(null)

    def previousImportBlock = importBlock(previousImports.values.toSeq)

    def importBlock(importData: Seq[ImportData]) = {
      Timer("importBlock 0")
      val snippets = for {
        (prefix, allImports) <- importData.toList.groupBy(_.prefix)
        imports <- Util.transpose(allImports.groupBy(_.fromName).values.toList)
      } yield {
        // Don't import importable variables called `_`. They seem to
        // confuse Scala into thinking it's a wildcard even when it isn't
        imports.filter(_.fromName != "_") match{
          case Seq(imp) if imp.fromName == imp.toName =>
            s"import $prefix.${Parsers.backtickWrap(imp.fromName)}"
          case imports =>
            val lines = for (x <- imports) yield {
              if (x.fromName == x.toName)
                "\n  " + Parsers.backtickWrap(x.fromName)
              else {
                "\n  " + Parsers.backtickWrap(x.fromName) +
                " => " + (if (x.toName == "_") "_" else Parsers.backtickWrap(x.toName))
              }
            }
            val block = lines.mkString(",")
            s"import $prefix.{$block\n}"
        }
      }
      val res = snippets.mkString("\n")
      Timer("importBlock 1")
      res
    }
  
    def interrupted() = {
      Thread.interrupted()
      Res.Failure("\nInterrupted!")
    }

    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    def processLine(code: String,
                    printCode: String,
                    printer: Iterator[String] => Unit,
                    extraImports: Seq[ImportData] = Seq()) = for {
      wrapperName <- Res.Success("cmd" + getCurrentLine)
      _ <- Catching{ case e: ThreadDeath => interrupted() }
      (classFiles, newImports) <- compileClass(wrapCode(
        wrapperName,
        code,
        printCode,
        previousImports.values.toSeq ++ extraImports
      ))
      _ = Timer("eval.processLine compileClass end")
      cls <- loadClass(wrapperName, classFiles)
      _ = Timer("eval.processLine loadClass end")
      _ = currentLine += 1
      _ <- Catching{
        // Exit
        case Ex(_: InvEx, _: InitEx, ReplExit(value))  => Res.Exit(value)
        // Interrupted during pretty-printing
        case Ex(_: ThreadDeath)                 => interrupted()
        // Interrupted during evaluation
        case Ex(_: InvEx, _: ThreadDeath)       => interrupted()

        case Ex(_: InvEx, _: InitEx, userEx@_*) => Res.Exception(userEx(0), "")
        case Ex(_: InvEx, userEx@_*)            => Res.Exception(userEx(0), "")
        case Ex(userEx@_*)                      => Res.Exception(userEx(0), "")
      }
    } yield {
      // Exhaust the printer iterator now, before exiting the `Catching`
      // block, so any exceptions thrown get properly caught and handled

      val iter = evalMain(cls).asInstanceOf[Iterator[String]]
      Timer("eval.processLine evaluatorRunPrinter 1")
      evaluatorRunPrinter(printer(iter))
      Timer("eval.processLine evaluatorRunPrinter end")
      evaluationResult(wrapperName, newImports)
    }
    
    def wrapCode(wrapperName: String,
                 code: String,
                 printCode: String,
                 imports: Seq[ImportData]) = s"""
${importBlock(imports)}

object $wrapperName{
$code
  def $$main() = { $printCode }
  override def toString = "$wrapperName"
}
"""

    def cachedCompileBlock(code: String,
                           imports: Seq[ImportData],
                           printCode: String = ""): Res[(String, Class[_], Seq[ImportData])] = {
      Timer("cachedCompileBlock 0")
      val wrapperName = cacheTag(code, imports, sess.frames.head.classloader.classpathHash)
      Timer("cachedCompileBlock 1")
      val wrappedCode = wrapCode(wrapperName, code, printCode, imports)
      Timer("cachedCompileBlock 2")
      val compiled = cacheLoad(wrapperName) match {
        case Some((classFiles, newImports)) =>
          addToCompilerClasspath(classFiles)
          Res.Success((classFiles, newImports))
        case None => for {
          (classFiles, newImports) <- compileClass(wrappedCode)
          _ = cacheSave(wrapperName, (classFiles, newImports))
        } yield (classFiles, newImports)
      }
      Timer("cachedCompileBlock 3")
      for {
        (classFiles, newImports) <- compiled
        _ = Timer("cachedCompileBlock 4")
        cls <- loadClass(wrapperName, classFiles)
      } yield (wrapperName, cls, newImports)
    }

    def processScriptBlock(code: String, scriptImports: Seq[ImportData]) = for {
      (wrapperName, cls, newImports) <- cachedCompileBlock(code, scriptImports)
    } yield {
      Timer("cachedCompileBlock")
      evalMain(cls)
      Timer("evalMain")
      val res = evaluationResult(wrapperName, newImports)
      Timer("evaluationResult")
      res
    }

    def update(newImports: Seq[ImportData]) = {
      val newImportMap = for(i <- newImports) yield (i.toName -> i)
      frames.head.previousImports = previousImports ++ newImportMap
    }

    def evaluationResult(wrapperName: String, imports: Seq[ImportData]) = {
      Evaluated(
        wrapperName,
        imports.map(id => id.copy(
          wrapperName = wrapperName,
          prefix = if (id.prefix == "") wrapperName else id.prefix
        ))
      )
    }
  }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter(f: => Unit) = f

  /**
   * This gives our cache tags for compile caching. The cache tags are a hash
   * of classpath, previous commands (in-same-script), and the block-code.
   * Previous commands are hashed in the wrapper names, which are contained 
   * in imports, so we don't need to pass them explicitly.
   */
  def cacheTag(code: String, imports: Seq[ImportData], classpathHash: Array[Byte]): String = {
    val bytes = Util.md5Hash(Iterator(
      Util.md5Hash(Iterator(code.getBytes)),
      Util.md5Hash(imports.iterator.map(_.toString.getBytes)),
      classpathHash
    ))
    "cache" + bytes.map("%02x".format(_)).mkString //add prefix to make sure it begins with a letter
  }


}
