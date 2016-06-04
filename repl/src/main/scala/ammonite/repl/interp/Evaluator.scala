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
                  printer: Printer,
                  fileName: String,
                  extraImports: Seq[ImportData] = Seq()
                 ): Res[Evaluated]

  def processScriptBlock(code: String,
                         scriptImports: Seq[ImportData],
                         printer: Printer,
                         wrapperName: String,
                         pkgName: String): Res[Evaluated]

  def previousImportBlock: String

  def sess: Session

  /*
   * How many wrappers has this instance compiled
   */ 
  def compilationCount: Int
}

object Evaluator{


  def apply(currentClassloader: ClassLoader,
            compile: => (Array[Byte], Printer, Int, String) => Compiler.Output,
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
        Seq.empty[ImportData],
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

    private var _compilationCount = 0
    def compilationCount = _compilationCount

    def compileClass(code: (String, Int),
                     printer: Printer,
                     fileName: String): Res[(ClassFiles, Seq[ImportData])] = for {
      compiled <- Res.Success{
        val c = compile(code._1.getBytes, printer, code._2, fileName)
//        pprint.log(c)
        c
      }
      _ = _compilationCount += 1
      (classfiles, imports) <- Res[(ClassFiles, Seq[ImportData])](
        compiled,
        "Compilation Failed"
      )
    } yield (classfiles, imports)

    def loadClass(fullName: String, classFiles: ClassFiles): Res[Class[_]] = {
      Res[Class[_]](Try {
        for ((name, bytes) <- classFiles) {
//          println("loadClass " + name)
          sess.frames.head.classloader.newFileDict(name) = bytes
        }
        val res = Class.forName(fullName , true, sess.frames.head.classloader)
        res
      }, e => "Failed to load compiled class " + e)
    }


    def evalMain(cls: Class[_]) =
      cls.getDeclaredMethod("$main").invoke(null)

    def previousImportBlock = importBlock(imports)

    def importBlock(importData: Seq[ImportData]) = {
      Timer("importBlock 0")

      // Group the remaining imports into sliding groups according to their
      // prefix, while still maintaining their ordering
      val grouped = mutable.Buffer[mutable.Buffer[ImportData]]()
      for(data <- importData){
        if (grouped.isEmpty) grouped.append(mutable.Buffer(data))
        else {
          val last = grouped.last.last

          // Start a new import if we're importing from somewhere else, or
          // we're importing the same thing from the same place but aliasing
          // it to a different name, since you can't import the same thing
          // twice in a single import statement
          val startNewImport =
            last.prefix != data.prefix || grouped.last.exists(_.fromName == data.fromName)

          if (startNewImport) grouped.append(mutable.Buffer(data))
          else grouped.last.append(data)
        }
      }
//      pprint.log(grouped)
      // Stringify everything
      val out = for(group <- grouped) yield {
        val printedGroup = for(item <- group) yield{
          if (item.fromName == item.toName) Parsers.backtickWrap(item.fromName)
          else s"${Parsers.backtickWrap(item.fromName)} => ${Parsers.backtickWrap(item.toName)}"
        }
        "import " + group.head.prefix + ".{\n  " + printedGroup.mkString(",\n  ") + "\n}\n"
      }
      val res = out.mkString

      Timer("importBlock 1")
      pprint.log(res)
      res
    }

    def interrupted(e: Throwable) = {
      Thread.interrupted()
      Res.Failure(Some(e), "\nInterrupted!")
    }

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
    def processLine(code: String,
                    printCode: String,
                    printer: Printer,
                    fileName: String,
                    extraImports: Seq[ImportData] = Seq()
                   ) = for {
      wrapperName <- Res.Success("cmd" + getCurrentLine)
      _ <- Catching{ case e: ThreadDeath => interrupted(e) }
//      _ = pprint.log(this.imports)
      (classFiles, newImports) <- compileClass(
        wrapCode(
          None,
          wrapperName,
          code,
          printCode,
          Frame.mergeImports(imports, extraImports)
        ),
        printer,
        fileName
      )
      _ = Timer("eval.processLine compileClass end")
      cls <- loadClass(wrapperName, classFiles)
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
      evaluationResult(wrapperName, newImports)
    }

    def wrapCode(pkgName: Option[String],
                 wrapperName: String,
                 code: String,
                 printCode: String,
                 imports: Seq[ImportData]) = {
      val topWrapper = s"""
${pkgName.fold("")("package " + _)}
${importBlock(imports)}

object $wrapperName{\n"""

     val bottomWrapper = s"""\ndef $$main() = { $printCode }
  override def toString = "$wrapperName"
}
"""
     val importsLen = topWrapper.length
     (topWrapper + code + bottomWrapper, importsLen)
    }

    def cachedCompileBlock(code: String,
                           imports: Seq[ImportData],
                           printer: Printer,
                           wrapperName: String,
                           pkgName: String,
                           printCode: String = ""): Res[(Class[_], Seq[ImportData])] = {

      Timer("cachedCompileBlock 1")
      val wrappedCode = wrapCode(
        Some(pkgName), wrapperName, code, printCode, imports
      )
      Timer("cachedCompileBlock 2")
      val compiled = cacheLoad(pkgName + "." + wrapperName) match {
        case Some((classFiles, newImports)) if false =>
//          pprint.log("cacheLoad Some")
          addToCompilerClasspath(classFiles)
          Res.Success((classFiles, newImports))
        case _ =>
//          pprint.log("cacheLoad None")
          val noneCalc = for {
            (classFiles, newImports) <- compileClass(
              wrappedCode, printer, wrapperName + ".scala"
            )
            _ = cacheSave(pkgName + "." + wrapperName, (classFiles, newImports))
          } yield (classFiles, newImports)

          noneCalc
      }
      Timer("cachedCompileBlock 3")
//      pprint.log(compiled)
      val x = for {
        (classFiles, newImports) <- compiled
        _ = Timer("cachedCompileBlock 4")
        _ = pprint.log(newImports)
        cls <- loadClass(pkgName + "." + wrapperName, classFiles)
      } yield (cls, newImports)

      x
    }

    def processScriptBlock(code: String,
                           scriptImports: Seq[ImportData],
                           printer: Printer,
                           wrapperName: String,
                           pkgName: String) = for {
      (cls, newImports) <- cachedCompileBlock(
        code, scriptImports,  printer, wrapperName, pkgName
      )
      _ <- Catching{userCodeExceptionHandler}
    } yield {
      Timer("cachedCompileBlock")
      evalMain(cls)
      Timer("evalMain")
      val res = evaluationResult(wrapperName, newImports)
      Timer("evaluationResult")
      res
    }


    def update(newImports: Seq[ImportData]) = {
      frames.head.addImports(newImports)
    }

    def evaluationResult(wrapperName: String,
                         imports: Seq[ImportData]) = {
      Evaluated(
        wrapperName,
        imports.map(id => id.copy(
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
