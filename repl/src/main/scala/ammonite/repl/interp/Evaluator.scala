package ammonite.repl.interp

import java.lang.reflect.InvocationTargetException
import java.net.URL

import acyclic.file
import ammonite.repl.frontend.{ReplExit, ReplAPI}
import ammonite.repl._
import java.net.URLClassLoader
import java.security.MessageDigest

import ammonite.repl.interp.Evaluator.SpecialClassloader
import Util.CompileCache

import scala.reflect.runtime.universe._
import scala.collection.mutable
import scala.util.Try

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 */
trait Evaluator{
  def evalClass(code: String, wrapperName: String): Res[(Class[_], Seq[ImportData])]

  /**
   * _2, _1, 0, 1, 2, 3...
   *
   * Numbers starting from _ are used to represent predefined commands
   */
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
  def processLine(code: String, printCode: String, printer: Iterator[String] => Unit): Res[Evaluated]
  def processScriptBlock(code: String, scriptImports: Seq[ImportData]): Res[Evaluated]

  def previousImportBlock: String
  def addJar(url: URL): Unit
  def evalClassloader: SpecialClassloader

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
            addToCompilerClasspath:  => Traversable[(String,Array[Byte])] => Unit): Evaluator = new Evaluator{

    /**
     * Imports which are required by earlier commands to the REPL. Imports
     * have a specified key, so that later imports of the same name (e.g.
     * defining a variable twice) can kick the earlier import out of the
     * map. Otherwise if you import the same name twice you get compile
     * errors instead of the desired shadowing.
     */
    lazy val previousImports = mutable.Map(defaultImports.toSeq :_*)

    lazy val defaultImports = {
      def namesFor(t: scala.reflect.runtime.universe.Type): Set[String] = {
        val yours = t.members.map(_.name.toString).toSet
        val default = typeOf[Object].members.map(_.name.toString)
        yours -- default
      }

      def importsFor[T: TypeTag](name: String) = {
        namesFor(typeOf[T]).map(n => n -> ImportData(n, n, "", name)).toSeq
      }

      def importFrom(src: String, name: String) = {
        src -> ImportData(name, name, "", src)
      }

      mutable.Map(
        importsFor[ReplAPI]("ReplBridge.repl") ++
        importsFor[ammonite.repl.IvyConstructor]("ammonite.repl.IvyConstructor") ++
        Seq(
          importFrom("pprint", "pprintln"),
          importFrom("ReplBridge", "repl")
        )
        :_*
      )
    }

    lazy val defaultImportBlock = importBlock(defaultImports.values.toSeq)

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
    var evalClassloader: SpecialClassloader = null

    evalClassloader = new SpecialClassloader(currentClassloader)

    def addJar(url: URL) = evalClassloader.add(url)

    def evalClass(code: String, wrapperName: String) = for{
      (classFiles, importData) <- compileClass(code)
      cls <- loadClass(wrapperName, classFiles)
    } yield (cls, importData)

    private var _compilationCount = 0
    def compilationCount = _compilationCount

    def compileClass(code: String): Res[(Traversable[(String, Array[Byte])], Seq[ImportData])] = for {
      (output, compiled) <- Res.Success{
        val output = mutable.Buffer.empty[String]
        val c = compile(code.getBytes, output.append(_))
        (output, c)
      }
      _ = _compilationCount += 1
      result <- Res[(Traversable[(String, Array[Byte])], Seq[ImportData])](
        compiled, "Compilation Failed\n" + output.mkString("\n")
      )
    } yield result

    def loadClass(wrapperName: String, classFiles: Traversable[(String, Array[Byte])]): Res[Class[_]] = {
      Res[Class[_]](Try {
        for ((name, bytes) <- classFiles) evalClassloader.newFileDict(name) = bytes
        Class.forName(wrapperName , true, evalClassloader)
      }, e => "Failed to load compiled class " + e)
    }

    def evalMain(cls: Class[_]) =
      cls.getDeclaredMethod("$main").invoke(null)

    def previousImportBlock = importBlock(previousImports.values.toSeq)

    def importBlock(importData: Seq[ImportData]) = {
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
              else
                "\n  " + Parsers.backtickWrap(x.fromName) + " => " + (if (x.toName == "_") "_" else Parsers.backtickWrap(x.toName))

            }
            val block = lines.mkString(",")
            s"import $prefix.{$block\n}"
        }
      }
      snippets.mkString("\n")
    }
    def interrupted() = {
      Thread.interrupted()
      Res.Failure("\nInterrupted!")
    }

    def generateWrapper(wrapperName: String, imports: String, init: String, main: String) = {
      s"""
        $imports

        case object $wrapperName{
          $init
          def $$main() = { $main }
        }
        """
    }

    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    def processLine(code: String, printCode: String, printer: Iterator[String] => Unit) = for {
      wrapperName <- Res.Success("cmd" + getCurrentLine)
      _ <- Catching{ case e: ThreadDeath => interrupted() }
      (cls, newImports) <- evalClass(
        generateWrapper(
          wrapperName,
          previousImportBlock,
          code,
          printCode
        ),
        wrapperName
      )
      _ = currentLine += 1
      _ <- Catching{
        // Exit
        case Ex(_: InvEx, _: InitEx, ReplExit)  => Res.Exit
        // Interrupted during pretty-printing
        case Ex(_: ThreadDeath)                 => interrupted()
        // Interrupted during evaluation
        case Ex(_: InvEx, _: ThreadDeath)       => interrupted()

        case Ex(_: InvEx, _: InitEx, userEx@_*) => Res.Exception(userEx(0), ""/*, stop = "$main"*/)
        case Ex(_: InvEx, userEx@_*)            => Res.Exception(userEx(0), ""/*, stop = "$main"*/)
        case Ex(userEx@_*)                      => Res.Exception(userEx(0), ""/*, stop = "evaluatorRunPrinter"*/)
      }
    } yield {
      // Exhaust the printer iterator now, before exiting the `Catching`
      // block, so any exceptions thrown get properly caught and handled
      evaluatorRunPrinter(printer(evalMain(cls).asInstanceOf[Iterator[String]]))
      evaluationResult(wrapperName, newImports)
    }

    def compileCacheBlock(wrapperName: String, code: String, scriptImports: Seq[ImportData]): Res[(Class[_], Seq[ImportData])] = for {
      _ <- Catching{ case e: ThreadDeath => interrupted() }
      (classFiles, importData) <- compileClass(
        generateWrapper(
          wrapperName,
          defaultImportBlock + "\n" + importBlock(scriptImports),
          code,
          ""
        )
      )
      _ = cacheSave(wrapperName,(classFiles,importData))
      cls <- loadClass(wrapperName,classFiles)
    } yield (cls, importData)

    def processScriptBlock(code: String, scriptImports: Seq[ImportData]) = {
      val wrapperName = cacheTag(code, scriptImports, evalClassloader.getURLs.mkString(" "))
      loadCachedClass(wrapperName) match {
        case Some((cls,importData)) =>
          evalMain(cls)
          Res.Success(evaluationResult(wrapperName, importData))
        case None =>
          for {
            (cls, newImports) <- compileCacheBlock(wrapperName, code, scriptImports)
          } yield {
            evalMain(cls)
            evaluationResult(wrapperName, newImports)
          }
      }
    }

    def loadCachedClass(tag: String): Option[(Class[_],Seq[ImportData])] = {
      cacheLoad(tag).flatMap{ case (classFiles, importData) =>
        addToCompilerClasspath(classFiles)
        loadClass(tag,classFiles) match {
          case Res.Success(cls) => Some((cls,importData))
          case _ => None
        }
      }
    }

    def update(newImports: Seq[ImportData]) = {
      for(i <- newImports) previousImports(i.toName) = i
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
   * This gives our cache tags for compile caching.
   */
  def cacheTag(code: String, imports: Seq[ImportData], classpath: String): String = {
    val idString = code + imports.mkString(" ") + classpath
    val bytes = MessageDigest.getInstance("MD5").digest(idString.getBytes)
    "cache" + bytes.map("%02x".format(_)).mkString //add prefix to make sure it begins with a letter
  }

  /**
   * Classloader used to implement the jar-downloading
   * command-evaluating logic in Ammonite.
   *
   * http://stackoverflow.com/questions/3544614/how-is-the-control-flow-to-findclass-of
   */
  class SpecialClassloader(parent: ClassLoader) extends URLClassLoader(Array(), parent){
    /**
     * Files which have been compiled, stored so that our special
     * classloader can get at them.
     */
    val newFileDict = mutable.Map.empty[String, Array[Byte]]
    override def findClass(name: String): Class[_] = {
      def loadedFromBytes =
        for(bytes <- newFileDict.get(name))
        yield defineClass(name, bytes, 0, bytes.length)

      loadedFromBytes.getOrElse(super.findClass(name))
    }
    def add(url: URL) = addURL(url)
  }
}
