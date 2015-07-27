package ammonite.repl.interp

import java.lang.reflect.InvocationTargetException
import java.net.URL

import acyclic.file
import ammonite.repl.frontend.{ReplExit, ReplAPI}
import ammonite.repl._
import java.net.URLClassLoader
import java.security.MessageDigest

import ammonite.repl.interp.Evaluator.SpecialClassloader
import Util.{CompileCache, ClassFiles}

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
            addToCompilerClasspath:  => ClassFiles => Unit): Evaluator = new Evaluator{

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

    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    def processLine(code: String, printCode: String, printer: Iterator[String] => Unit) = for {
      wrapperName <- Res.Success("cmd" + getCurrentLine)
      _ <- Catching{ case e: ThreadDeath => interrupted() }
      (classFiles, newImports) <- compileClass(wrapCode(
        wrapperName,
        code,
        printCode,
        previousImports.values.toSeq
      ))

      cls <- loadClass(wrapperName, classFiles)

      _ = currentLine += 1
      _ <- Catching{
        // Exit
        case Ex(_: InvEx, _: InitEx, ReplExit)  => Res.Exit
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
      evaluatorRunPrinter(printer(evalMain(cls).asInstanceOf[Iterator[String]]))
      evaluationResult(wrapperName, newImports)
    }
    
    def wrapCode(wrapperName: String,
                 code: String,
                 printCode: String,
                 imports: Seq[ImportData]) = s"""
      $defaultImportBlock
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
      val wrapperName = cacheTag(code, imports, evalClassloader.classpathHash)
      val wrappedCode = wrapCode(wrapperName, code, printCode, imports)

      val compiled = cacheLoad(wrapperName) match {
        case Some((classFiles, newImports)) =>
          addToCompilerClasspath(classFiles)
          Res.Success((classFiles, newImports))
        case None => for {
          (classFiles, newImports) <- compileClass(wrappedCode)
          _ = cacheSave(wrapperName, (classFiles, imports))
        } yield (classFiles, newImports)
      }

      for {
        (classFiles, newImports) <- compiled
        cls <- loadClass(wrapperName, classFiles)
      } yield (wrapperName, cls, newImports)
    }

    def processScriptBlock(code: String, scriptImports: Seq[ImportData]) = for {
      (wrapperName, cls, newImports) <- cachedCompileBlock(code, scriptImports)
    } yield {
      evalMain(cls)
      evaluationResult(wrapperName, newImports)
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
   * This gives our cache tags for compile caching. The cache tags are a hash
   * of classpath, previous commands (in-same-script), and the block-code.
   * Previous commands are hashed in the wrapper names, which are contained 
   * in imports, so we don't need to pass them explicitly.
   */
  def cacheTag(code: String, imports: Seq[ImportData], classpathHash: Array[Byte]): String = {
    val bytes = md5Hash(md5Hash(code.getBytes) ++ md5Hash(imports.mkString.getBytes) ++ classpathHash)
    "cache" + bytes.map("%02x".format(_)).mkString //add prefix to make sure it begins with a letter
  }

  def md5Hash(data: Array[Byte]) = MessageDigest.getInstance("MD5").digest(data)

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
    def add(url: URL) = {
      _classpathHash = md5Hash(_classpathHash ++ jarHash(url))
      addURL(url)
    }

    private def jarHash(url: URL) = {
      val is = url.openStream
      val baos = new java.io.ByteArrayOutputStream
      try {
        val byteChunk = new Array[Byte](4096) 
        var n = 0
        n = is.read(byteChunk)
        while (n > 0) {
          baos.write(byteChunk, 0, n)
          n = is.read(byteChunk)
        }
      } finally {
        if (is != null) is.close()
      }  
      md5Hash(baos.toByteArray())
    }

    //we don't need to hash in classes from newFileDict, because cache tag depend on 
    //them implicitly throgh having their hash in the imports.
    private var _classpathHash = {
      val urls = getURLs
      if(!urls.isEmpty){
        urls.tail.foldLeft(jarHash(urls.head)){ (oldHash,jarURL) =>
          md5Hash(oldHash ++ jarHash(jarURL))
        }
      } else md5Hash(Array.empty)
    }
    def classpathHash: Array[Byte] = _classpathHash
  }
}
