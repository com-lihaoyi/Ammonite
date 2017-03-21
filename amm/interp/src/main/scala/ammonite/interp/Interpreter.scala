package ammonite.interp

import java.io.{File, OutputStream, PrintStream}
import java.util.regex.Pattern

import scala.collection.mutable
import scala.tools.nsc.Settings
import ammonite.ops._
import ammonite.runtime._
import fastparse.all._

import annotation.tailrec
import ammonite.util.ImportTree
import ammonite.util.Util.{CacheDetails, newLine, normalizeNewlines}
import ammonite.util._

import scala.reflect.io.VirtualDirectory


/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(val printer: Printer,
                  val storage: Storage,
                  customPredefs: Seq[(Name, String)],
                  // Allows you to set up additional "bridges" between the REPL
                  // world and the outside world, by passing in the full name
                  // of the `APIHolder` object that will hold the bridge and
                  // the object that will be placed there. Needs to be passed
                  // in as a callback rather than run manually later as these
                  // bridges need to be in place *before* the predef starts
                  // running, so you can use them predef to e.g. configure
                  // the REPL before it starts
                  extraBridges: Interpreter => Seq[(String, String, AnyRef)],
                  val wd: Path,
                  verboseOutput: Boolean = true)
  extends ImportHook.InterpreterInterface{ interp =>



  //this variable keeps track of where should we put the imports resulting from scripts.
  private var scriptImportCallback: Imports => Unit = eval.update

  var lastException: Throwable = null

  private var _compilationCount = 0
  def compilationCount = _compilationCount


  val mainThread = Thread.currentThread()
  val eval = Evaluator(mainThread.getContextClassLoader, 0)

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var compiler: Compiler = null
  var pressy: Pressy = _
  val beforeExitHooks = mutable.Buffer.empty[Any ⇒ Any]

  def evalClassloader = eval.frames.head.classloader

  def reInit() = {
    if(compiler != null)
      init()
  }

  def init() = {
    // Note we not only make a copy of `settings` to pass to the compiler,
    // we also make a *separate* copy to pass to the presentation compiler.
    // Otherwise activating autocomplete makes the presentation compiler mangle
    // the shared settings and makes the main compiler sad
    val settings = Option(compiler).fold(new Settings)(_.compiler.settings.copy)
    compiler = Compiler(
      Classpath.classpath ++ eval.frames.head.classpath,
      dynamicClasspath,
      evalClassloader,
      eval.frames.head.pluginClassloader,
      () => pressy.shutdownPressy(),
      settings
    )
    pressy = Pressy(
      Classpath.classpath ++ eval.frames.head.classpath,
      dynamicClasspath,
      evalClassloader,

      settings.copy()
    )
  }

  val bridges = extraBridges(this) :+ ("ammonite.runtime.InterpBridge", "interp", interpApi)
  for ((name, shortName, bridge) <- bridges ){
    APIHolder.initBridge(evalClassloader, name, bridge)
  }
  // import ammonite.repl.ReplBridge.{value => repl}
  // import ammonite.runtime.InterpBridge.{value => interp}
  val bridgePredefs =
    for ((name, shortName, bridge) <- bridges)
    yield Name(s"${shortName}Bridge") -> s"import $name.{value => $shortName}"


  val importHooks = Ref(Map[Seq[String], ImportHook](
    Seq("file") -> ImportHook.File,
    Seq("exec") -> ImportHook.Exec,
    Seq("url") -> ImportHook.Http,
    Seq("ivy") -> ImportHook.Ivy,
    Seq("cp") -> ImportHook.Classpath,
    Seq("plugin", "ivy") -> ImportHook.PluginIvy,
    Seq("plugin", "cp") -> ImportHook.PluginClasspath
  ))

  val predefs = bridgePredefs ++ customPredefs ++ Seq(
    Name("SharedPredef") -> storage.loadSharedPredef,
    Name("LoadedPredef") -> storage.loadPredef
  )

  // Use a var and a for-loop instead of a fold, because when running
  // `processModule0` user code may end up calling `processModule` which depends
  // on `predefImports`, and we should be able to provide the "current" imports
  // to it even if it's half built
  var predefImports = Imports()
  for( (wrapperName, sourceCode) <- predefs) {
    val pkgName = Seq(Name("ammonite"), Name("predef"))

    processModule(
      ImportHook.Source.File(wd/s"${wrapperName.raw}.sc"),
      sourceCode,
      wrapperName,
      pkgName,
      true,
      ""
    ) match{
      case Res.Success((imports, wrapperHashes)) =>
        predefImports = predefImports ++ imports
      case Res.Failure(ex, msg) =>
        ex match{
          case Some(e) => throw new RuntimeException("Error during Predef: " + msg, e)
          case None => throw new RuntimeException("Error during Predef: " + msg)
        }

      case Res.Exception(ex, msg) =>
        throw new RuntimeException("Error during Predef: " + msg, ex)
    }
  }

  reInit()



  def resolveSingleImportHook(source: ImportHook.Source, tree: ImportTree) = {
    val strippedPrefix = tree.prefix.takeWhile(_(0) == '$').map(_.stripPrefix("$"))
    val hookOpt = importHooks().collectFirst{case (k, v) if strippedPrefix.startsWith(k) => (k, v)}
    for{
      (hookPrefix, hook) <- Res(hookOpt, "Import Hook could not be resolved")
      hooked <- hook.handle(source, tree.copy(prefix = tree.prefix.drop(hookPrefix.length)), this)
      hookResults <- Res.map(hooked){
        case res: ImportHook.Result.Source =>
          for{
            (moduleImports, _) <- processModule(
              res.source, res.code, res.wrapper, res.pkg,
              autoImport = false, extraCode = ""
            )
          } yield {
            if (!res.exec) res.imports
            else moduleImports ++ res.imports

          }
        case res: ImportHook.Result.ClassPath =>

          if (res.plugin) handlePluginClasspath(res.file.toIO)
          else handleEvalClasspath(res.file.toIO)

          Res.Success(Imports())
      }
    } yield {
      reInit()
      hookResults
    }
  }
  def resolveImportHooks(source: ImportHook.Source,
                         stmts: Seq[String]): Res[(Imports, Seq[String], Seq[ImportTree])] = {
      val hookedStmts = mutable.Buffer.empty[String]
      val importTrees = mutable.Buffer.empty[ImportTree]
      for(stmt <- stmts) {
        Parsers.ImportSplitter.parse(stmt) match{
          case f: Parsed.Failure => hookedStmts.append(stmt)
          case Parsed.Success(parsedTrees, _) =>
            var currentStmt = stmt
            for(importTree <- parsedTrees){
              if (importTree.prefix(0)(0) == '$') {
                val length = importTree.end - importTree.start
                currentStmt = currentStmt.patch(
                  importTree.start, (importTree.prefix(0) + ".$").padTo(length, ' '), length
                )
                importTrees.append(importTree)
              }
            }
            hookedStmts.append(currentStmt)
        }
      }

      for {
        hookImports <- Res.map(importTrees)(resolveSingleImportHook(source, _))
      } yield {
        val imports = Imports(hookImports.flatten.flatMap(_.value))
        (imports, hookedStmts, importTrees)
      }
    }

  def processLine(code: String, stmts: Seq[String], fileName: String): Res[Evaluated] = {
    val preprocess = Preprocessor(compiler.parse)
    for{
      _ <- Catching { case ex =>
        Res.Exception(ex, "Something unexpected went wrong =(")
      }

      (hookImports, hookedStmts, _) <- resolveImportHooks(
        ImportHook.Source.File(wd/"<console>"),
        stmts
      )
      unwrappedStmts = hookedStmts.flatMap{x =>
        Parsers.unwrapBlock(x) match {
          case Some(contents) => Parsers.split(contents).get.get.value
          case None => Seq(x)
        }
      }
      processed <- preprocess.transform(
        unwrappedStmts,
        eval.getCurrentLine,
        "",
        Seq(Name("$sess")),
        Name("cmd" + eval.getCurrentLine),
        predefImports ++ eval.frames.head.imports ++ hookImports,
        prints => s"ammonite.repl.ReplBridge.value.Internal.combinePrints($prints)",
        extraCode = ""
      )
      out <- evaluateLine(
        processed, printer,
        fileName, Name("cmd" + eval.getCurrentLine)
      )
    } yield out.copy(imports = out.imports ++ hookImports)

  }




  def withContextClassloader[T](t: => T) = {
    val oldClassloader = Thread.currentThread().getContextClassLoader
    try{
      Thread.currentThread().setContextClassLoader(evalClassloader)
      t
    } finally {
      Thread.currentThread().setContextClassLoader(oldClassloader)
    }
  }

  def compileClass(processed: Preprocessor.Output,
                   printer: Printer,
                   fileName: String): Res[(Util.ClassFiles, Imports)] = for {
    compiled <- Res.Success{
      compiler.compile(processed.code.getBytes, printer, processed.prefixCharLength, fileName)
    }
    _ = _compilationCount += 1
    (classfiles, imports) <- Res[(Util.ClassFiles, Imports)](
      compiled,
      "Compilation Failed"
    )
  } yield {
    (classfiles, imports)
  }



  def evaluateLine(processed: Preprocessor.Output,
                   printer: Printer,
                   fileName: String,
                   indexedWrapperName: Name): Res[Evaluated] = {

    for{
      _ <- Catching{ case e: ThreadDeath => Evaluator.interrupted(e) }
      (classFiles, newImports) <- compileClass(
        processed,
        printer,
        fileName
      )
      res <- withContextClassloader{
        eval.processLine(
          classFiles,
          newImports,
          printer,
          fileName,
          indexedWrapperName
        )

      }
    } yield res
  }


  def processScriptBlock(processed: Preprocessor.Output,
                         printer: Printer,
                         wrapperName: Name,
                         fileName: String,
                         pkgName: Seq[Name]) = {
    for {
      (cls, newImports, tag) <- cachedCompileBlock(
        processed,
        printer,
        wrapperName,
        fileName,
        pkgName,
        "scala.Iterator[String]()"
      )
      res <- eval.processScriptBlock(cls, newImports, wrapperName, pkgName, tag)
    } yield res
  }


  def cachedCompileBlock(processed: Preprocessor.Output,
                         printer: Printer,
                         wrapperName: Name,
                         fileName: String,
                         pkgName: Seq[Name],
                         printCode: String): Res[(Class[_], Imports, String)] = {


      val fullyQualifiedName = (pkgName :+ wrapperName).map(_.encoded).mkString(".")

      val tag = Interpreter.cacheTag(
        processed.code, Nil, eval.frames.head.classloader.classpathHash
      )
      val compiled = storage.compileCacheLoad(fullyQualifiedName, tag) match {
        case Some((classFiles, newImports)) =>
          val clsFiles = classFiles.map(_._1)

          Evaluator.addToClasspath(classFiles, dynamicClasspath)
          Res.Success((classFiles, newImports))
        case _ =>
          val noneCalc = for {
            (classFiles, newImports) <- compileClass(
              processed, printer, fileName
            )
            _ = storage.compileCacheSave(fullyQualifiedName, tag, (classFiles, newImports))
          } yield {
            (classFiles, newImports)
          }

          noneCalc
      }
      for {
        (classFiles, newImports) <- compiled
        cls <- eval.loadClass(fullyQualifiedName, classFiles)
      } yield (cls, newImports, tag)
    }

  def processModule(source: ImportHook.Source,
                    code: String,
                    wrapperName: Name,
                    pkgName: Seq[Name],
                    autoImport: Boolean,
                    extraCode: String): Res[(Imports, Seq[(String, String)])] = {
    val tag = Interpreter.cacheTag(
      code, Nil, eval.frames.head.classloader.classpathHash
    )
    storage.classFilesListLoad(
      pkgName.map(_.backticked).mkString("."),
      wrapperName.backticked,
      tag
    ) match {
      case None =>
        (source, verboseOutput) match {
          case (ImportHook.Source.File(fName), true) =>
            printer.out("Compiling " + fName.last + "\n")
          case (ImportHook.Source.URL(url), true) =>
            printer.out("Compiling " + url + "\n")
          case _ =>
        }
        init()
        val res = processModule0(
          source, code, wrapperName, pkgName,
          predefImports, autoImport, extraCode
        )
        res match{
         case Res.Success(data) =>
           reInit()
           val (imports, wrapperHashes, importTrees) = data
           storage.classFilesListSave(
             pkgName.map(_.backticked).mkString("."),
             wrapperName.backticked,
             wrapperHashes,
             imports,
             tag,
             importTrees
           )
           Res.Success((imports, wrapperHashes))
         case r: Res.Failing => r
       }
      case Some((wrapperHashes, classFiles, imports, importsTrees)) =>
        importsTrees.map(resolveSingleImportHook(source, _))

        val classFileNames = classFiles.map(_.map(_._1))
        withContextClassloader(
          eval.evalCachedClassFiles(
            classFiles,
            pkgName.map(_.backticked).mkString("."),
            wrapperName.backticked,
            dynamicClasspath,
            wrapperHashes.map(_._1)
          ) match {
            case Res.Success(_) =>
              eval.update(imports)
              Res.Success((imports, wrapperHashes))
            case r: Res.Failing => r
          }
        )
    }

  }

  def preprocessScript(source: ImportHook.Source, code: String) = for{
    blocks <- Preprocessor.splitScript(Interpreter.skipSheBangLine(code))
    hooked <- Res.map(blocks){case (prelude, stmts) => resolveImportHooks(source, stmts) }
    (hookImports, hookBlocks, importTrees) = hooked.unzip3
  } yield (blocks.map(_._1).zip(hookBlocks), Imports(hookImports.flatMap(_.value)), importTrees)

  def processModule0(source: ImportHook.Source,
                     code: String,
                     wrapperName: Name,
                     pkgName: Seq[Name],
                     startingImports: Imports,
                     autoImport: Boolean,
                     extraCode: String): Res[Interpreter.ProcessedData] = {
    for{
      (processedBlocks, hookImports, importTrees) <- preprocessScript(source, code)
      (imports, cacheData) <- processCorrectScript(
        processedBlocks,
        startingImports ++ hookImports,
        pkgName,
        wrapperName,
        (processed, wrapperIndex, indexedWrapperName) =>
          withContextClassloader(
            processScriptBlock(
              processed, printer,
              Interpreter.indexWrapperName(wrapperName, wrapperIndex),
              source match {
                case ImportHook.Source.File(fname) => fname.toString
                case _ => wrapperName.raw + ".sc"
              },
              pkgName
            )
          ),
        autoImport,
        extraCode
      )
    } yield (imports ++ hookImports, cacheData, importTrees.flatten)
  }



  def processExec(code: String): Res[Imports] = {
    init()
    for {
      (processedBlocks, hookImports, _) <- preprocessScript(
        ImportHook.Source.File(wd/"<console>"),
        code
      )
      (imports, _) <- processCorrectScript(
        processedBlocks,
        eval.frames.head.imports ++ hookImports,
        Seq(Name("$sess")),
        Name("cmd" + eval.getCurrentLine),
        { (processed, wrapperIndex, indexedWrapperName) =>
          evaluateLine(
            processed,
            printer,
            s"Main$wrapperIndex.sc",
            indexedWrapperName
          )
        },
        autoImport = true,
        ""
      )
    } yield imports ++ hookImports
  }



  def processCorrectScript(blocks: Seq[(String, Seq[String])],
                           startingImports: Imports,
                           pkgName: Seq[Name],
                           wrapperName: Name,
                           evaluate: Interpreter.EvaluateCallback,
                           autoImport: Boolean,
                           extraCode: String
                          ): Res[Interpreter.CacheData] = {

    val preprocess = Preprocessor(compiler.parse)
    // we store the old value, because we will reassign this in the loop
    val outerScriptImportCallback = scriptImportCallback

    /**
      * Iterate over the blocks of a script keeping track of imports.
      *
      * We keep track of *both* the `scriptImports` as well as the `lastImports`
      * because we want to be able to make use of any import generated in the
      * script within its blocks, but at the end we only want to expose the
      * imports generated by the last block to who-ever loaded the script
      */
    @tailrec def loop(blocks: Seq[(String, Seq[String])],
                      scriptImports: Imports,
                      lastImports: Imports,
                      wrapperIndex: Int,
                      compiledData: List[CacheDetails]): Res[Interpreter.CacheData] = {
      if (blocks.isEmpty) {
        // No more blocks
        // if we have imports to pass to the upper layer we do that
        if (autoImport) outerScriptImportCallback(lastImports)
        Res.Success(lastImports, compiledData)
      } else {
        // imports from scripts loaded from this script block will end up in this buffer
        var nestedScriptImports = Imports()
        scriptImportCallback = { imports =>
          nestedScriptImports = nestedScriptImports ++ imports
        }
        // pretty printing results is disabled for scripts
        val indexedWrapperName = Interpreter.indexWrapperName(wrapperName, wrapperIndex)
        val (leadingSpaces, stmts) = blocks.head
        val res = for{
          processed <- preprocess.transform(
            stmts,
            "",
            leadingSpaces,
            pkgName,
            indexedWrapperName,
            scriptImports,
            _ => "scala.Iterator[String]()",
            extraCode = extraCode
          )

          ev <- evaluate(processed, wrapperIndex, indexedWrapperName)
        } yield ev

        res match {
          case r: Res.Failure => r
          case r: Res.Exception => r
          case Res.Success(ev) =>
            val last = ev.imports ++ nestedScriptImports
            loop(
              blocks.tail,
              scriptImports ++ last,
              last,
              wrapperIndex + 1,
              (ev.wrapper.map(_.encoded).mkString("."), ev.tag) :: compiledData
            )
          case Res.Skip => loop(
            blocks.tail,
            scriptImports,
            lastImports,
            wrapperIndex + 1,
            compiledData
          )
        }
      }
    }
    // wrapperIndex starts off as 1, so that consecutive wrappers can be named
    // Wrapper, Wrapper2, Wrapper3, Wrapper4, ...
    try loop(blocks, startingImports, Imports(), wrapperIndex = 1, List())
    finally scriptImportCallback = outerScriptImportCallback
  }

  def handleOutput(res: Res[Evaluated]): Unit = {
    res match{
      case Res.Skip => // do nothing
      case Res.Exit(value) => pressy.shutdownPressy()
      case Res.Success(ev) => eval.update(ev.imports)
      case Res.Failure(ex, msg) => lastException = ex.getOrElse(lastException)
      case Res.Exception(ex, msg) => lastException = ex
    }
  }
  def loadIvy(coordinates: (String, String, String), verbose: Boolean = true) = {
    val (groupId, artifactId, version) = coordinates
    val cacheKey = (interpApi.resolvers().hashCode.toString, groupId, artifactId, version)

    val fetched =
      storage.ivyCache()
        .get(cacheKey)

    val psOpt =
      fetched
        .map(_.map(new java.io.File(_)))
        .filter(_.forall(_.exists()))

    psOpt match{
      case Some(ps) => ps
      case None =>
        val resolved = ammonite.runtime.tools.IvyThing(
          () => interpApi.resolvers(),
          printer,
          verboseOutput
        ).resolveArtifact(
          groupId,
          artifactId,
          version,
          if (verbose) 2 else 1
        ).toSet

        // #433 only non-snapshot versions are cached
        if (!version.endsWith("SNAPSHOT")) {
          storage.ivyCache() = storage.ivyCache().updated(
            cacheKey,
            resolved.map(_.getAbsolutePath)
          )
        }

        resolved
    }
  }
  abstract class DefaultLoadJar extends LoadJar {

    lazy val ivyThing = ammonite.runtime.tools.IvyThing(
      () => interpApi.resolvers(),
      printer,
      verboseOutput
    )

    def handleClasspath(jar: File): Unit

    def cp(jar: Path): Unit = {
      handleClasspath(new java.io.File(jar.toString))
      reInit()
    }
    def cp(jars: Seq[Path]): Unit = {
      jars.map(_.toString).map(new java.io.File(_)).foreach(handleClasspath)
      reInit()
    }
    def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit = {
      val resolved = loadIvy(coordinates, verbose)
      val (groupId, artifactId, version) = coordinates


      resolved.foreach(handleClasspath)

      reInit()
    }
  }

  def handleEvalClasspath(jar: File) = {
    eval.frames.head.addClasspath(Seq(jar))
    evalClassloader.add(jar.toURI.toURL)
  }
  def handlePluginClasspath(jar: File) = {
    eval.frames.head.pluginClassloader.add(jar.toURI.toURL)
  }
  lazy val interpApi: InterpAPI = new InterpAPI{ outer =>
    lazy val resolvers =
      Ref(ammonite.runtime.tools.Resolvers.defaultResolvers)

    object load extends DefaultLoadJar with Load {

      def handleClasspath(jar: File) = handleEvalClasspath(jar)

      def apply(line: String) = processExec(line) match{
        case Res.Failure(ex, s) => throw new CompilationError(s)
        case Res.Exception(t, s) => throw t
        case _ =>
      }

      def exec(file: Path): Unit = apply(normalizeNewlines(read(file)))

      def module(file: Path) = {
        val (pkg, wrapper) = Util.pathToPackageWrapper(file, wd)
        processModule(
          ImportHook.Source.File(wd/"Main.sc"),
          normalizeNewlines(read(file)),
          wrapper,
          pkg,
          true,
          ""
        ) match{
          case Res.Failure(ex, s) => throw new CompilationError(s)
          case Res.Exception(t, s) => throw t
          case x => //println(x)
        }
        reInit()
      }

      object plugin extends DefaultLoadJar {
        def handleClasspath(jar: File) = handlePluginClasspath(jar)
      }

    }
  }

}

object Interpreter{
  val SheBang = "#!"
  val SheBangEndPattern = Pattern.compile(s"""((?m)^!#.*)$newLine""")


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
    bytes.map("%02x".format(_)).mkString
  }

  def skipSheBangLine(code: String)= {
    if (code.startsWith(SheBang)) {
      val matcher = SheBangEndPattern matcher code
      val shebangEnd = if (matcher.find) matcher.end else code.indexOf(newLine)
      val numberOfStrippedLines = newLine.r.findAllMatchIn( code.substring(0, shebangEnd) ).length
      (newLine * numberOfStrippedLines) + code.substring(shebangEnd)
    } else
      code
  }


  type EvaluateCallback = (Preprocessor.Output, Int, Name) => Res[Evaluated]
  type CacheData = (Imports, Seq[CacheDetails])
  type ProcessedData = (Imports, Seq[CacheDetails], Seq[ImportTree])
  def indexWrapperName(wrapperName: Name, wrapperIndex: Int): Name = {
    Name(wrapperName.raw + (if (wrapperIndex == 1) "" else "_" + wrapperIndex))
  }

  def initPrinters(output: OutputStream, error: OutputStream, verboseOutput: Boolean) = {
    val colors = Ref[Colors](Colors.Default)
    val printStream = new PrintStream(output, true)
    val errorPrintStream = new PrintStream(error, true)


    def printlnWithColor(color: fansi.Attrs, s: String) = {
      Seq(color(s).render, newLine).foreach(errorPrintStream.print)
    }
    val printer = Printer(
      printStream.print,
      printlnWithColor(colors().warning(), _),
      printlnWithColor(colors().error(), _),
      printlnWithColor(fansi.Attrs.Empty, _)
    )
    (colors, printStream, errorPrintStream, printer)
  }
}
