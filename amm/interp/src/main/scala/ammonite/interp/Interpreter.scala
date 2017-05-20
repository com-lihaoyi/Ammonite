package ammonite.interp

import java.io.{File, OutputStream, PrintStream}
import java.util.regex.Pattern

import scala.collection.mutable
import scala.tools.nsc.Settings
import ammonite.ops._
import ammonite.runtime.Evaluator.addToClasspath
import ammonite.runtime._
import fastparse.all._

import annotation.tailrec
import ammonite.util.ImportTree
import ammonite.util.Util._
import ammonite.util._

import scala.reflect.io.VirtualDirectory


/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(val printer: Printer,
                  val storage: Storage,
                  customPredefs: Seq[Interpreter.PredefInfo],
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
    yield Interpreter.PredefInfo(
      Name(s"${shortName}Bridge"),
      s"import $name.{value => $shortName}",
      true,
      None
    )


  val importHooks = Ref(Map[Seq[String], ImportHook](
    Seq("file") -> ImportHook.File,
    Seq("exec") -> ImportHook.Exec,
    Seq("ivy") -> ImportHook.Ivy,
    Seq("cp") -> ImportHook.Classpath,
    Seq("plugin", "ivy") -> ImportHook.PluginIvy,
    Seq("plugin", "cp") -> ImportHook.PluginClasspath
  ))

  val predefs = {
    val (sharedPredefContent, sharedPredefPath) = storage.loadSharedPredef
    val (predefContent, predefPath) = storage.loadPredef
    bridgePredefs ++ customPredefs ++ Seq(
      Interpreter.PredefInfo(
        Name("UserSharedPredef"),
        sharedPredefContent,
        false,
        sharedPredefPath
      ),
      Interpreter.PredefInfo(
        Name("UserPredef"),
        predefContent,
        false,
        predefPath
      )
    )
  }

  // Use a var and a for-loop instead of a fold, because when running
  // `processModule0` user code may end up calling `processModule` which depends
  // on `predefImports`, and we should be able to provide the "current" imports
  // to it even if it's half built
  var predefImports = Imports()
  for {
    predefInfo <- predefs
    if predefInfo.code.nonEmpty
  }{
    val pkgName = Seq(Name("ammonite"), Name("predef"))

    processModule(
      predefInfo.code,
      CodeSource(predefInfo.name, pkgName, predefInfo.path),
      true,
      "",
      predefInfo.hardcoded
    ) match{
      case Res.Success(processed) =>
        predefImports = predefImports ++ processed.finalImports
      case Res.Failure(ex, msg) =>
        ex match{
          case Some(e) => throw new RuntimeException("Error during Predef: " + msg, e)
          case None => throw new RuntimeException("Error during Predef: " + msg)
        }

      case Res.Exception(ex, msg) =>
        throw new RuntimeException("Error during Predef: " + msg, ex)
      case _ => ???
    }
  }

  reInit()



  def resolveSingleImportHook(source: Option[Path], tree: ImportTree) = {
    val strippedPrefix = tree.prefix.takeWhile(_(0) == '$').map(_.stripPrefix("$"))
    val hookOpt = importHooks().collectFirst{case (k, v) if strippedPrefix.startsWith(k) => (k, v)}
    for{
      (hookPrefix, hook) <- Res(hookOpt, "Import Hook could not be resolved")
      hooked <- Res(
        hook.handle(source, tree.copy(prefix = tree.prefix.drop(hookPrefix.length)), this)
      )
      hookResults <- Res.map(hooked){
        case res: ImportHook.Result.Source =>
          for{
            processed <- processModule(
              res.code, res.blockInfo,
              autoImport = false, extraCode = "", hardcoded = false
            )
          } yield {
            if (!res.exec) res.imports
            else processed.finalImports ++ res.imports

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

  def resolveImportHooks(source: Option[Path],
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

      for (hookImports <- Res.map(importTrees)(resolveSingleImportHook(source, _)))
      yield (Imports(hookImports.flatten.flatMap(_.value)), hookedStmts, importTrees)
    }

  def processLine(code: String, stmts: Seq[String], fileName: String): Res[Evaluated] = {
    val preprocess = Preprocessor(compiler.parse)
    for{
      _ <- Catching { case ex =>
        Res.Exception(ex, "Something unexpected went wrong =(")
      }

      (hookImports, normalStmts, _) <- resolveImportHooks(
        Some(wd/"<console>"),
        stmts
      )

      processed <- preprocess.transform(
        normalStmts,
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
                         blockInfo: CodeSource) = {
    for {
      (cls, newImports, tag) <- cachedCompileBlock(
        processed,
        printer,
        blockInfo,
        "scala.Iterator[String]()"
      )
      res <- eval.processScriptBlock(
        cls,
        newImports,
        blockInfo.wrapperName,
        blockInfo.pkgName,
        tag
      )
    } yield res
  }



  def evalCachedClassFiles(source: Option[Path],
                           cachedData: Seq[ClassFiles],
                           dynamicClasspath: VirtualDirectory,
                           classFilesList: Seq[ScriptOutput.BlockMetadata]): Res[Seq[_]] = {
    Res.map(cachedData.zipWithIndex) {
      case (clsFiles, index) =>
        addToClasspath(clsFiles, dynamicClasspath)
        val blockMeta = classFilesList(index)
        blockMeta.importHookTrees.foreach(resolveSingleImportHook(source, _))
        val cls = eval.loadClass(blockMeta.id.wrapperPath, clsFiles)
        try cls.map(eval.evalMain(_))
        catch Evaluator.userCodeExceptionHandler
    }
  }

  def cachedCompileBlock(processed: Preprocessor.Output,
                         printer: Printer,
                         blockInfo: CodeSource,
                         printCode: String): Res[(Class[_], Imports, String)] = {


    val fullyQualifiedName = blockInfo.jvmPathPrefix

    val tag = Interpreter.cacheTag(
      processed.code, Nil, eval.frames.head.classloader.classpathHash
    )
    val compiled = storage.compileCacheLoad(fullyQualifiedName, tag) match {
      case Some((classFiles, newImports)) =>

        Evaluator.addToClasspath(classFiles, dynamicClasspath)
        Res.Success((classFiles, newImports))
      case _ =>
        val noneCalc = for {
          (classFiles, newImports) <- compileClass(
            processed, printer, blockInfo.printablePath
          )
        } yield {
          storage.compileCacheSave(fullyQualifiedName, tag, (classFiles, newImports))
          (classFiles, newImports)
        }

        noneCalc
    }
    for {
      (classFiles, newImports) <- compiled
      cls <- eval.loadClass(fullyQualifiedName, classFiles)
    } yield (cls, newImports, tag)
  }

  def processModule(code: String,
                    blockInfo: CodeSource,
                    autoImport: Boolean,
                    extraCode: String,
                    hardcoded: Boolean): Res[ScriptOutput.Metadata] = {

    val tag = Interpreter.cacheTag(
      code, Nil,
      if (hardcoded) Array.empty[Byte]
      else eval.frames.head.classloader.classpathHash
    )

    storage.classFilesListLoad(blockInfo.filePathPrefix, tag) match {
      case None =>
        printer.info("Compiling " + blockInfo.printablePath)
        init()
        val res = processModule0(code, blockInfo, predefImports, autoImport, extraCode)
        res match{
         case Res.Success(data) =>
           reInit()

           storage.classFilesListSave(
             blockInfo.filePathPrefix,
             data.blockInfo,
             data.finalImports,
             tag
           )
           Res.Success(data)
         case r: Res.Failing => r
       }
      case Some(compiledScriptData) =>

        withContextClassloader(
          evalCachedClassFiles(
            blockInfo.source,
            compiledScriptData.classFiles,
            dynamicClasspath,
            compiledScriptData.processed.blockInfo
          ) match {
            case Res.Success(_) =>
              eval.update(compiledScriptData.processed.finalImports)
              Res.Success(compiledScriptData.processed)
            case r: Res.Failing => r
          }
        )
    }
  }


  def processModule0(code: String,
                     blockInfo: CodeSource,
                     startingImports: Imports,
                     autoImport: Boolean,
                     extraCode: String): Res[ScriptOutput.Metadata] = {
    for{
      blocks <- Preprocessor.splitScript(Interpreter.skipSheBangLine(code))
      res <- processCorrectScript(
        blocks,
        startingImports,
        blockInfo,
        (processed, indexedWrapperName) =>
          withContextClassloader(
            processScriptBlock(
              processed,
              printer,
              blockInfo.copy(wrapperName = indexedWrapperName)
            )
          ),
        autoImport,
        extraCode
      )
    } yield res
  }



  def processExec(code: String): Res[Imports] = {
    init()
    for {
      blocks <- Preprocessor.splitScript(Interpreter.skipSheBangLine(code))
      processedData <- processCorrectScript(
        blocks,
        eval.frames.head.imports,
        CodeSource(Name("cmd" + eval.getCurrentLine), Seq(Name("$sess")),Some(wd/"<console>")),
        { (processed, indexedWrapperName) =>
          evaluateLine(
            processed,
            printer,
            s"Exec.sc",
            indexedWrapperName
          )
        },
        autoImport = true,
        ""
      )
    } yield processedData.finalImports
  }



  def processCorrectScript(blocks: Seq[(String, Seq[String])],
                           startingImports: Imports,
                           blockInfo: CodeSource,
                           evaluate: (Preprocessor.Output, Name) => Res[Evaluated],
                           autoImport: Boolean,
                           extraCode: String): Res[ScriptOutput.Metadata] = {

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
                      scriptImports0: Imports,
                      lastImports: Imports,
                      wrapperIndex: Int,
                      perBlockMetadata: List[ScriptOutput.BlockMetadata])
                      : Res[ScriptOutput.Metadata] = {
      if (blocks.isEmpty) {
        // No more blocks
        // if we have imports to pass to the upper layer we do that
        if (autoImport) outerScriptImportCallback(lastImports)
        Res.Success(ScriptOutput.Metadata(lastImports, perBlockMetadata))
      } else {
        // imports from scripts loaded from this script block will end up in this buffer
        var nestedScriptImports = Imports()
        scriptImportCallback = { imports =>
          nestedScriptImports = nestedScriptImports ++ imports
        }
        // pretty printing results is disabled for scripts
        val indexedWrapperName = Interpreter.indexWrapperName(blockInfo.wrapperName, wrapperIndex)
        val (leadingSpaces, stmts) = blocks.head
        val res = for{
          (hookImports, hookStmts, hookImportTrees) <- resolveImportHooks(blockInfo.source, stmts)
          scriptImports = scriptImports0 ++ hookImports
          processed <- preprocess.transform(
            hookStmts,
            "",
            leadingSpaces,
            blockInfo.pkgName,
            indexedWrapperName,
            scriptImports,
            _ => "scala.Iterator[String]()",
            extraCode = extraCode
          )

          ev <- evaluate(processed, indexedWrapperName)
        } yield (ev, scriptImports, hookImportTrees)

        res match {
          case r: Res.Failure => r
          case r: Res.Exception => r
          case Res.Success((ev, scriptImports, hookImportTrees)) =>
            val last = ev.imports ++ nestedScriptImports
            val newCompiledData = ScriptOutput.BlockMetadata(
              VersionedWrapperId(ev.wrapper.map(_.encoded).mkString("."), ev.tag),
              hookImportTrees
            )
            loop(
              blocks.tail,
              scriptImports ++ last,
              last,
              wrapperIndex + 1,
              newCompiledData :: perBlockMetadata
            )
          case Res.Skip => loop(
            blocks.tail,
            scriptImports0,
            lastImports,
            wrapperIndex + 1,
            perBlockMetadata
          )
          case _ => ???
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
  def loadIvy(coordinates: (String, String, String)*) = {
    val cacheKey = (interpApi.repositories().hashCode.toString, coordinates)

    storage.ivyCache().get(cacheKey) match{
      case Some(res) => Right(res.map(new java.io.File(_)))
      case None =>
        ammonite.runtime.tools.IvyThing.resolveArtifact(
          interpApi.repositories(),
          for((groupId, artifactId, version) <- coordinates)
          yield coursier.Dependency(coursier.Module(groupId, artifactId), version),
          verbose = verboseOutput
        )match{
          case Right(loaded) =>
            val loadedSet = loaded.toSet
            storage.ivyCache() = storage.ivyCache().updated(
              cacheKey, loadedSet.map(_.getAbsolutePath)
            )
            Right(loadedSet)
          case Left(l) =>
            Left(l)
        }
    }


  }
  abstract class DefaultLoadJar extends LoadJar {
    def handleClasspath(jar: File): Unit

    def cp(jar: Path): Unit = {
      handleClasspath(new java.io.File(jar.toString))
      reInit()
    }
    def cp(jars: Seq[Path]): Unit = {
      jars.map(_.toString).map(new java.io.File(_)).foreach(handleClasspath)
      reInit()
    }
    def ivy(coordinates: (String, String, String)*): Unit = {
      loadIvy(coordinates:_*) match{
        case Left(failureMsg) =>
          throw new Exception(failureMsg)
        case Right(loaded) =>
          loaded.foreach(handleClasspath)

      }

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

    val beforeExitHooks = interp.beforeExitHooks

    val repositories = Ref(ammonite.runtime.tools.IvyThing.defaultRepositories)

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
          normalizeNewlines(read(file)),
          CodeSource(wrapper, pkg, Some(wd/"Main.sc")),
          true,
          "",
          hardcoded = false
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
    * Information about a particular predef file or snippet. [[hardcoded]]
    * represents whether or not we cache the snippet forever regardless of
    * classpath, which is true for many "internal" predefs which only do
    * imports from Ammonite's own packages and don't rely on external code
    */
  case class PredefInfo(name: Name, code: String, hardcoded: Boolean, path: Option[Path])

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

  def indexWrapperName(wrapperName: Name, wrapperIndex: Int): Name = {
    Name(wrapperName.raw + (if (wrapperIndex == 1) "" else "_" + wrapperIndex))
  }

  def initPrinters(output: OutputStream,
                   info: OutputStream,
                   error: OutputStream,
                   verboseOutput: Boolean) = {
    val colors = Ref[Colors](Colors.Default)
    val printStream = new PrintStream(output, true)
    val errorPrintStream = new PrintStream(error, true)
    val infoPrintStream = new PrintStream(info, true)

    def printlnWithColor(stream: PrintStream, color: fansi.Attrs, s: String) = {
      stream.println(color(s).render)
    }

    val printer = Printer(
      printStream.print,
      printlnWithColor(errorPrintStream, colors().warning(), _),
      printlnWithColor(errorPrintStream, colors().error(), _),
      s => if (verboseOutput) printlnWithColor(infoPrintStream, fansi.Attrs.Empty, s)
    )
    (colors, printStream, errorPrintStream, printer)
  }
}
