package ammonite.interp

import java.io.{File, OutputStream, PrintStream}
import java.util.regex.Pattern

import scala.collection.mutable
import ammonite.ops._
import ammonite.runtime._
import fastparse.all._

import annotation.tailrec
import ammonite.util.ImportTree
import ammonite.util.Util._
import ammonite.util._

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(val printer: Printer,
                  val storage: Storage,
                  customPredefs: Seq[PredefInfo],
                  // Allows you to set up additional "bridges" between the REPL
                  // world and the outside world, by passing in the full name
                  // of the `APIHolder` object that will hold the bridge and
                  // the object that will be placed there. Needs to be passed
                  // in as a callback rather than run manually later as these
                  // bridges need to be in place *before* the predef starts
                  // running, so you can use them predef to e.g. configure
                  // the REPL before it starts
                  extraBridges: Interpreter => Seq[(String, String, AnyRef, () => Unit)],
                  val wd: Path,
                  verboseOutput: Boolean = true)
  extends ImportHook.InterpreterInterface{ interp =>


  var lastException: Throwable = null

  val mainThread = Thread.currentThread()

  val (compilerManager, eval) = {
    val hash = SpecialClassLoader.initialClasspathSignature(mainThread.getContextClassLoader)

    import ammonite.ops._

    // *Try* to load the JVM source files and make them available as resources,
    // so that the `source` helper can navigate to the sources within the
    // Java standard library
    val likelyJdkSourceLocation = Path(System.getProperty("java.home"))/up/"src.zip"
    def special = new SpecialClassLoader(
      new ForkClassLoader(mainThread.getContextClassLoader, getClass.getClassLoader),
      hash,
      likelyJdkSourceLocation.toNIO.toUri.toURL
    )

    val initialFrame = new Frame(special, special, Imports(), Seq())

    val frames = Ref(List(initialFrame))

    (new CompilerLifecycleManager(frames), Evaluator(0, frames()))
  }

  private var scriptImportCallback: Imports => Unit = eval.update
  def compilationCount = compilerManager.compilationCount
  val watchedFiles = mutable.Buffer.empty[(Path, Option[Long])]

  // We keep an *in-memory* cache of scripts, in additional to the global
  // filesystem cache shared between processes. This is because the global
  // cache is keyed on (script, env), but you can load the same script multiple
  // times in the same process (e.g. via a diamond dependency graph) and each
  // time it will have a different `env. Despite this, we want to ensure we
  // do not compile/load/run the same script more than once in the same
  // process, so we cache it based on the source of the code and return the
  // same result every time it gets run in the same process
  val alreadyLoadedFiles = mutable.Map.empty[CodeSource, ScriptOutput.Metadata]

  val beforeExitHooks = mutable.Buffer.empty[Any ⇒ Any]


  val importHooks = Ref(Map[Seq[String], ImportHook](
    Seq("file") -> ImportHook.File,
    Seq("exec") -> ImportHook.Exec,
    Seq("ivy") -> ImportHook.Ivy,
    Seq("cp") -> ImportHook.Classpath,
    Seq("plugin", "ivy") -> ImportHook.PluginIvy,
    Seq("plugin", "cp") -> ImportHook.PluginClasspath
  ))


  // Use a var and callbacks instead of a fold, because when running
  // `processModule0` user code may end up calling `processModule` which depends
  // on `predefImports`, and we should be able to provide the "current" imports
  // to it even if it's half built
  var predefImports = Imports()
  PredefInitialization.apply(
    extraBridges(this) :+ ("ammonite.interp.InterpBridge", "interp", interpApi, () => ()),
    interpApi,
    eval.evalClassloader,
    storage,
    customPredefs,
    processModule(_, _, true, "", _),
    imports => predefImports = predefImports ++ imports
  )

  // The ReplAPI requires some special post-Interpreter-initialization
  // code to run, so let it pass it in a callback and we'll run it here
  for ((name, shortName, bridge, cb) <- extraBridges(this) ) cb()


  def resolveSingleImportHook(source: CodeSource, tree: ImportTree) = {
    val strippedPrefix = tree.prefix.takeWhile(_(0) == '$').map(_.stripPrefix("$"))
    val hookOpt = importHooks().collectFirst{case (k, v) if strippedPrefix.startsWith(k) => (k, v)}
    for{
      (hookPrefix, hook) <- Res(hookOpt, "Import Hook could not be resolved")
      hooked <- Res(
        hook.handle(source, tree.copy(prefix = tree.prefix.drop(hookPrefix.length)), this)
      )
      hookResults <- Res.map(hooked){
        case res: ImportHook.Result.Source =>
          res.codeSource.path.foreach(interpApi.watch)
          for{
            processed <- processModule(
              res.code, res.codeSource,
              autoImport = false, extraCode = "", hardcoded = false
            )
          } yield {
            // For $file imports, we do not propagate any imports from the imported scripted
            // to the enclosing session. Instead, the imported script wrapper object is
            // brought into scope and you're meant to use the methods defined on that.
            //
            // Only $exec imports merge the scope of the imported script into your enclosing
            // scope, but those are comparatively rare.
            if (!res.exec) res.hookImports
            else processed.blockInfo.last.finalImports ++ res.hookImports
          }
        case res: ImportHook.Result.ClassPath =>

          if (res.plugin) compilerManager.handlePluginClasspath(res.file.toIO)
          else compilerManager.handleEvalClasspath(res.file.toIO)

          Res.Success(Imports())
      }
    } yield {
      hookResults
    }
  }

  def resolveImportHooks(source: CodeSource,
                         stmts: Seq[String]): Res[ImportHookInfo] = {
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
      yield ImportHookInfo(
        Imports(hookImports.flatten.flatMap(_.value)),
        hookedStmts,
        importTrees
      )
    }

  def processLine(code: String, stmts: Seq[String], fileName: String): Res[Evaluated] = {

    val wrapperName = Name("cmd" + eval.getCurrentLine)

    for{
      _ <- Catching { case ex =>
        Res.Exception(ex, "Something unexpected went wrong =(")
      }

      ImportHookInfo(hookImports, hookStmts, _) <- resolveImportHooks(

        CodeSource(wrapperName, Seq(), Seq(Name("ammonite"), Name("$file")), Some(wd/"(console)")),
        stmts
      )

      processed <- compilerManager.preprocess.transform(
        hookStmts,
        eval.getCurrentLine,
        "",
        Seq(Name("ammonite"), Name("$sess")),
        wrapperName,
        predefImports ++ eval.imports ++ hookImports,
        prints => s"ammonite.repl.ReplBridge.value.Internal.combinePrints($prints)",
        extraCode = ""
      )
      (out, tag) <- evaluateLine(
        processed, printer,
        fileName, Name("cmd" + eval.getCurrentLine)
      )
    } yield out.copy(imports = out.imports ++ hookImports)
  }


  def evaluateLine(processed: Preprocessor.Output,
                   printer: Printer,
                   fileName: String,
                   indexedWrapperName: Name): Res[(Evaluated, Tag)] = {
    for{
      _ <- Catching{ case e: ThreadDeath => Evaluator.interrupted(e) }
      (classFiles, newImports) <- compilerManager.compileClass(
        processed,
        printer,
        fileName
      )
      res <- eval.processLine(
        classFiles,
        newImports,
        printer,
        fileName,
        indexedWrapperName
      )
    } yield (res, Tag("", ""))
  }


  def processScriptBlock(processed: Preprocessor.Output,
                         codeSource0: CodeSource,
                         indexedWrapperName: Name) = {

    val codeSource = codeSource0.copy(wrapperName = indexedWrapperName)
    val fullyQualifiedName = codeSource.jvmPathPrefix

    val tag = Tag(
      Interpreter.cacheTag(processed.code.getBytes),
      Interpreter.cacheTag(eval.evalClassloader.classpathHash)
    )

    for {
      (classFiles, newImports) <- compilerManager.compileClass(
        processed, printer, codeSource.printablePath
      )
      cls <- eval.loadClass(fullyQualifiedName, classFiles)

      res <- eval.processScriptBlock(
        cls,
        newImports,
        codeSource.wrapperName,
        codeSource.pkgName
      )
    } yield {
      storage.compileCacheSave(fullyQualifiedName, tag, (classFiles, newImports))

      (res, tag)
    }
  }


  def processModule(code: String,
                    codeSource: CodeSource,
                    autoImport: Boolean,
                    extraCode: String,
                    hardcoded: Boolean): Res[ScriptOutput.Metadata] = {

    alreadyLoadedFiles.get(codeSource) match{
      case Some(x) => Res.Success(x)
      case None =>
        val tag = Tag(
          Interpreter.cacheTag(code.getBytes),
          Interpreter.cacheTag(
            if (hardcoded) Array.empty[Byte]
            else eval.evalClassloader.classpathHash
          )
        )


        val cachedScriptData = storage.classFilesListLoad(codeSource.filePathPrefix, tag)


        // Lazy, because we may not always need this if the script is already cached
        // and none of it's blocks end up needing to be re-compiled. We don't know up
        // front if any blocks will need re-compilation, because it may import $file
        // another script which gets changed, and we'd only know when we reach that block
        lazy val splittedScript = Preprocessor.splitScript(Interpreter.skipSheBangLine(code))

        for{
          blocks <- cachedScriptData match {
            case None => splittedScript.map(_.map(_ => None))
            case Some(scriptOutput) =>
              Res.Success(
                scriptOutput.classFiles
                  .zip(scriptOutput.processed.blockInfo)
                  .map(Some(_))
              )
          }

          data <- processCorrectScript(
            blocks,
            splittedScript,
            predefImports,
            codeSource,
            processScriptBlock(_, codeSource, _),
            autoImport,
            extraCode
          )
        } yield {
          storage.classFilesListSave(
            codeSource.filePathPrefix,
            data.blockInfo,
            tag
          )
          alreadyLoadedFiles(codeSource) = data
          data
        }
    }

  }


  def processExec(code: String): Res[Imports] = for {
    blocks <- Preprocessor.splitScript(Interpreter.skipSheBangLine(code))
    processedData <- processCorrectScript(
      blocks.map(_ => None),
      Res.Success(blocks),
      eval.imports,
      CodeSource(
        Name("cmd" + eval.getCurrentLine),
        Seq(),
        Seq(Name("ammonite"), Name("$sess")),
        Some(wd/"(console)")
      ),
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
  } yield processedData.blockInfo.last.finalImports




  type BlockData = Option[(ClassFiles, ScriptOutput.BlockMetadata)]



  def processCorrectScript(blocks: Seq[BlockData],
                           splittedScript: => Res[IndexedSeq[(String, Seq[String])]],
                           startingImports: Imports,
                           codeSource: CodeSource,
                           evaluate: (Preprocessor.Output, Name) => Res[(Evaluated, Tag)],
                           autoImport: Boolean,
                           extraCode: String): Res[ScriptOutput.Metadata] = {

    // we store the old value, because we will reassign this in the loop
    val outerScriptImportCallback = scriptImportCallback

    /**
      * Iterate over the blocks of a script keeping track of imports.
      *
      * We keep track of *both* the `scriptImports` as well as the `lastImports`
      * because we want to be able to make use of any import generated in the
      * script within its blocks, but at the end we only want to expose the
      * imports generated by the last block to who-ever loaded the script
      *
      * @param blocks the compilation block of the script, separated by `@`s.
      *               Each one is a tuple containing the leading whitespace and
      *               a sequence of statements in that block
      *
      * @param scriptImports the set of imports that apply to the current
      *                      compilation block, excluding that of the last
      *                      block that was processed since that is held
      *                      separately in `lastImports` and treated
      *                      specially
      *
      * @param lastImports the imports created by the last block that was processed;
      *                    only imports created by that
      *
      * @param wrapperIndex a counter providing the index of the current block, so
      *                     e.g. if `Foo.sc` has multiple blocks they can be named
      *                     `Foo_1` `Foo_2` etc.
      *
      * @param perBlockMetadata an accumulator for the processed metadata of each block
      *                         that is fed in
      */
    @tailrec def loop(blocks: Seq[BlockData],
                      scriptImports: Imports,
                      lastImports: Imports,
                      wrapperIndex: Int,
                      perBlockMetadata: List[ScriptOutput.BlockMetadata])
                      : Res[ScriptOutput.Metadata] = {
      if (blocks.isEmpty) {
        // No more blocks
        // if we have imports to pass to the upper layer we do that
        if (autoImport) outerScriptImportCallback(lastImports)
        Res.Success(ScriptOutput.Metadata(perBlockMetadata))
      } else {
        // imports from scripts loaded from this script block will end up in this buffer
        var nestedScriptImports = Imports()
        scriptImportCallback = { imports =>
          nestedScriptImports = nestedScriptImports ++ imports
        }
        // pretty printing results is disabled for scripts
        val indexedWrapperName = Interpreter.indexWrapperName(codeSource.wrapperName, wrapperIndex)


        def compileRunBlock(leadingSpaces: String, hookInfo: ImportHookInfo) = {
          val printSuffix = if (wrapperIndex == 1) "" else  " #" + wrapperIndex
          printer.info("Compiling " + codeSource.printablePath + printSuffix)
          for{
            processed <- compilerManager.preprocess.transform(
              hookInfo.stmts,
              "",
              leadingSpaces,
              codeSource.pkgName,
              indexedWrapperName,
              scriptImports ++ hookInfo.imports,
              _ => "scala.Iterator[String]()",
              extraCode = extraCode
            )

            (ev, tag) <- evaluate(processed, indexedWrapperName)
          } yield ScriptOutput.BlockMetadata(
            VersionedWrapperId(ev.wrapper.map(_.encoded).mkString("."), tag),
            leadingSpaces,
            hookInfo,
            ev.imports
          )
        }


        val res = blocks.head match{
          case None  =>
            for{
              allSplittedChunks <- splittedScript
              (leadingSpaces, stmts) = allSplittedChunks(wrapperIndex - 1)
              hookInfo <- resolveImportHooks(codeSource, stmts)
              res <- compileRunBlock(leadingSpaces, hookInfo)
            } yield res

          case Some((classFiles, blockMetadata)) =>
            blockMetadata.hookInfo.trees.foreach(resolveSingleImportHook(codeSource, _))
            val envHash = Interpreter.cacheTag(eval.evalClassloader.classpathHash)
            if (envHash != blockMetadata.id.tag.env) {
              compileRunBlock(blockMetadata.leadingSpaces, blockMetadata.hookInfo)
            } else{
              compilerManager.addToClasspath(classFiles)

              val cls = eval.loadClass(blockMetadata.id.wrapperPath, classFiles)
              val evaluated =
                try cls.map(eval.evalMain(_))
                catch Evaluator.userCodeExceptionHandler

              evaluated.map(_ => blockMetadata)
            }
        }

        res match{
          case Res.Success(blockMetadata) =>
            val last =
              blockMetadata.hookInfo.imports ++
              blockMetadata.finalImports ++
              nestedScriptImports

            loop(
              blocks.tail,
              scriptImports ++ last,
              last,
              wrapperIndex + 1,
              blockMetadata :: perBlockMetadata
            )

          case r: Res.Failure => r
          case r: Res.Exception => r
          case Res.Skip =>
            loop(blocks.tail, scriptImports, lastImports, wrapperIndex + 1, perBlockMetadata)

        }
      }
    }
    // wrapperIndex starts off as 1, so that consecutive wrappers can be named
    // Wrapper, Wrapper2, Wrapper3, Wrapper4, ...
    try {

      for(res <- loop(blocks, startingImports, Imports(), wrapperIndex = 1, List()))
      // We build up `blockInfo` backwards, since it's a `List`, so reverse it
      // before giving it to the outside world
      yield ScriptOutput.Metadata(res.blockInfo.reverse)
    } finally scriptImportCallback = outerScriptImportCallback
  }

  def handleOutput(res: Res[Evaluated]): Unit = {
    res match{
      case Res.Skip => // do nothing
      case Res.Exit(value) => compilerManager.shutdownPressy()
      case Res.Success(ev) => eval.update(ev.imports)
      case Res.Failure(ex, msg) => lastException = ex.getOrElse(lastException)
      case Res.Exception(ex, msg) => lastException = ex
    }
  }
  def loadIvy(coordinates: coursier.Dependency*) = {
    val cacheKey = (interpApi.repositories().hashCode.toString, coordinates)

    storage.ivyCache().get(cacheKey) match{
      case Some(res) => Right(res.map(new java.io.File(_)))
      case None =>
        ammonite.runtime.tools.IvyThing.resolveArtifact(
          interpApi.repositories(),
          coordinates,
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
    }
    def cp(jars: Seq[Path]): Unit = {
      jars.map(_.toString).map(new java.io.File(_)).foreach(handleClasspath)
    }
    def ivy(coordinates: coursier.Dependency*): Unit = {
      loadIvy(coordinates:_*) match{
        case Left(failureMsg) =>
          throw new Exception(failureMsg)
        case Right(loaded) =>
          loaded.foreach(handleClasspath)

      }
    }
  }

  lazy val interpApi: InterpAPI = new InterpAPI{ outer =>

    def watch(p: Path) = {
      watchedFiles.append(p -> Interpreter.mtimeIfExists(p))
    }

    def configureCompiler(callback: scala.tools.nsc.Global => Unit) = {
      compilerManager.configureCompiler(callback)
    }

    val beforeExitHooks = interp.beforeExitHooks

    val repositories = Ref(ammonite.runtime.tools.IvyThing.defaultRepositories)

    object load extends DefaultLoadJar with Load {

      def handleClasspath(jar: File) = compilerManager.handleEvalClasspath(jar)

      def apply(line: String) = processExec(line) match{
        case Res.Failure(ex, s) => throw new CompilationError(s)
        case Res.Exception(t, s) => throw t
        case _ =>
      }

      def exec(file: Path): Unit = {
        watch(file)
        apply(normalizeNewlines(read(file)))
      }

      def module(file: Path) = {
        watch(file)
        val (pkg, wrapper) = Util.pathToPackageWrapper(
          Seq(Name("dummy")),
          file relativeTo wd
        )
        processModule(
          normalizeNewlines(read(file)),
          CodeSource(
            wrapper,
            pkg,
            Seq(Name("ammonite"), Name("$file")),
            Some(wd/"Main.sc")
          ),
          true,
          "",
          hardcoded = false
        ) match{
          case Res.Failure(ex, s) => throw new CompilationError(s)
          case Res.Exception(t, s) => throw t
          case x => //println(x)
        }
      }

      object plugin extends DefaultLoadJar {
        def handleClasspath(jar: File) = compilerManager.handlePluginClasspath(jar)
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
  def cacheTag(classpathHash: Array[Byte]): String = {
    val bytes = Util.md5Hash(Iterator(
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

  def mtimeIfExists(p: Path) = if (!exists(p)) None else Some(p.mtime.toMillis)

  def initPrinters(colors0: Colors,
                   output: OutputStream,
                   info: OutputStream,
                   error: OutputStream,
                   verboseOutput: Boolean) = {
    val colors = Ref[Colors](colors0)
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
