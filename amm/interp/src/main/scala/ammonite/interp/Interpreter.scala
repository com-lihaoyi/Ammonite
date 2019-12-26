package ammonite.interp

import java.io.{File, OutputStream, PrintStream}
import java.util.regex.Pattern

import ammonite.interp.api.{InterpAPI, InterpLoad, LoadJar}
import ammonite.interp.CodeWrapper

import scala.collection.mutable

import ammonite.runtime._
import fastparse._

import annotation.tailrec
import ammonite.runtime.tools.IvyThing
import ammonite.util.ImportTree
import ammonite.util.Util._
import ammonite.util._
import coursierapi.{Dependency, Fetch}

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(val printer: Printer,
                  val storage: Storage,
                  basePredefs: Seq[PredefInfo],
                  customPredefs: Seq[PredefInfo],
                  // Allows you to set up additional "bridges" between the REPL
                  // world and the outside world, by passing in the full name
                  // of the `APIHolder` object that will hold the bridge and
                  // the object that will be placed there. Needs to be passed
                  // in as a callback rather than run manually later as these
                  // bridges need to be in place *before* the predef starts
                  // running, so you can use them predef to e.g. configure
                  // the REPL before it starts
                  extraBridges: Seq[(String, String, AnyRef)],
                  val wd: os.Path,
                  colors: Ref[Colors],
                  verboseOutput: Boolean = true,
                  getFrame: () => Frame,
                  val createFrame: () => Frame,
                  initialClassLoader: ClassLoader = null,
                  replCodeWrapper: CodeWrapper,
                  val scriptCodeWrapper: CodeWrapper,
                  alreadyLoadedDependencies: Seq[Dependency],
                  importHooks: Map[Seq[String], ImportHook] = ImportHook.defaults,
                  classPathWhitelist: Set[Seq[String]] = Set.empty)
  extends ImportHook.InterpreterInterface{ interp =>


  def headFrame = getFrame()
  val repositories = Ref(ammonite.runtime.tools.IvyThing.defaultRepositories)
  val resolutionHooks = mutable.Buffer.empty[Fetch => Fetch]

  headFrame.classloader.specialLocalClasses ++= Seq(
    "ammonite.interp.api.InterpBridge",
    "ammonite.interp.api.InterpBridge$"
  )

  headFrame.classloader.specialLocalClasses ++= extraBridges
    .map(_._1)
    .flatMap(c => Seq(c, c + "$"))

  val mainThread = Thread.currentThread()

  def evalClassloader = headFrame.classloader
  def pluginClassloader = headFrame.pluginClassloader
  def handleImports(i: Imports) = headFrame.addImports(i)
  def frameImports = headFrame.imports
  def frameUsedEarlierDefinitions = headFrame.usedEarlierDefinitions

  def dependencyComplete: String => (Int, Seq[String]) =
    IvyThing.completer(repositories(), verbose = verboseOutput)

  val compilerManager = new CompilerLifecycleManager(
    storage,
    headFrame,
    Some(dependencyComplete),
    classPathWhitelist,
    Option(initialClassLoader).getOrElse(headFrame.classloader)
  )

  val eval = Evaluator(headFrame)

  private var scriptImportCallback: Imports => Unit = handleImports

  val watchedFiles = mutable.Buffer.empty[(os.Path, Long)]

  // We keep an *in-memory* cache of scripts, in additional to the global
  // filesystem cache shared between processes. This is because the global
  // cache is keyed on (script, env), but you can load the same script multiple
  // times in the same process (e.g. via a diamond dependency graph) and each
  // time it will have a different `env. Despite this, we want to ensure we
  // do not compile/load/run the same script more than once in the same
  // process, so we cache it based on the source of the code and return the
  // same result every time it gets run in the same process
  val alreadyLoadedFiles = mutable.Map.empty[CodeSource, ScriptOutput.Metadata]

  val beforeExitHooks = mutable.Buffer.empty[Any => Any]

  def compilationCount = compilerManager.compilationCount


  // Use a var and callbacks instead of a fold, because when running
  // `processModule0` user code may end up calling `processModule` which depends
  // on `predefImports`, and we should be able to provide the "current" imports
  // to it even if it's half built
  var predefImports = Imports()

  // Needs to be run after the Interpreter has been instantiated, as some of the
  // ReplAPIs available in the predef need access to the Interpreter object
  def initializePredef(): Option[(Res.Failing, Seq[(os.Path, Long)])] = {
    PredefInitialization.apply(
      ("ammonite.interp.api.InterpBridge", "interp", interpApi) +: extraBridges,
      interpApi,
      evalClassloader,
      storage,
      basePredefs,
      customPredefs,
      // AutoImport is false, because we do not want the predef imports to get
      // bundled into the main `eval.imports`: instead we pass `predefImports`
      // manually throughout, and keep `eval.imports` as a relatively-clean
      // listing which only contains the imports for code a user entered.
      processModule(_, _, autoImport = false, "", _),
      imports => predefImports = predefImports ++ imports,
      watch
    ) match{
      case Res.Success(_) => None
      case Res.Skip => None
      case r @ Res.Exception(t, s) => Some((r, watchedFiles.toSeq))
      case r @ Res.Failure(s) => Some((r, watchedFiles.toSeq))
      case r @ Res.Exit(_) => Some((r, watchedFiles.toSeq))
    }
  }

  // The ReplAPI requires some special post-Interpreter-initialization
  // code to run, so let it pass it in a callback and we'll run it here
  def watch(p: os.Path) = watchedFiles.append(p -> Interpreter.pathSignature(p))

  def resolveSingleImportHook(
    source: CodeSource,
    tree: ImportTree,
    wrapperPath: Seq[Name]
  ) = synchronized{
    val strippedPrefix = tree.prefix.takeWhile(_(0) == '$').map(_.stripPrefix("$"))
    val hookOpt = importHooks.collectFirst{case (k, v) if strippedPrefix.startsWith(k) => (k, v)}
    for{
      (hookPrefix, hook) <- Res(hookOpt, s"Import Hook ${tree.prefix} could not be resolved")
      hooked <- Res(
        hook.handle(
          source,
          tree.copy(prefix = tree.prefix.drop(hookPrefix.length)),
          this,
          wrapperPath
        )
      )
      hookResults <- Res.map(hooked){
        case res: ImportHook.Result.Source =>
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

          if (res.plugin) headFrame.addPluginClasspath(Seq(res.file.toNIO.toUri.toURL))
          else headFrame.addClasspath(Seq(res.file.toNIO.toUri.toURL))

          Res.Success(Imports())
      }
    } yield hookResults
  }



  def resolveImportHooks(importTrees: Seq[ImportTree],
                         hookedStmts: Seq[String],
                         source: CodeSource,
                         wrapperPath: Seq[Name]): Res[ImportHookInfo] = synchronized{

    // Fake an update to the classpath to force re-creation of the compiler
    // Workaround for https://github.com/scala/bug/issues/11564#issuecomment-501834821,
    // which I caused in 2.13.0 and should be fixed in 2.13.1
    if (scala.util.Properties.versionNumberString == "2.13.0") headFrame.addClasspath(Nil)

    for (hookImports <- Res.map(importTrees)(resolveSingleImportHook(source, _, wrapperPath)))
    yield ImportHookInfo(
      Imports(hookImports.flatten.flatMap(_.value)),
      hookedStmts,
      importTrees
    )
  }

  def processLine(code: String,
                  stmts: Seq[String],
                  currentLine: Int,
                  silent: Boolean = false,
                  incrementLine: () => Unit): Res[Evaluated] = synchronized{

    val wrapperName = Name("cmd" + currentLine)

    val codeSource = CodeSource(
      wrapperName,
      Seq(),
      Seq(Name("ammonite"), Name("$sess")),
      Some(wd/"(console)")
    )
    val (hookStmts, importTrees) = Parsers.parseImportHooks(codeSource, stmts)

    for{
      _ <- Catching { case ex => Res.Exception(ex, "") }
      ImportHookInfo(hookImports, hookStmts, _) <- resolveImportHooks(
        importTrees,
        hookStmts,
        codeSource,
        replCodeWrapper.wrapperPath
      )

      processed <- compilerManager.preprocess("(console)").transform(
        hookStmts,
        currentLine.toString,
        "",
        codeSource,
        wrapperName,
        predefImports ++ frameImports ++ hookImports,
        prints => s"ammonite.repl.ReplBridge.value.Internal.combinePrints($prints)",
        extraCode = "",
        skipEmpty = true,
        codeWrapper = replCodeWrapper
      )
      (out, tag) <- evaluateLine(
        processed, printer,
        wrapperName.encoded + ".sc", wrapperName,
        silent,
        incrementLine
      )
    } yield out.copy(imports = out.imports ++ hookImports)
  }


  def evaluateLine(processed: Preprocessor.Output,
                   printer: Printer,
                   fileName: String,
                   indexedWrapperName: Name,
                   silent: Boolean = false,
                   incrementLine: () => Unit): Res[(Evaluated, Tag)] = synchronized{
    for{
      _ <- Catching{ case e: ThreadDeath => Evaluator.interrupted(e) }
      output <- compilerManager.compileClass(
        processed,
        printer,
        fileName
      )
      _ = incrementLine()
      res <- eval.processLine(
        output.classFiles,
        output.imports,
        output.usedEarlierDefinitions.getOrElse(Nil),
        printer,
        indexedWrapperName,
        replCodeWrapper.wrapperPath,
        silent,
        evalClassloader
      )
    } yield (res, Tag("", "", classPathWhitelist.hashCode().toString))
  }


  def processSingleBlock(processed: Preprocessor.Output,
                         codeSource0: CodeSource,
                         indexedWrapperName: Name) = synchronized{


    val codeSource = codeSource0.copy(wrapperName = indexedWrapperName)
    val fullyQualifiedName = codeSource.jvmPathPrefix

    val tag = Tag(
      Interpreter.cacheTag(processed.code.getBytes),
      Interpreter.cacheTag(evalClassloader.classpathHash(codeSource0.path)),
      classPathWhitelist.hashCode().toString
    )

    for {
      _ <- Catching{case e: Throwable => e.printStackTrace(); throw e}
      output <- compilerManager.compileClass(
        processed, printer, codeSource.fileName
      )
      cls <- eval.loadClass(fullyQualifiedName, output.classFiles)

      res <- eval.processScriptBlock(
        cls,
        output.imports,
        output.usedEarlierDefinitions.getOrElse(Nil),
        codeSource.wrapperName,
        scriptCodeWrapper.wrapperPath,
        codeSource.pkgName,
        evalClassloader
      )
    } yield {
      storage.compileCacheSave(
        fullyQualifiedName,
        tag,
        Storage.CompileCache(output.classFiles, output.imports)
      )

      (res, tag)
    }
  }


  def processModule(code: String,
                    codeSource: CodeSource,
                    autoImport: Boolean,
                    extraCode: String,
                    hardcoded: Boolean,
                    moduleCodeWrapper: CodeWrapper = scriptCodeWrapper)
                    : Res[ScriptOutput.Metadata] = synchronized{

    alreadyLoadedFiles.get(codeSource) match{
      case Some(x) => Res.Success(x)
      case None =>
        val tag = Tag(
          Interpreter.cacheTag(code.getBytes),
          Interpreter.cacheTag(
            if (hardcoded) Array.empty[Byte]
            else evalClassloader.classpathHash(codeSource.path)
          ),
          classPathWhitelist.hashCode().toString
        )


        val cachedScriptData = storage.classFilesListLoad(
          os.sub / codeSource.filePathPrefix,
          tag
        )


        // Lazy, because we may not always need this if the script is already cached
        // and none of it's blocks end up needing to be re-compiled. We don't know up
        // front if any blocks will need re-compilation, because it may import $file
        // another script which gets changed, and we'd only know when we reach that block
        lazy val splittedScript = Preprocessor.splitScript(
          Interpreter.skipSheBangLine(code),
          codeSource.fileName
        )

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

          _ <- Catching { case ex => Res.Exception(ex, "") }

          metadata <- processAllScriptBlocks(
            blocks,
            splittedScript,
            predefImports,
            codeSource,
            processSingleBlock(_, codeSource, _),
            autoImport,
            extraCode
          )
        } yield {
          storage.classFilesListSave(
            os.sub / codeSource.filePathPrefix,
            metadata.blockInfo,
            tag
          )
          alreadyLoadedFiles(codeSource) = metadata
          metadata
        }
    }
  }

  def processExec(code: String,
                  currentLine: Int,
                  incrementLine: () => Unit): Res[Imports] = synchronized{
    val wrapperName = Name("cmd" + currentLine)
    val fileName = wrapperName.encoded + ".sc"
    for {
      blocks <- Preprocessor.splitScript(Interpreter.skipSheBangLine(code), fileName)

      metadata <- processAllScriptBlocks(
        blocks.map(_ => None),
        Res.Success(blocks),
        predefImports ++ frameImports,
        CodeSource(
          wrapperName,
          Seq(),
          Seq(Name("ammonite"), Name("$sess")),
          Some(wd/"(console)")
        ),
        (processed, indexedWrapperName) =>
          evaluateLine(processed, printer, fileName, indexedWrapperName, false, incrementLine),
        autoImport = true,
        ""
      )
    } yield {
      metadata.blockInfo.last.finalImports
    }
  }


  type BlockData = Option[(ClassFiles, ScriptOutput.BlockMetadata)]


  def processAllScriptBlocks(blocks: Seq[BlockData],
                             splittedScript: => Res[IndexedSeq[(String, Seq[String])]],
                             startingImports: Imports,
                             codeSource: CodeSource,
                             evaluate: (Preprocessor.Output, Name) => Res[(Evaluated, Tag)],
                             autoImport: Boolean,
                             extraCode: String): Res[ScriptOutput.Metadata] = synchronized{

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
            processed <- compilerManager.preprocess(codeSource.fileName).transform(
              hookInfo.stmts,
              "",
              leadingSpaces,
              codeSource,
              indexedWrapperName,
              scriptImports ++ hookInfo.imports,
              _ => "scala.Iterator[String]()",
              extraCode = extraCode,
              skipEmpty = false,
              codeWrapper = scriptCodeWrapper
            )

            (ev, tag) <- evaluate(processed, indexedWrapperName)
          } yield ScriptOutput.BlockMetadata(
            VersionedWrapperId(ev.wrapper.map(_.encoded).mkString("."), tag),
            leadingSpaces,
            hookInfo,
            ev.imports
          )
        }



        val cachedLoaded = for{
          (classFiles, blockMetadata) <- blocks.head
          // We don't care about the results of resolving the import hooks;
          // Assuming they still *can* be resolved, the `envHash` check will
          // ensure re-compile this block if the contents of any import hook
          // changes
          if resolveImportHooks(
            blockMetadata.hookInfo.trees,
            blockMetadata.hookInfo.stmts,
            codeSource,
            scriptCodeWrapper.wrapperPath
          ).isInstanceOf[Res.Success[_]]
        } yield {
          val envHash = Interpreter.cacheTag(evalClassloader.classpathHash(codeSource.path))
          if (envHash != blockMetadata.id.tag.env) {
            compileRunBlock(blockMetadata.leadingSpaces, blockMetadata.hookInfo)
          } else{
            compilerManager.addToClasspath(classFiles)

            val cls = eval.loadClass(blockMetadata.id.wrapperPath, classFiles)
            val evaluated =
              try cls.map(eval.evalMain(_, evalClassloader))
              catch Evaluator.userCodeExceptionHandler

            evaluated.map(_ => blockMetadata)
          }
        }

        val res = cachedLoaded.getOrElse{
          for{
            allSplittedChunks <- splittedScript
            (leadingSpaces, stmts) = allSplittedChunks(wrapperIndex - 1)
            (hookStmts, importTrees) = Parsers.parseImportHooks(codeSource, stmts)
            hookInfo <- resolveImportHooks(
             importTrees, hookStmts, codeSource, scriptCodeWrapper.wrapperPath
            )
            res <- compileRunBlock(leadingSpaces, hookInfo)
          } yield res
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

          case r: Res.Exit => r
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


  private val alwaysExclude = alreadyLoadedDependencies
    .map(dep => (dep.getModule.getOrganization, dep.getModule.getName))
    .toSet

  def loadIvy(coordinates: Dependency*) = synchronized{
    val cacheKey = (
      interpApi.repositories().hashCode.toString + classPathWhitelist.hashCode().toString,
      coordinates
    )

    storage.ivyCache().get(cacheKey) match{
      case Some(res) => Right(res.map(new java.io.File(_)))
      case None =>
        ammonite.runtime.tools.IvyThing.resolveArtifact(
          interpApi.repositories(),
          coordinates
            .filter(dep => !alwaysExclude((dep.getModule.getOrganization, dep.getModule.getName)))
            .map { dep =>
              alwaysExclude.iterator.foldLeft(dep)((dep, excl) =>
                dep.addExclusion(excl._1, excl._2)
              )
            },
          verbose = verboseOutput,
          output = printer.errStream,
          hooks = resolutionHooks.toSeq
        )match{
          case Right((canBeCached, loaded)) =>
            val loadedSet = loaded.toSet
            if (canBeCached)
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
    def handleClasspath(jar: java.net.URL): Unit

    def cp(jar: os.Path): Unit = {
      handleClasspath(jar.toNIO.toUri.toURL)
    }
    def cp(jars: Seq[os.Path]): Unit = {
      jars.map(_.toNIO.toUri.toURL).foreach(handleClasspath)
    }
    def cp(jar: java.net.URL): Unit = {
      handleClasspath(jar)
    }
    def ivy(coordinates: Dependency*): Unit = {
      loadIvy(coordinates:_*) match{
        case Left(failureMsg) =>
          throw new Exception(failureMsg)
        case Right(loaded) =>
          loaded
            .map(_.toURI.toURL)
            .foreach(handleClasspath)

      }
    }
  }


  private[this] lazy val interpApi: InterpAPI = new InterpAPI{ outer =>

    val colors = interp.colors

    def watch(p: os.Path) = interp.watch(p)

    def configureCompiler(callback: scala.tools.nsc.Global => Unit) = {
      compilerManager.configureCompiler(callback)
    }

    def preConfigureCompiler(callback: scala.tools.nsc.Settings => Unit) = {
      compilerManager.preConfigureCompiler(callback)
    }

    val beforeExitHooks = interp.beforeExitHooks

    val repositories = interp.repositories
    val resolutionHooks = interp.resolutionHooks

    object load extends DefaultLoadJar with InterpLoad {

      def handleClasspath(jar: java.net.URL) = headFrame.addClasspath(Seq(jar))


      def module(file: os.Path) = {
        watch(file)
        val (pkg, wrapper) = ammonite.util.Util.pathToPackageWrapper(
          Seq(Name("dummy")),
          file relativeTo wd
        )
        processModule(
          normalizeNewlines(os.read(file)),
          CodeSource(
            wrapper,
            pkg,
            Seq(Name("ammonite"), Name("$file")),
            Some(wd/"Main.sc")
          ),
          autoImport = true,
          extraCode = "",
          hardcoded = false
        ) match{
          case Res.Failure(s) => throw new CompilationError(s)
          case Res.Exception(t, s) => throw t
          case x => //println(x)
        }
      }

      object plugin extends DefaultLoadJar {
        def handleClasspath(jar: java.net.URL) = headFrame.addPluginClasspath(Seq(jar))
      }

    }
  }

}

object Interpreter{

  def mtimeIfExists(p: os.Path) = if (os.exists(p)) os.mtime(p) else 0L

  /**
    * Recursively mtimes things, with the sole purpose of providing a number
    * that will change if that file changes or that folder's contents changes
    *
    * Ensure we include the file paths within a folder as part of the folder
    * signature, as file moves often do not update the mtime but we want to
    * trigger a "something changed" event anyway
    */
  def pathSignature(p: os.Path) =
    if (!os.exists(p)) 0L
    else try {
      if (os.isDir(p)) os.walk(p).map(x => x.hashCode + mtimeIfExists(x)).sum
      else os.mtime(p)
    } catch { case e: java.nio.file.NoSuchFileException =>
      0L
    }

  val SheBang = "#!"
  val SheBangEndPattern = Pattern.compile(s"""((?m)^!#.*)$newLine""")



  /**
    * This gives our cache tags for compile caching. The cache tags are a hash
    * of classpath, previous commands (in-same-script), and the block-code.
    * Previous commands are hashed in the wrapper names, which are contained
    * in imports, so we don't need to pass them explicitly.
    */
  def cacheTag(classpathHash: Array[Byte]): String = {
    val bytes = ammonite.util.Util.md5Hash(Iterator(
      classpathHash
    ))
    bytes.map("%02x".format(_)).mkString
  }

  def skipSheBangLine(code: String) = {
    val newLineLength = newLine.length
    /**
      * the skipMultipleLines function is necessary to support the parsing of
      * multiple shebang lines. The NixOs nix-shell normally uses 2+ shebang lines.
      */
    def skipMultipleLines(ind: Int = 0): Int = {
      val index = code.indexOf('\n', ind)
      if (code.substring(index + 1).startsWith(SheBang))
        skipMultipleLines(ind + index + 1)
      else index - (newLineLength - 1)
    }

    if (code.startsWith(SheBang)) {
      val matcher = SheBangEndPattern matcher code
      val shebangEnd = if (matcher.find) matcher.end else skipMultipleLines()
      val numberOfStrippedLines = newLine.r.findAllMatchIn( code.substring(0, shebangEnd) ).length
      (newLine * numberOfStrippedLines) + code.substring(shebangEnd)
    } else
      code
  }

  def indexWrapperName(wrapperName: Name, wrapperIndex: Int): Name = {
    Name(wrapperName.raw + (if (wrapperIndex == 1) "" else "_" + wrapperIndex))
  }

  def initPrinters(colors0: Colors,
                   output: OutputStream,
                   error: OutputStream,
                   verboseOutput: Boolean) = {
    val colors = Ref[Colors](colors0)
    val printStream = new PrintStream(output, true)
    val errorPrintStream = new PrintStream(error, true)

    def printlnWithColor(stream: PrintStream, color: fansi.Attrs, s: String) = {
      stream.println(color(s).render)
    }

    val printer = Printer(
      printStream,
      errorPrintStream,
      printStream,
      printlnWithColor(errorPrintStream, colors().warning(), _),
      printlnWithColor(errorPrintStream, colors().error(), _),
      s => if (verboseOutput) printlnWithColor(errorPrintStream, colors().info(), s)
    )
    (colors, printer)
  }

}
