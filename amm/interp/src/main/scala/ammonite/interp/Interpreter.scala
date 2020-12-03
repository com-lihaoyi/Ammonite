package ammonite.interp

import java.io.{File, OutputStream, PrintStream}
import java.nio.file.Path
import java.util.function.{Function, Supplier}
import java.util.regex.Pattern

import ammonite.compiler.iface.{CodeSource, CodeWrapper, Imports, Preprocessor => IPreprocessor}
import ammonite.interp.api.{InterpAPI, InterpLoad, LoadJar}

import scala.collection.mutable
import scala.collection.JavaConverters._
import ammonite.runtime._

import annotation.tailrec
import ammonite.compiler._
import ammonite.runtime.tools.IvyThing
import ammonite.util.ImportTree
import ammonite.util.InterfaceExtensions._
import ammonite.util.Util._
import ammonite.util._
import coursierapi.{Dependency, Fetch, Repository}

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(val printer: Printer,
                  val storage: Storage,
                  val wd: os.Path,
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


  def userScalaVersion = compilerManager.userScalaVersion

  def headFrame = getFrame()
  val repositories = mutable.Buffer(ammonite.runtime.tools.IvyThing.defaultRepositories: _*)
  val resolutionHooks = mutable.Buffer.empty[Function[Fetch, Fetch]]

  val mainThread = Thread.currentThread()

  def evalClassloader = headFrame.classloader
  def pluginClassloader = headFrame.pluginClassloader
  def handleImports(i: Imports) = headFrame.addImports(i)
  def frameImports = headFrame.imports
  def frameUsedEarlierDefinitions = headFrame.usedEarlierDefinitions

  def dependencyComplete: String => (Int, Seq[String]) =
    IvyThing.completer(repositories.toSeq, verbose = verboseOutput)

  val compilerManager: ammonite.compiler.iface.CompilerLifecycleManager = {
    val mgr = new CompilerLifecycleManager
    mgr.setRTCacheDir(storage.dirOpt.map(_.toNIO).orNull)
    mgr.setHeadFrame(() => headFrame)
    mgr.setDependencyCompleter { () =>
      input =>
        val (idx, completions) = dependencyComplete(input)
        entry(idx, completions.toArray)
    }
    mgr.setClassPathWhiteList(classPathWhitelist.toArray.map(_.toArray))
    mgr.setInitialClassLoader(Option(initialClassLoader).getOrElse(headFrame.classloader))
    mgr
  }

  val eval = Evaluator(headFrame)

  private var scriptImportCallback: Imports => Unit = handleImports

  val watchedValues = mutable.Buffer.empty[(Watchable, Long)]

  // We keep an *in-memory* cache of scripts, in additional to the global
  // filesystem cache shared between processes. This is because the global
  // cache is keyed on (script, env), but you can load the same script multiple
  // times in the same process (e.g. via a diamond dependency graph) and each
  // time it will have a different `env. Despite this, we want to ensure we
  // do not compile/load/run the same script more than once in the same
  // process, so we cache it based on the source of the code and return the
  // same result every time it gets run in the same process
  val alreadyLoadedFiles = mutable.Map.empty[CodeSource, ScriptOutput.Metadata]

  private val beforeExitHooksList = mutable.Buffer.empty[Function[Object, Object]]
  def beforeExitHooks: Seq[Object => Object] =
    beforeExitHooksList.toSeq.map(f => f.apply _)

  def compilationCount = compilerManager.compilationCount

  private val dependencyLoader = new DependencyLoader(
    printer,
    storage,
    alreadyLoadedDependencies,
    verboseOutput
  )


  // Use a var and callbacks instead of a fold, because when running
  // `processModule0` user code may end up calling `processModule` which depends
  // on `predefImports`, and we should be able to provide the "current" imports
  // to it even if it's half built
  var predefImports = new Imports(Array.empty)

  // Needs to be run after the Interpreter has been instantiated, as some of the
  // ReplAPIs available in the predef need access to the Interpreter object
  def initializePredef(
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
    baseImports: Imports = Interpreter.predefImports
  ): Option[(Res.Failing, Seq[(Watchable, Long)])] = {

    headFrame.classloader.specialLocalClasses ++= Seq(
      "ammonite.interp.api.InterpBridge",
      "ammonite.interp.api.InterpBridge$"
    )

    headFrame.classloader.specialLocalClasses ++= extraBridges
      .map(_._1)
      .flatMap(c => Seq(c, c + "$"))

    val bridgeImports = PredefInitialization.initBridges(
      ("ammonite.interp.api.InterpBridge", "interp", interpApi) +: extraBridges,
      evalClassloader
    )
    predefImports = predefImports ++ bridgeImports ++ baseImports

    PredefInitialization.apply(
      interpApi,
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
      case r @ Res.Exception(t, s) => Some((r, watchedValues.toSeq))
      case r @ Res.Failure(s) => Some((r, watchedValues.toSeq))
      case r @ Res.Exit(_) => Some((r, watchedValues.toSeq))
    }
  }

  // The ReplAPI requires some special post-Interpreter-initialization
  // code to run, so let it pass it in a callback and we'll run it here
  def watch(p: os.Path) = watchedValues.append(
    (Watchable.Path(p), Watchable.pathSignature(p))
  )
  def watchValue[T](v: => T) = watchedValues.append((() => v.hashCode, v.hashCode()))

  def resolveSingleImportHook(
    source: CodeSource,
    tree: ImportTree,
    wrapperPath: Seq[Name]
  ) = synchronized{
    val hookOpt = importHooks.find{case (k, v) => tree.strippedPrefix.startsWith(k)}
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

          if (res.plugin) headFrame.addPluginClasspath(res.files.map(_.toNIO.toUri.toURL))
          else headFrame.addClasspath(res.files.map(_.toNIO.toUri.toURL))

          Res.Success(new Imports())

        case ImportHook.Result.Repo(repo) =>
          addRepository(repo)
          Res.Success(new Imports())

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
      new Imports(hookImports.flatten.flatMap(_.data).toArray),
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

    val codeSource = new CodeSource(
      wrapperName.raw,
      Array(),
      Array("ammonite", "$sess"),
      (wd/"(console)").toNIO
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

      processed <- compilerManager.preprocessor("(console)").transform(
        hookStmts,
        currentLine.toString,
        "",
        codeSource,
        wrapperName,
        predefImports ++ frameImports ++ hookImports,
        prints => s"ammonite.repl.ReplBridge.value.Internal.combinePrints($prints)",
        extraCode = "",
        skipEmpty = true,
        markScript = false,
        codeWrapper = replCodeWrapper
      )
      (out, tag) <- evaluateLine(
        processed,
        wrapperName.encoded + ".sc", wrapperName,
        silent,
        incrementLine
      )
    } yield out.copy(imports = out.imports ++ hookImports)
  }

  def evaluateLine(processed: IPreprocessor.Output,
                   fileName: String,
                   indexedWrapperName: Name,
                   silent: Boolean = false,
                   incrementLine: () => Unit): Res[(Evaluated, Tag)] = synchronized{
    for{
      _ <- Catching{ case e: ThreadDeath => Evaluator.interrupted(e) }
      output <- Res(
        compilerManager.compileClass(
          processed,
          printer,
          fileName
        ),
        "Compilation Failed"
      )
      _ = incrementLine()
      res <- eval.processLine(
        output.classFiles.map { e => (e.getKey, e.getValue) }.toVector,
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


  def processSingleBlock(processed: IPreprocessor.Output,
                         codeSource0: CodeSource,
                         indexedWrapperName: Name) = synchronized{


    val codeSource = codeSource0.withWrapperName(indexedWrapperName)
    val fullyQualifiedName = codeSource.jvmPathPrefix

    val tag = Tag(
      Interpreter.cacheTag(processed.code.getBytes),
      Interpreter.cacheTag(evalClassloader.classpathHash(codeSource0.path.map(os.Path(_)))),
      classPathWhitelist.hashCode().toString
    )

    for {
      _ <- Catching{case e: Throwable => e.printStackTrace(); throw e}
      output <- Res(
        compilerManager.compileClass(
          processed, printer, codeSource.fileName
        ),
        "Compilation Failed"
      )
      cls <- eval.loadClass(
        fullyQualifiedName,
        output.classFiles.toVector.map(e => (e.getKey, e.getValue))
      )

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
        Storage.CompileCache(
          output.classFiles.toVector.map(e => (e.getKey, e.getValue)),
          output.imports
        )
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
            else evalClassloader.classpathHash(codeSource.path.map(os.Path(_)))
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
            case None => Res(splittedScript).map(_.map(_ => None))
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
            Res(splittedScript),
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
      blocks <- Res(Preprocessor.splitScript(Interpreter.skipSheBangLine(code), fileName))

      metadata <- processAllScriptBlocks(
        blocks.map(_ => None),
        Res.Success(blocks),
        predefImports ++ frameImports,
        new CodeSource(
          wrapperName.raw,
          Array(),
          Array("ammonite", "$sess"),
          (wd/"(console)").toNIO
        ),
        (processed, indexedWrapperName) =>
          evaluateLine(processed, fileName, indexedWrapperName, false, incrementLine),
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
                             evaluate: (IPreprocessor.Output, Name) => Res[(Evaluated, Tag)],
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
        var nestedScriptImports = new Imports

        scriptImportCallback = { imports =>
          nestedScriptImports = nestedScriptImports ++ imports
        }

        // pretty printing results is disabled for scripts
        val indexedWrapperName = Interpreter.indexWrapperName(codeSource.wrapperName, wrapperIndex)


        def compileRunBlock(leadingSpaces: String, hookInfo: ImportHookInfo) = {
          val printSuffix = if (wrapperIndex == 1) "" else  " #" + wrapperIndex
          printer.info("Compiling " + codeSource.printablePath + printSuffix)
          for{
            processed <- compilerManager.preprocessor(codeSource.fileName).transform(
              hookInfo.stmts,
              "",
              leadingSpaces,
              codeSource,
              indexedWrapperName,
              scriptImports ++ hookInfo.imports,
              _ => "scala.Iterator[String]()",
              extraCode = extraCode,
              skipEmpty = false,
              markScript = false,
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
          val envHash = Interpreter.cacheTag(
            evalClassloader.classpathHash(codeSource.path.map(os.Path(_)))
          )
          if (envHash != blockMetadata.id.tag.env) {
            compileRunBlock(blockMetadata.leadingSpaces, blockMetadata.hookInfo)
          } else{
            compilerManager.addToClasspath(
              classFiles.toArray
                .map { case (n, b) => entry(n, b) }
            )

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

      for(res <- loop(blocks, startingImports, new Imports, wrapperIndex = 1, List()))
      // We build up `blockInfo` backwards, since it's a `List`, so reverse it
      // before giving it to the outside world
      yield ScriptOutput.Metadata(res.blockInfo.reverse)
    } finally scriptImportCallback = outerScriptImportCallback
  }

  def loadIvy(coordinates: Dependency*) = synchronized {
    dependencyLoader.load(
      coordinates,
      repositories.toSeq,
      resolutionHooks.toSeq.map(f => f.apply _)
    )
  }

  private def addRepository(repository: Repository): Unit = synchronized {
    repositories += repository
  }

  abstract class DefaultLoadJar extends LoadJar {
    def handleClasspath(jar: java.net.URL): Unit

    def cp(jars: Path*): Unit = {
      jars.map(_.toUri.toURL).foreach(handleClasspath)
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

    def watch(p: Path) = interp.watch(os.Path(p))
    def addWatchValue[T](v: Supplier[T]): Unit = interp.watchValue(v.get())

    def objCompilerLifeCycleManager = interp.compilerManager

    val beforeExitHooksList = interp.beforeExitHooksList.asJava

    val repositoriesList = interp.repositories.asJava
    val resolutionHooksList = interp.resolutionHooks.asJava

    object load extends DefaultLoadJar with InterpLoad {

      def handleClasspath(jar: java.net.URL) = headFrame.addClasspath(Seq(jar))


      def module(file: Path) = {
        watch(file)
        val (pkg, wrapper) = ammonite.util.Util.pathToPackageWrapper(
          Seq(Name("dummy")),
          os.Path(file) relativeTo wd
        )
        processModule(
          normalizeNewlines(os.read(os.Path(file))),
          new CodeSource(
            wrapper.raw,
            pkg.map(_.raw).toArray,
            Array("ammonite", "$file"),
            (wd/"Main.sc").toNIO
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

  val predefImports = ammonite.util.Imports(
    ImportData("ammonite.interp.api.InterpExtras.exit"),
    ImportData("ammonite.interp.api.InterpExtras.{InterpAPIExtensions, ReplClassLoaderExtensions}"),
    ImportData(
      "ammonite.interp.api.IvyConstructor.{ArtifactIdExt, GroupIdExt}",
      importType = ImportData.Type
    ),
    ImportData("ammonite.util.InterfaceExtensions.ImportsExtensions"),
    ImportData("ammonite.runtime.tools.{browse, grep, time}"),
    ImportData("ammonite.runtime.tools.tail", importType = ImportData.TermType),
    ImportData("ammonite.repl.tools.{desugar, source}"),
    ImportData("mainargs.{arg, main}"),
    ImportData("ammonite.repl.tools.Util.PathRead")
  )



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
