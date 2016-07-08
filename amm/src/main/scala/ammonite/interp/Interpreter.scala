package ammonite.interp

import ammonite.tools.{IvyThing, Resolver, Resolvers}
import java.io.{File, OutputStream}
import java.nio.file.NotDirectoryException

import org.apache.ivy.plugins.resolver.RepositoryResolver

import scala.collection.mutable
import scala.tools.nsc.Settings
import ammonite.ops._
import fastparse.all._

import annotation.tailrec
import ammonite._
import ammonite.frontend._
import ammonite.util.Parsers.ImportTree
import ammonite.util.Util.CacheDetails
import ammonite.util._
import pprint.{Config, PPrint, PPrinter}
import ammonite.terminal.Filter

import scala.reflect.io.VirtualDirectory
import scala.util.Try

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter(prompt0: Ref[String],
                  frontEnd0: Ref[FrontEnd],
                  width: => Int,
                  height: => Int,
                  colors0: Ref[Colors],
                  printer: Printer,
                  storage: Storage,
                  history: => History,
                  predef: String,
                  val wd: Path,
                  replArgs: Seq[Bind[_]],
                  timer: Timer)
  extends ImportHook.InterpreterInterface{ interp =>


  val hardcodedPredef =
    "import ammonite.frontend.ReplBridge.repl.{pprintConfig, derefPPrint}"


  //this variable keeps track of where should we put the imports resulting from scripts.
  private var scriptImportCallback: Imports => Unit = eval.update

  var lastException: Throwable = null

  private var _compilationCount = 0
  def compilationCount = _compilationCount


  val mainThread = Thread.currentThread()
  val eval = Evaluator(mainThread.getContextClassLoader, 0, timer)

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var compiler: Compiler = null
  var pressy: Pressy = _
  def evalClassloader = eval.sess.frames.head.classloader

  def reInit() = {
    if(compiler != null)
      init()
  }

  def init() = timer{
    // Note we not only make a copy of `settings` to pass to the compiler,
    // we also make a *separate* copy to pass to the presentation compiler.
    // Otherwise activating autocomplete makes the presentation compiler mangle
    // the shared settings and makes the main compiler sad
    val settings = Option(compiler).fold(new Settings)(_.compiler.settings.copy)
    compiler = Compiler(
      Classpath.classpath ++ eval.sess.frames.head.classpath,
      dynamicClasspath,
      evalClassloader,
      eval.sess.frames.head.pluginClassloader,
      () => pressy.shutdownPressy(),
      settings,
      timer
    )
    pressy = Pressy(
      Classpath.classpath ++ eval.sess.frames.head.classpath,
      dynamicClasspath,
      evalClassloader,

      settings.copy()
    )
  }




  evalClassloader.findClassPublic("ammonite.frontend.ReplBridge$")
  val bridgeCls = evalClassloader.findClassPublic("ammonite.frontend.ReplBridge")

  ReplAPI.initReplBridge(
    bridgeCls.asInstanceOf[Class[ReplAPIHolder]],
    replApi
  )

  val argString = replArgs.zipWithIndex.map{ case (b, idx) =>
    s"""
    val ${b.name} =
      ammonite.frontend.ReplBridge.repl.replArgs($idx).value.asInstanceOf[${b.typeTag.tpe}]
    """
  }.mkString("\n")

  val importHooks = Ref(Map[Seq[String], ImportHook](
    Seq("file") -> ImportHook.File,
    Seq("exec") -> ImportHook.Exec,
    Seq("url") -> ImportHook.Http,
    Seq("ivy") -> ImportHook.Ivy,
    Seq("cp") -> ImportHook.Classpath,
    Seq("plugin", "ivy") -> ImportHook.PluginIvy,
    Seq("plugin", "cp") -> ImportHook.PluginClasspath
  ))

  val predefs = Seq(
    (hardcodedPredef, Name("HardcodedPredef")),
    (predef, Name("Predef")),
    (storage.loadPredef, Name("LoadedPredef")),
    (argString, Name("ArgsPredef"))
  )

  // Use a var and a for-loop instead of a fold, because when running
  // `processModule0` user code may end up calling `processModule` which depends
  // on `predefImports`, and we should be able to provide the "current" imports
  // to it even if it's half built
  var predefImports = Imports()
  for( (sourceCode, wrapperName) <- predefs) timer{
    val pkgName = Seq(Name("ammonite"), Name("predef"))

    processModule(
      ImportHook.Source.File(wd/"Main.sc"),
      sourceCode,
      wrapperName,
      pkgName,
      true
    ) match{
      case Res.Success(imports) =>
        predefImports = predefImports ++ imports
      case Res.Failure(ex, msg) =>
        ex match{
          case Some(e) => throw new RuntimeException("Error during Predef: " + msg, e)
          case None => throw new RuntimeException("Error during Predef: " + msg)
        }

      case Res.Exception(ex, msg) =>
        throw new RuntimeException("Error during Predef: " + msg, ex)
    }
  }("loadingPredef " + wrapperName.backticked)

  eval.sess.save()
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
            moduleImports <- processModule(
              res.source, res.code, res.wrapper, res.pkg, autoImport = false
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
                         stmts: Seq[String]): Res[(Imports, Seq[String], Seq[ImportTree])] =
    timer{
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

  def processLine(code: String, stmts: Seq[String], fileName: String): Res[Evaluated] = timer{
    val preprocess = Preprocessor(compiler.parse)
    for{
      _ <- Catching { case ex =>
        Res.Exception(ex, "Something unexpected went wrong =(")
      }

      (hookImports, hookedStmts, _) <- resolveImportHooks(
        ImportHook.Source.File(wd/"<console>"),
        stmts
      )
      processed <- preprocess.transform(
        hookedStmts,
        eval.getCurrentLine,
        "",
        Seq(Name("$sess")),
        Name("cmd" + eval.getCurrentLine),
        predefImports ++ eval.sess.frames.head.imports ++ hookImports,
        prints => s"ammonite.frontend.ReplBridge.repl.Internal.combinePrints($prints)"
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
                   indexedWrapperName: Name): Res[Evaluated] = timer{
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
                         pkgName: Seq[Name]) = timer{
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
                         printCode: String): Res[(Class[_], Imports, String)] =
    timer{


      val fullyQualifiedName = (pkgName :+ wrapperName).map(_.encoded).mkString(".")
      val tag = Interpreter.cacheTag(
        processed.code, Nil, eval.sess.frames.head.classloader.classpathHash
      )
      val compiled = storage.compileCacheLoad(fullyQualifiedName, tag) match {
        case Some((classFiles, newImports)) =>
          Compiler.addToClasspath(classFiles, dynamicClasspath)
          Res.Success((classFiles, newImports))
        case _ =>
          val noneCalc = for {
            (classFiles, newImports) <- compileClass(
              processed, printer, fileName
            )
            _ = storage.compileCacheSave(fullyQualifiedName, tag, (classFiles, newImports))
          } yield (classFiles, newImports)

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
                    autoImport: Boolean): Res[Imports] = timer{

    val tag = Interpreter.cacheTag(
      code, Nil, eval.sess.frames.head.classloader.classpathHash
    )
    storage.classFilesListLoad(
      pkgName.map(_.backticked).mkString("."),
      wrapperName.backticked,
      tag
    ) match {
      case None =>
        init()
        val res = processModule0(source, code, wrapperName, pkgName, predefImports, autoImport)
        res match{
         case Res.Success(data) =>
           reInit()
           val (imports, cachedData, importTrees) = data
           storage.classFilesListSave(
             pkgName.map(_.backticked).mkString("."),
             wrapperName.backticked,
             cachedData,
             imports,
             tag,
             importTrees
           )
           res.map(_._1)
         case r: Res.Failing => r
       }
      case Some((classFilesList, cachedData, imports, importsTrees)) =>
        importsTrees.map(resolveSingleImportHook(source, _))
        eval.evalCachedClassFiles(
          cachedData,
          pkgName.map(_.backticked).mkString("."),
          wrapperName.backticked,
          dynamicClasspath,
          classFilesList
        ) match {
          case Res.Success(_) =>
            eval.update(imports)
            Res.Success(imports)
          case r: Res.Failing => r
        }
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
                     autoImport: Boolean): Res[Interpreter.ProcessedData] = timer{
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
              wrapperName.raw + ".sc", pkgName
            )
          ),
        autoImport
      )
    } yield (imports ++ hookImports, cacheData, importTrees.flatten)
  }



  def processExec(code: String): Res[Imports] = timer{
    init()
    for {
      (processedBlocks, hookImports, _) <- preprocessScript(
        ImportHook.Source.File(wd/"<console>"),
        code
      )
      (imports, _) <- processCorrectScript(
        processedBlocks,
        eval.sess.frames.head.imports ++ hookImports,
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
        autoImport = true
      )
    } yield imports ++ hookImports
  }



  def processCorrectScript(blocks: Seq[(String, Seq[String])],
                           startingImports: Imports,
                           pkgName: Seq[Name],
                           wrapperName: Name,
                           evaluate: Interpreter.EvaluateCallback,
                           autoImport: Boolean
                          ): Res[Interpreter.CacheData] = timer{

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
            _ => "scala.Iterator[String]()"
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
              (ev.wrapper.map(_.backticked).mkString("."), ev.tag) :: compiledData
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
  def loadIvy(coordinates: (String, String, String), verbose: Boolean = true) = timer{
    val (groupId, artifactId, version) = coordinates
    val cacheKey = (replApi.resolvers().hashCode.toString, groupId, artifactId, version)
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
        val resolved = IvyThing(() => replApi.resolvers()).resolveArtifact(
          groupId,
          artifactId,
          version,
          if (verbose) 2 else 1
        ).toSet

        storage.ivyCache() = storage.ivyCache().updated(
          cacheKey,
          resolved.map(_.getAbsolutePath)
        )

        resolved
    }
  }
  abstract class DefaultLoadJar extends LoadJar {

    lazy val ivyThing = IvyThing(() => replApi.resolvers())

    def handleClasspath(jar: File): Unit

    def cp(jar: Path): Unit = {
      handleClasspath(new java.io.File(jar.toString))
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
    eval.sess.frames.head.addClasspath(Seq(jar))
    evalClassloader.add(jar.toURI.toURL)
  }
  def handlePluginClasspath(jar: File) = {
    replApi.sess.frames.head.pluginClassloader.add(jar.toURI.toURL)
  }
  lazy val replApi: ReplAPI = new DefaultReplAPI { outer =>

    def lastException = Interpreter.this.lastException

    def imports = Preprocessor.importBlock(eval.sess.frames.head.imports)
    val colors = colors0
    val prompt = prompt0
    val frontEnd = frontEnd0

    lazy val resolvers =
      Ref(Resolvers.defaultResolvers)

    object load extends DefaultLoadJar with Load {

      def handleClasspath(jar: File) = handleEvalClasspath(jar)

      def apply(line: String) = processExec(line) match{
        case Res.Failure(ex, s) => throw new CompilationError(s)
        case Res.Exception(t, s) => throw t
        case _ =>
      }

      def exec(file: Path): Unit = apply(read(file))

      def module(file: Path) = {
        val (pkg, wrapper) = Util.pathToPackageWrapper(file, wd)
        processModule(
          ImportHook.Source.File(wd/"Main.sc"),
          read(file),
          wrapper,
          pkg,
          true
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
    implicit def tprintColors = pprint.TPrintColors(
      typeColor = colors().`type`()
    )
    implicit val codeColors = new CodeColors{
      def comment = colors().comment()
      def `type` = colors().`type`()
      def literal = colors().literal()
      def keyword = colors().keyword()
      def ident = colors().ident()
    }
    implicit lazy val pprintConfig: Ref[pprint.Config] = {
      Ref.live[pprint.Config]( () =>
        pprint.Config.apply(
          width = width,
          height = height / 2,
          colors = pprint.Colors(
            colors().literal(),
            colors().prefix()
          )
        )
      )

    }

    def show[T: PPrint](implicit cfg: Config) = (t: T) => {
      pprint.tokenize(t, height = 0)(implicitly[PPrint[T]], cfg).foreach(printer.out)
      printer.out("\n")
    }
    def show[T: PPrint](t: T,
                        width: Integer = null,
                        height: Integer = 0,
                        indent: Integer = null,
                        colors: pprint.Colors = null)
                       (implicit cfg: Config = Config.Defaults.PPrintConfig) = {


      pprint.tokenize(t, width, height, indent, colors)(implicitly[PPrint[T]], cfg)
            .foreach(printer.out)
      printer.out("\n")
    }

    def search(target: scala.reflect.runtime.universe.Type) = {
      Interpreter.this.compiler.search(target)
    }
    def compiler = Interpreter.this.compiler.compiler
    def newCompiler() = init()
    def fullHistory = storage.fullHistory()
    def history = Interpreter.this.history


    def width = interp.width

    def height = interp.height

    override def replArgs = Interpreter.this.replArgs.toVector

    object sess extends Session {
      def frames = eval.sess.frames
      def save(name: String) = eval.sess.save(name)
      def delete(name: String) = eval.sess.delete(name)

      def pop(num: Int = 1) = {
        val res = eval.sess.pop(num)
        reInit()
        res
      }
      def load(name: String = "") = {
        val res = eval.sess.load(name)
        reInit()
        res
      }
    }
  }

}
object Interpreter{
  val SheBang = "#!"


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
    if (code.startsWith(SheBang))
      code.substring(code.indexOf('\n'))
    else
      code
  }


  type EvaluateCallback = (Preprocessor.Output, Int, Name) => Res[Evaluated]
  type CacheData = (Imports, Seq[CacheDetails])
  type ProcessedData = (Imports, Seq[CacheDetails], Seq[ImportTree])
  def indexWrapperName(wrapperName: Name, wrapperIndex: Int): Name = {
    Name(wrapperName.raw + (if (wrapperIndex == 1) "" else "_" + wrapperIndex))
  }


}