package ammonite.repl.interp
import ammonite.repl.tools.{IvyThing, Resolver, Resolvers}
import java.io.File
import ammonite.repl.Res
import scala.collection.mutable
import scala.tools.nsc.Settings
import acyclic.file
import fastparse.all._
import ammonite.ops._
import ammonite.repl.Parsers.ImportTree
import pprint.{Config, PPrint}
import annotation.tailrec
import ammonite.repl._
import ammonite.repl.frontend._

import scala.reflect.io.VirtualDirectory

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
                  replArgs: Seq[Bind[_]])
  extends ImportHook.InterpreterInterface{ interp =>


  val hardcodedPredef =
    "import ammonite.repl.frontend.ReplBridge.repl.{pprintConfig, derefPPrint}"


  //this variable keeps track of where should we put the imports resulting from scripts.
  private var scriptImportCallback: Imports => Unit = eval.update

  var lastException: Throwable = null

  private var _compilationCount = 0
  def compilationCount = _compilationCount


  val mainThread = Thread.currentThread()
  val eval = Evaluator(mainThread.getContextClassLoader, 0  )

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var compiler: Compiler = _
  var pressy: Pressy = _
  def evalClassloader = eval.sess.frames.head.classloader
  def init() = {
    Timer("Interpreter init init 0")
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
      settings
    )
    Timer("Interpreter init init compiler")
    pressy = Pressy(
      Classpath.classpath ++ eval.sess.frames.head.classpath,
      dynamicClasspath,
      evalClassloader,

      settings.copy()
    )
    Timer("Interpreter init init pressy")
  }


  val preprocess = Preprocessor(compiler.parse)

  Timer("Interpreter init Preprocess")


  evalClassloader.findClassPublic("ammonite.repl.frontend.ReplBridge$")
  val bridgeCls = evalClassloader.findClassPublic("ammonite.repl.frontend.ReplBridge")

  ReplAPI.initReplBridge(
    bridgeCls.asInstanceOf[Class[ReplAPIHolder]],
    replApi
  )
  Timer("Interpreter init eval")
  init()
  Timer("Interpreter init init")
  val argString = replArgs.zipWithIndex.map{ case (b, idx) =>
    s"""
    val ${b.name} =
      ammonite.repl.frontend.ReplBridge.repl.replArgs($idx).value.asInstanceOf[${b.typeTag.tpe}]
    """
  }.mkString("\n")

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
  var predefImports = Imports(Nil)
  for( (sourceCode, wrapperName) <- predefs) {
    val pkgName = Seq(Name("ammonite"), Name("predef"))
    processModule0(sourceCode, wrapperName, pkgName, predefImports) match{
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
  }

  eval.sess.save()
  Timer("Interpreter init predef 0")
  init()
  Timer("Interpreter init predef 1")

  val importHooks = Ref(Map[String, ImportHook](
    "file" -> ImportHook.File,
    "url" -> ImportHook.Http,
    "ivy" -> ImportHook.Ivy
  ))

  def resolveImportHooks(stmts: Seq[String]): Res[(Imports, Seq[String])] = {
    val hookedStmts = mutable.Buffer.empty[String]
    val importTrees = mutable.Buffer.empty[ImportTree]
    for(stmt <- stmts) {
      Parsers.ImportSplitter.parse(stmt) match{
        case f: Parsed.Failure => hookedStmts.append(stmt)
        case Parsed.Success(parsedTrees, _) =>
          var currentStmt = stmt
          for(importTree <- parsedTrees){
            if (importTree.prefix(0)(0) == '$') {
              pprint.log(importTree)
              currentStmt = currentStmt.patch(
                importTree.start,
                "scala._".padTo(importTree.end - importTree.start, ' '),
                importTree.end
              )
              importTrees.append(importTree)
            }
          }
          hookedStmts.append(currentStmt)
      }
    }

    for {
      hookImports <- Res.map(importTrees){ tree =>
        val hook = importHooks()(tree.prefix.head.stripPrefix("$"))
        for{
          hooked <- hook.handle(tree.copy(prefix = tree.prefix.drop(1)), this)
          hookResults <- Res.map(hooked){
            case res: ImportHook.Result.Source =>
              processModule(res.code, res.wrapper, res.pkg)
              Res.Success(Imports(Nil))
            case res: ImportHook.Result.ClassPath =>
              eval.sess.frames.head.addClasspath(Seq(res.file.toIO))
              evalClassloader.add(res.file.toIO.toURI.toURL)
              init()
              Res.Success(Imports(Nil))
          }
        } yield hookResults
      }
    } yield (Imports(hookImports.flatten.flatMap(_.value)), stmts)
  }

  def processLine(code: String, stmts: Seq[String], fileName: String): Res[Evaluated] = for{
    _ <- Catching { case ex =>
      Res.Exception(ex, "Something unexpected went wrong =(")
    }

    (hookImports, hookedStmts) <- resolveImportHooks(stmts)

    processed <- preprocess.transform(
      hookedStmts,
      eval.getCurrentLine,
      "",
      Seq(Name("$sess")),
      Name("cmd" + eval.getCurrentLine),
      eval.sess.frames.head.imports ++ hookImports,
      prints => s"ammonite.repl.frontend.ReplBridge.repl.Internal.combinePrints($prints)"
    )

    out <- evaluateLine(
      processed, printer,
      fileName, Name("cmd" + eval.getCurrentLine)
    )
  } yield out


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
                         pkgName: Seq[Name]) = for {
    (cls, newImports) <- cachedCompileBlock(
      processed,
      printer,
      wrapperName,
      fileName,
      pkgName,
      "scala.Iterator[String]()"
    )
    res <- eval.processScriptBlock(cls, newImports, wrapperName, pkgName)
  } yield res


  def cachedCompileBlock(processed: Preprocessor.Output,
                         printer: Printer,
                         wrapperName: Name,
                         fileName: String,
                         pkgName: Seq[Name],
                         printCode: String): Res[(Class[_], Imports)] = {

    Timer("cachedCompileBlock 1")

    val fullyQualifiedName = (pkgName :+ wrapperName).map(_.encoded).mkString(".")
    val tag = Interpreter.cacheTag(
      processed.code, Nil, eval.sess.frames.head.classloader.classpathHash
    )
    Timer("cachedCompileBlock 2")
    val compiled = storage.compileCacheLoad(fullyQualifiedName, tag) match {
      case Some((classFiles, newImports)) =>
        compiler.addToClasspath(classFiles)
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
    Timer("cachedCompileBlock 3")
    val x = for {
      (classFiles, newImports) <- compiled
      _ = Timer("cachedCompileBlock 4")
      cls <- eval.loadClass(fullyQualifiedName, classFiles)
    } yield (cls, newImports)

    x
  }

  def processModule(code: String, wrapperName: Name, pkgName: Seq[Name]) = {

    processModule0(code, wrapperName, pkgName, predefImports)
  }

  def processModule0(code: String,
                     wrapperName: Name,
                     pkgName: Seq[Name],
                     startingImports: Imports): Res[Imports] = for{
    blocks <- Preprocessor.splitScript(Interpreter.skipSheBangLine(code))
    (hookImports, hookedStmts) <- resolveImportHooks(blocks.flatMap(_._2))
    res <- processCorrectScript(
      blocks,
      startingImports ++ hookImports,
      pkgName,
      wrapperName,
      (processed, wrapperIndex, indexedWrapperName) =>
        withContextClassloader(
          processScriptBlock(
            processed, printer,
            Interpreter.indexWrapperName(wrapperName, wrapperIndex),
            wrapperName.raw + ".scala", pkgName
          )
        )
    )
  } yield res

  def processExec(code: String): Res[Imports] = for {
    blocks <- Preprocessor.splitScript(Interpreter.skipSheBangLine(code))
    (hookImports, hookedStmts) <- resolveImportHooks(blocks.flatMap(_._2))
    res <- processCorrectScript(
      blocks,
      eval.sess.frames.head.imports ++ hookImports,
      Seq(Name("$sess")),
      Name("cmd" + eval.getCurrentLine),
      { (processed, wrapperIndex, indexedWrapperName) =>
        evaluateLine(
          processed,
          printer,
          s"Main$wrapperIndex.scala",
          indexedWrapperName
        )
      }
    )
  } yield res


  def processCorrectScript(blocks: Seq[(String, Seq[String])],
                           startingImports: Imports,
                           pkgName: Seq[Name],
                           wrapperName: Name,
                           evaluate: Interpreter.EvaluateCallback)
                          : Res[Imports] = {

    Timer("processCorrectScript 1")
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
                      wrapperIndex: Int): Res[Imports] = {
      if (blocks.isEmpty) {
        // No more blocks
        // if we have imports to pass to the upper layer we do that
        outerScriptImportCallback(lastImports)
        Res.Success(lastImports)
      } else {
        Timer("processScript loop 0")
        // imports from scripts loaded from this script block will end up in this buffer
        var nestedScriptImports = Imports(Nil)
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
            loop(blocks.tail, scriptImports ++ last, last, wrapperIndex + 1)
          case Res.Skip => loop(blocks.tail, scriptImports, lastImports, wrapperIndex + 1)
        }
      }
    }
    // wrapperIndex starts off as 1, so that consecutive wrappers can be named
    // Wrapper, Wrapper2, Wrapper3, Wrapper4, ...
    try loop(blocks, startingImports, Imports(Nil), wrapperIndex = 1 )
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
    val psOpt =
      storage.ivyCache()
        .get((replApi.resolvers.hashCode.toString, groupId, artifactId, version))
        .map(_.map(new java.io.File(_)))
        .filter(_.forall(_.exists()))

    psOpt match{
      case Some(ps) => ps
      case None =>
        IvyThing(() => replApi.resolvers()).resolveArtifact(
          groupId,
          artifactId,
          version,
          if (verbose) 2 else 1
        ).toSet
    }
  }
  abstract class DefaultLoadJar extends LoadJar with Resolvers {

    lazy val ivyThing = IvyThing(() => resolvers)

    def handleClasspath(jar: File): Unit

    def cp(jar: Path): Unit = {
      handleClasspath(new java.io.File(jar.toString))
      init()
    }
    def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit = {
      val resolved = loadIvy(coordinates, verbose)
      val (groupId, artifactId, version) = coordinates
      storage.ivyCache() = storage.ivyCache().updated(
        (resolvers.hashCode.toString, groupId, artifactId, version),
        resolved.map(_.getAbsolutePath)
      )

      resolved.foreach(handleClasspath)


      init()
    }
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

      def resolvers: List[Resolver] =
        outer.resolvers()

      def handleClasspath(jar: File) = {
        eval.sess.frames.head.addClasspath(Seq(jar))
        evalClassloader.add(jar.toURI.toURL)
      }

      def apply(line: String) = processExec(line) match{
        case Res.Failure(ex, s) => throw new CompilationError(s)
        case Res.Exception(t, s) => throw t
        case _ =>
      }

      def exec(file: Path): Unit = apply(read(file))

      def module(file: Path): Unit = {
        val (pkg, wrapper) = Util.pathToPackageWrapper(file, wd)
        processModule(read(file), wrapper, pkg) match{
          case Res.Failure(ex, s) => throw new CompilationError(s)
          case Res.Exception(t, s) => throw t
          case x => //println(x)
        }
        init()
      }

      object plugin extends DefaultLoadJar {
        def resolvers: List[Resolver] =
          outer.resolvers()

        def handleClasspath(jar: File) =
          sess.frames.head.pluginClassloader.add(jar.toURI.toURL)
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
        init()
        res
      }
      def load(name: String = "") = {
        val res = eval.sess.load(name)
        init()
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
    "cache" + bytes.map("%02x".format(_)).mkString //add prefix to make sure it begins with a letter
  }

  def skipSheBangLine(code: String)= {
    if (code.startsWith(SheBang))
      code.substring(code.indexOf('\n'))
    else
      code
  }

  type EvaluateCallback = (Preprocessor.Output, Int, Name) => Res[Evaluated]

  def indexWrapperName(wrapperName: Name, wrapperIndex: Int): Name = {
    Name(wrapperName.raw + (if (wrapperIndex == 1) "" else "_" + wrapperIndex))
  }


}