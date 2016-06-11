package ammonite.repl.interp
import ammonite.repl.tools.{IvyThing, Resolvers, Resolver}
import java.io.File
import ammonite.repl.Res
import scala.collection.mutable
import scala.tools.nsc.Settings
import acyclic.file
import fastparse.all._
import ammonite.ops._
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
                  wd: Path,
                  replArgs: Seq[Bind[_]]){ interp =>


  val hardcodedPredef =
    """import ammonite.repl.frontend.ReplBridge.repl.{pprintConfig, derefPPrint}
      |""".stripMargin

  val SheBang = "#!"

  var lastException: Throwable = null

  private var _compilationCount = 0
  def compilationCount = _compilationCount

  def processLine(code: String,
                  stmts: Seq[String],
                  fileName: String): Res[Evaluated] = {
    for{
      _ <- Catching { case ex =>
        Res.Exception(ex, "Something unexpected went wrong =(")
      }
      (wrappedCode, importsLength) <- preprocess.transform(
        stmts,
        eval.getCurrentLine,
        "",
        "ammonite.session",
        "cmd" + eval.getCurrentLine,
        eval.sess.frames.head.imports,
        prints =>
        s"""
          ammonite.repl
                  .frontend
                  .ReplBridge
                  .repl
                  .Internal
                  .combinePrints($prints)
          """
      )
      out <- evaluateLine(wrappedCode, importsLength, printer, fileName)
    } yield out
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

  def compileClass(code: (String, Int),
                   printer: Printer,
                   fileName: String): Res[(Util.ClassFiles, Seq[ImportData])] = for {
    compiled <- Res.Success{
      compiler.compile(code._1.getBytes, printer, code._2, fileName)
    }
    _ = _compilationCount += 1
    (classfiles, imports) <- Res[(Util.ClassFiles, Seq[ImportData])](
      compiled,
      "Compilation Failed"
    )
  } yield (classfiles, imports)

  def evaluateLine(code: String,
                   importsLength: Int,
                   printer: Printer,
                   fileName: String,
                   extraImports: Seq[ImportData] = Seq() ): Res[Evaluated] = {

    for{
      _ <- Catching{ case e: ThreadDeath => Evaluator.interrupted(e) }
      (classFiles, newImports) <- compileClass(
        (code, importsLength),
        printer,
        fileName
      )
      res <- withContextClassloader{
        eval.processLine(
          classFiles,
          newImports,
          printer,
          fileName,
          extraImports
        )

      }
    } yield res
  }

  def processScriptBlock(wrappedCode: String,
                         importsLength: Int,
                         printer: Printer,
                         wrapperName: String,
                         fileName: String,
                         pkgName: String) = for {
    (cls, newImports) <- cachedCompileBlock(
      wrappedCode, importsLength, printer, wrapperName, fileName, pkgName
    )
    res <- eval.processScriptBlock(cls, newImports, wrapperName, pkgName)
  } yield res


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


  def cachedCompileBlock(wrappedCode: String,
                         importsLength: Int,
                         printer: Printer,
                         wrapperName: String,
                         fileName: String,
                         pkgName: String,
                         printCode: String = ""): Res[(Class[_], Seq[ImportData])] = {

    Timer("cachedCompileBlock 1")

    val fullyQualifiedName = pkgName + "." + wrapperName
    val tag = cacheTag(wrappedCode, Nil, eval.sess.frames.head.classloader.classpathHash)
    Timer("cachedCompileBlock 2")
    val compiled = storage.compileCacheLoad(fullyQualifiedName, tag) match {
      case Some((classFiles, newImports)) =>
        compiler.addToClasspath(classFiles)
        Res.Success((classFiles, newImports))
      case _ =>
        val noneCalc = for {
          (classFiles, newImports) <- compileClass(
            (wrappedCode, importsLength), printer, fileName
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

  def processModule(code: String, wrapperName: String, pkgName: String) = {
    processModule0(code, wrapperName, pkgName, predefImports)
  }

  def indexWrapperName(wrapperName: String, wrapperIndex: Int) = {
    wrapperName + (if (wrapperIndex == 1) "" else wrapperIndex)
  }
  def processModule0(code: String,
                    wrapperName: String,
                    pkgName: String,
                    startingImports: Seq[ImportData]): Res[Seq[ImportData]] = {
    processScript(
      skipSheBangLine(code),
      startingImports,
      pkgName,
      wrapperName,
      (wrappedCode, importsLength, wrapperIndex) =>
        withContextClassloader(
          processScriptBlock(
            wrappedCode, importsLength, printer,
            indexWrapperName(wrapperName, wrapperIndex),
            wrapperName + ".scala", pkgName
          )
        )
    )
  }


  def processExec(code: String): Res[Seq[ImportData]] = {
    processScript(
      skipSheBangLine(code),
      predefImports,
      "ammonite.session",
      "cmd" + eval.getCurrentLine,
      { (wrappedCode, importsLength, wrapperIndex) =>
        evaluateLine(wrappedCode, importsLength, printer, s"Main$wrapperIndex.scala")
      }
    )
  }


  private def skipSheBangLine(code: String)= {
    if (code.startsWith(SheBang))
      code.substring(code.indexOf('\n'))
     else
      code
  }


  //this variable keeps track of where should we put the imports resulting from scripts.
  private var scriptImportCallback: Seq[ImportData] => Unit = eval.update

  type EvaluateCallback = (String, Int, Int) => Res[Evaluated]

  def errMsg(msg: String, code: String, expected: String, idx: Int): String = {
    val locationString = {
      val (first, last) = code.splitAt(idx)
      val lastSnippet = last.split('\n').headOption.getOrElse("")
      val firstSnippet = first.reverse.split('\n').lift(0).getOrElse("").reverse
      firstSnippet + lastSnippet + "\n" + (" " * firstSnippet.length) + "^"
    }

    s"Syntax Error: $msg\n$locationString"
  }

  def processCorrectScript(rawParsedCode: Parsed.Success[Seq[(String, Seq[String])]],
                           startingImports: Seq[ImportData],
                           pkgName: String,
                           wrapperName: String,
                           evaluate: EvaluateCallback)
                          : Res[Seq[ImportData]] = {
    var offset = 0
    val blocks = mutable.Buffer[(String, Seq[String])]()

    // comment holds comments or empty lines above the code which is not caught along with code
    for( (comment, code) <- rawParsedCode.get.value ){
      val ncomment = comment + "\n"*offset

      // 1 is added as Separator parser eats up the '\n' following @
      offset = offset + comment.count(_ == '\n') + code.map(_.count(_ == '\n')).sum + 1
      blocks.append((ncomment, code))
    }


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
                      scriptImports: Seq[ImportData],
                      lastImports: Seq[ImportData],
                      wrapperIndex: Int): Res[Seq[ImportData]] = {
      if (blocks.isEmpty) {
        // No more blocks
        // if we have imports to pass to the upper layer we do that
        outerScriptImportCallback(lastImports)
        Res.Success(lastImports)
      } else {
        Timer("processScript loop 0")
        // imports from scripts loaded from this script block will end up in this buffer
        var nestedScriptImports = Seq.empty[ImportData]
        scriptImportCallback = { imports =>
          nestedScriptImports = Frame.mergeImports(nestedScriptImports, imports)
        }
        // pretty printing results is disabled for scripts

        val res = for{
          (wrappedCode, importsLength) <- {

            preprocess.transform(
              blocks.head._2,
              "",
              blocks.head._1,
              pkgName,
              indexWrapperName(wrapperName, wrapperIndex),
              scriptImports,
              _ => ""
            )
          }
          ev <- evaluate(
            wrappedCode,
            importsLength,
            wrapperIndex
          )
        } yield ev

        res match {
          case r: Res.Failure => r
          case r: Res.Exception => r
          case Res.Success(ev) =>
            val last = Frame.mergeImports(ev.imports, nestedScriptImports)
            loop(blocks.tail, Frame.mergeImports(scriptImports, last), last, wrapperIndex + 1)
          case Res.Skip => loop(blocks.tail, scriptImports, lastImports, wrapperIndex + 1)
        }
      }
    }
    try {
      loop(
        blocks,
        Seq(), Seq(),
        // starts off as 1, so that consecutive wrappers can be named
        // Wrapper, Wrapper2, Wrapper3, Wrapper4, ...
        wrapperIndex = 1
      )


    } finally {
      scriptImportCallback = outerScriptImportCallback
    }
  }
  //common stuff in proccessModule and processExec
  def processScript(code: String,
                    startingImports: Seq[ImportData],
                    pkgName: String,
                    wrapperName: String,
                    evaluate: EvaluateCallback)
                    : Res[Seq[ImportData]] = {

    Timer("processScript 0a")
    Parsers.splitScript(code) match {
      case f: Parsed.Failure =>
        Timer("processScriptFailed 0b")
        Res.Failure(None, errMsg(f.msg, code, f.extra.traced.expected, f.index))
      case s: Parsed.Success[Seq[(String, Seq[String])]] =>
        Timer("processCorrectScript 0b")
        processCorrectScript(s, startingImports, pkgName, wrapperName, evaluate)
    }
  }

  def handleOutput(res: Res[Evaluated]) = {
    res match{
      case Res.Skip => true
      case Res.Exit(value) =>
        pressy.shutdownPressy()
        false
      case Res.Success(ev) =>
        eval.update(ev.imports)
        true
      case Res.Failure(ex, msg) =>    lastException = ex.getOrElse(lastException)
                                  true
      case Res.Exception(ex, msg) =>  lastException = ex
                                      true
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
      val (groupId, artifactId, version) = coordinates
      val psOpt =
        storage.ivyCache()
                 .get((resolvers.hashCode.toString, groupId, artifactId, version))
                 .map(_.map(new java.io.File(_)))
                 .filter(_.forall(_.exists()))

      psOpt match{
        case Some(ps) => ps.foreach(handleClasspath)
        case None =>
          val resolved = ivyThing.resolveArtifact(
            groupId,
            artifactId,
            version,
            if (verbose) 2 else 1
          )

          storage.ivyCache() = storage.ivyCache().updated(
            (resolvers.hashCode.toString, groupId, artifactId, version),
            resolved.map(_.getAbsolutePath).toSet
          )

          resolved.foreach(handleClasspath)
      }

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
      pprint.tokenize(t, height = 0)(implicitly[PPrint[T]], cfg).foreach(print)
      println()
    }
    def show[T: PPrint](t: T,
                        width: Integer = null,
                        height: Integer = 0,
                        indent: Integer = null,
                        colors: pprint.Colors = null)
                       (implicit cfg: Config = Config.Defaults.PPrintConfig) = {


      pprint.tokenize(t, width, height, indent, colors)(implicitly[PPrint[T]], cfg)
            .foreach(print)
      println()
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

  val mainThread = Thread.currentThread()
  val eval = Evaluator(
    mainThread.getContextClassLoader,
    0
  )

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
  val argString =
    replArgs.zipWithIndex
            .map{ case (b, idx) =>
              s"""
              val ${b.name} =
                ammonite.repl
                        .frontend
                        .ReplBridge
                        .repl
                        .replArgs($idx)
                        .value
                        .asInstanceOf[${b.typeTag.tpe}]
              """
            }
            .mkString("\n")

  val predefs = Seq(
    (hardcodedPredef, "HardcodedPredef", "ammonite.predef"),
    (predef, "Predef", "ammonite.predef"),
    (storage.loadPredef, "LoadedPredef", "ammonite.predef"),
    (argString, "ArgsPredef", "ammonite.predef")
  )

  val predefImports = predefs.foldLeft(Seq.empty[ImportData]){
    case (prevImports, (sourceCode, wrapperName, pkgName)) =>
      processModule0(sourceCode, wrapperName, pkgName, prevImports) match{
        case Res.Success(imports) => Frame.mergeImports(prevImports, imports)
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
}
