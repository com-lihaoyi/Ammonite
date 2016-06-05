package ammonite.repl.interp
import ammonite.repl.tools.{IvyThing, Resolvers, Resolver}
import java.io.File
import java.nio.file.NotDirectoryException
import org.apache.ivy.plugins.resolver.RepositoryResolver

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
                  replArgs: Seq[Bind[_]]){ interp =>


  val hardcodedPredef =
    """import ammonite.repl.frontend.ReplBridge.repl
      |import ammonite.repl.frontend.ReplBridge.repl.{pprintConfig, derefPPrint}
      |""".stripMargin

  val SheBang = "#!"

  var lastException: Throwable = null

  def processLine(code: String,
                  stmts: Seq[String],
                  fileName: String = "Main.scala") = {
    for{
      _ <- Catching { case ex =>
        Res.Exception(ex, "Something unexpected went wrong =(")
      }
      Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
      out <- evaluateLine(code, printSnippet, printer, fileName)
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

  def evaluateLine(code: String,
                   printSnippet: Seq[String],
                   printer: Printer,
                   fileName: String,
                   extraImports: Seq[ImportData] = Seq() ) = withContextClassloader{

      eval.processLine(
        code,
        s"""
        ammonite.repl
                .frontend
                .ReplBridge
                .repl
                .Internal
                .combinePrints(${printSnippet.mkString(", ")})
        """,
        printer,
        fileName,
        extraImports
      )

  }

  def processModule(code: String,
                    wrapperName: String,
                    pkgName: String): Res[Seq[ImportData]] = {
    processScript(
      skipSheBangLine(code),
      (code, imports, index) =>
        withContextClassloader(
          eval.processScriptBlock(
            code, imports, printer, wrapperName + index, pkgName
          )
        )
    )
  }


  def processExec(code: String): Res[Seq[ImportData]] = {
    processScript(
      skipSheBangLine(code),
      { (c, i, index) => evaluateLine(c, Nil, printer, s"Main$index.scala", i) }
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

  type EvaluateCallback = (String, Seq[ImportData], Int) => Res[Evaluated]

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
                           evaluate: EvaluateCallback)
                          : Res[Seq[ImportData]] = {
    var offset = 0
    val parsedCode = mutable.Buffer[(String, Seq[String])]()

    // comment holds comments or empty lines above the code which is not caught along with code
    for( (comment, code) <- rawParsedCode.get.value ){
      val ncomment = comment + "\n"*offset

      // 1 is added as Separator parser eats up the '\n' following @
      offset = offset + comment.count(_ == '\n') + code.map(_.count(_ == '\n')).sum + 1
      parsedCode.append((ncomment, code))
    }

    val parsedHardcodedPredef  = Parsers.splitScript(hardcodedPredef + "\n" + predef).get.value
    val blocks0 = parsedHardcodedPredef ++ parsedCode

    val blocks = for{
      (comment, code) <- blocks0
    } yield preprocess(code,"",comment)

    Timer("processCorrectScript 1")
    val errors = blocks.collect { case Res.Failure(ex, err) => err }
    Timer("processCorrectScript 2")
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
    @tailrec def loop(blocks: Seq[Preprocessor.Output],
                      scriptImports: Seq[ImportData],
                      lastImports: Seq[ImportData],
                      index: Int): Res[Seq[ImportData]] = {
      if (blocks.isEmpty) {
        // No more blocks
        // if we have imports to pass to the upper layer we do that
        outerScriptImportCallback(lastImports)
        Res.Success(lastImports)
      } else {
        Timer("processScript loop 0")
        // imports from scripts loaded from this script block will end up in this buffer
        val nestedScriptImports = mutable.Buffer.empty[ImportData]
        scriptImportCallback = { imports => nestedScriptImports ++= imports }
        // pretty printing results is disabled for scripts

        val Preprocessor.Output(code, _) = blocks.head

        Timer("processScript loop 1")
        val ev = evaluate(code, scriptImports, index)
        Timer("processScript loop 2")
        ev match {
          case r: Res.Failure => r
          case r: Res.Exception => r
          case Res.Success(ev) =>
            val last = Frame.mergeImports(ev.imports, nestedScriptImports)
            loop(blocks.tail, Frame.mergeImports(scriptImports, last), last, index + 1)
          case Res.Skip => loop(blocks.tail, scriptImports, lastImports, index + 1)
        }
      }
    }
    try {
      if (errors.isEmpty) {
        loop(blocks.collect { case Res.Success(o) => o }, Seq(), Seq(), 0)
      } else {
        printer.error(errors.mkString("\n"))
        Res.Success(Nil)
      }

    } finally {
      scriptImportCallback = outerScriptImportCallback
    }
  }
  //common stuff in proccessModule and processExec
  def processScript(code: String,
                    evaluate: (String, Seq[ImportData], Int) => Res[Evaluated])
                    : Res[Seq[ImportData]] = {

    Timer("processScript 0a")
    Parsers.splitScript(code) match {
      case f: Parsed.Failure =>
        Timer("processScriptFailed 0b")
        Res.Failure(None, errMsg(f.msg, code, f.extra.traced.expected, f.index))
      case s: Parsed.Success[Seq[(String, Seq[String])]] =>
        Timer("processCorrectScript 0b")
        processCorrectScript(s, evaluate)
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

    def imports = interp.eval.previousImportBlock
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
        val (pkg, wrapper) = Util.pathToPackageWrapper(file)
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
    compiler.compile,
    0,
    storage.compileCacheLoad,
    storage.compileCacheSave,
    compiler.addToClasspath
  )

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var compiler: Compiler = _
  var pressy: Pressy = _
  def evalClassloader = eval.sess.frames.head.classloader
  def init() = {
    Timer("Interpreter init init 0")
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
      settings
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

  processModule(
    hardcodedPredef + "\n" + predef,
    "HardcodedPredef",
    "ammonite.predef"
  )
  init()
  processModule(
    storage.loadPredef + "\n" + argString,
    "LoadedPredef",
    "ammonite.predef"
  )
  eval.sess.save()
  Timer("Interpreter init predef 0")
  init()
  Timer("Interpreter init predef 1")
}
