package ammonite.interp


import java.io.OutputStream
import java.nio.charset.StandardCharsets

import ammonite.runtime.{Classpath, Evaluator}
import ammonite.util.{ImportData, Imports, Name, Printer}
import ammonite.util.Util.newLine

import scala.collection.mutable
import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter
import scala.util.Try


/**
 * Encapsulates (almost) all the ickiness of Scalac so it doesn't leak into
 * the rest of the codebase. Makes use of a good amount of mutable state
 * for things like the log-output-forwarder or compiler-plugin-output because
 * These things are hard-coded into Scalac and can't be passed in from run to
 * run.
 *
 * Turns source-strings into the bytes of classfiles, possibly more than one
 * classfile per source-string (e.g. inner classes, or lambdas). Also lets
 * you query source strings using an in-built presentation compiler
 */
trait Compiler{
  def compiler: nsc.Global
  def compile(src: Array[Byte],
              printer: Printer,
              importsLen0: Int,
              userCodeNestingLevel: Int,
              fileName: String): Option[Compiler.Output]

  def search(name: scala.reflect.runtime.universe.Type): Option[String]
  /**
   * Either the statements that were parsed or the error message
   */
  def parse(fileName: String, line: String): Either[String, Seq[Global#Tree]]
  var importsLen = 0
  var userCodeNestingLevel = -1

}
object Compiler{
  /**
    * Writes files to dynamicClasspath. Needed for loading cached classes.
    */
  def addToClasspath(classFiles: Traversable[(String, Array[Byte])],
                     dynamicClasspath: VirtualDirectory): Unit = {

    for((name, bytes) <- classFiles){
      val output = writeDeep(dynamicClasspath, name.split('.').toList, ".class")
      output.write(bytes)
      output.close()
    }

  }

  def writeDeep(d: VirtualDirectory,
                path: List[String],
                suffix: String): OutputStream = path match {
    case head :: Nil => d.fileNamed(path.head + suffix).output
    case head :: rest =>
      writeDeep(
        d.subdirectoryNamed(head).asInstanceOf[VirtualDirectory],
        rest, suffix
      )
    // We should never write to an empty path, and one of the above cases
    // should catch this and return before getting here
    case Nil => ???
  }
  /**
   * If the Option is None, it means compilation failed
   * Otherwise it's a Traversable of (filename, bytes) tuples
   */
  case class Output(
    classFiles: Vector[(String, Array[Byte])],
    imports: Imports,
    usedEarlierDefinitions: Option[Seq[String]]
  )

  /**
    * Converts a bunch of bytes into Scalac's weird VirtualFile class
    *
    * Can only take in a relative path for the VirtualFile! Otherwise
    * fails with a weird assert in the presentation compiler:
    *
    * [Current.sc]: exception during background compile:
    * java.lang.AssertionError:
    * assertion failed: (
    *   ammonite/predef//Users/lihaoyi/Dropbox/Github/<-wrapped to next line->
    *   Ammonite/target/tempAmmoniteHome/predef.sc,false,16,16)
    *
    * It appears that passing in the filename *only* results in the correct
    * stack traces (e.g. Foo.sc:425 rather than Users/lihaoyi/Foo.sc:45), so we
    * do that and assert to make sure it's only one path segment
    */
  def makeFile(src: Array[Byte], name: String) = {
    assert(!name.contains('/'))
    val singleFile = new io.VirtualFile(name)
    val output = singleFile.output
    output.write(src)
    output.close()
    singleFile
  }

  /**
   * Converts Scalac's weird Future type
   * into a standard scala.concurrent.Future
   */
  def awaitResponse[T](func: Response[T] => Unit): T = {
    val r = new Response[T]
    func(r)
    r.get.fold(
      x => x,
      e => throw e
    )
  }



  def makeReporter(errorLogger: => String => Unit,
                   warningLogger: => String => Unit,
                   infoLogger: => String => Unit,
                   outerSettings: Settings) = {
    new AbstractReporter {
      def displayPrompt(): Unit = ???

      def display(pos: Position, msg: String, severity: Severity) = {
        severity match{
          case ERROR =>
            Classpath.traceClasspathProblem(s"ERROR: $msg")
            errorLogger(Position.formatMessage(pos, msg, false))
          case WARNING =>
            warningLogger(Position.formatMessage(pos, msg, false))
          case INFO =>
            infoLogger(Position.formatMessage(pos, msg, false))
        }
      }

      val settings = outerSettings
    }
  }


  def apply(classpath: Seq[java.io.File],
            dynamicClasspath: VirtualDirectory,
            evalClassloader: => ClassLoader,
            pluginClassloader: => ClassLoader,
            shutdownPressy: () => Unit,
            settings: Settings): Compiler = new Compiler{

    if(sys.env.contains("DIE"))???
    val PluginXML = "scalac-plugin.xml"
    lazy val plugins0 = {
      import scala.collection.JavaConverters._
      val loader = pluginClassloader

      val urls = loader
        .getResources(PluginXML)
        .asScala
        .toVector

      val pluginNames = for {
        url <- urls
        elem = scala.xml.XML.load(url.openStream())
        name = (elem \\ "plugin" \ "name").text
        className = (elem \\ "plugin" \ "classname").text
        // Hardcode exclusion of "com.lihaoyi" %% "acyclic", since that's
        // pretty useless and can cause problems conflicting with other plugins
        if className != "acyclic.plugin.RuntimePlugin"
        if name.nonEmpty && className.nonEmpty
      } yield (name, className)

      /*
       * If there's also a '-sources' jar for the plugin in the classpath,
       * there will be >= 1 plugins registered which may cause cryptic error messages
       * and break the REPL completely, so `.distinct` is called on pluginNames
       */
      val plugins = for {
        (name, className) <- pluginNames.distinct
        classOpt =
          try Some(loader.loadClass(className))
          catch { case _: ClassNotFoundException => None }
      } yield (name, className, classOpt)

      val notFound = plugins.collect{case (name, className, None) => (name, className) }
      if (notFound.nonEmpty) {
        for ((name, className) <- notFound.sortBy(_._1))
          Console.err.println(s"Implementation $className of plugin $name not found.")
      }

      plugins.collect{case (name, _, Some(cls)) => name -> cls }
    }

    var errorLogger: String => Unit = s => ()
    var warningLogger: String => Unit = s => ()
    var infoLogger: String => Unit = s => ()

    var lastImports = Seq.empty[ImportData]
    var usedEarlierDefinitions = Option.empty[Seq[String]]

    val (vd, reporter, compiler) = {

      val (dirDeps, jarDeps) = classpath.partition(_.isDirectory)

      val jcp = GlobalInitCompat.initGlobalClasspath(
        dirDeps, jarDeps, dynamicClasspath, settings
      )
      val vd = new io.VirtualDirectory("(memory)", None)
      if (Classpath.traceClasspathIssues) {
        settings.Ylogcp.value = true
        println("jardeps")
        jarDeps.foreach(p => println(s"${p.getName.takeRight(4)} $p"))
        println("finished")
      }

      settings.outputDirs.setSingleOutput(vd)

      settings.nowarnings.value = true
      // Otherwise the presence of `src`'s source files mixed with
      // classfiles causes scalac to get confused
      settings.termConflict.value = "object"

      val reporter = makeReporter(errorLogger, warningLogger, infoLogger, settings)

      val scalac = GlobalInitCompat.initGlobal(
        settings, reporter, jcp,
        evalClassloader,
        createPlugins = g => {
          List(
            new ammonite.interp.AmmonitePlugin(
              g,
              lastImports = _,
              uses => usedEarlierDefinitions = Some(uses),
              userCodeNestingLevel,
              importsLen
            )
          ) ++ {
            for {
              (name, cls) <- plugins0
              plugin = Plugin.instantiate(cls, g)
              initOk =
              try CompilerCompatibility.pluginInit(plugin, Nil, g.globalError)
              catch { case ex: Exception =>
                Console.err.println(s"Warning: disabling plugin $name, initialization failed: $ex")
                false
              }
              if initOk
            } yield plugin
          }
        }
      )


      // Initialize scalac to the parser phase immediately, so we can start
      // using Compiler#parse even if we haven't compiled any compilation
      // units yet due to caching
      val run = new scalac.Run()
      scalac.phase = run.parserPhase
      run.cancel()
      (vd, reporter, scalac)
    }

    def search(target: scala.reflect.runtime.universe.Type) = {
      def resolve(path: String*): compiler.Symbol = {
        var curr = path.toList
        var start: compiler.Symbol = compiler.RootClass
        while(curr != Nil){
          val head :: rest = curr
          start = start.typeSignature.member(compiler.newTermName(head))
          curr = rest
        }
        start
      }
      var thingsInScope = Map[compiler.Symbol, List[compiler.Name]](
        resolve() -> List(),
        resolve("java", "lang") -> List(),
        resolve("scala") -> List(),
        resolve("scala", "Predef") -> List()
      )
      var level = 5
      var found: Option[String] = None
      while(level > 0){
        thingsInScope = for {
          (sym, path) <- thingsInScope
          // No clue why this one blows up
          m <- Try(sym.typeSignature.members).toOption.toSeq.flatten
        } yield (m, m.name :: path)
        thingsInScope.find(target.typeSymbol.fullName == _._1.fullName).foreach{ path =>
          level = 0
          found = Some(path._2.mkString("."))
        }
      }
      found
    }



    /**
     * Compiles a blob of bytes and spits of a list of classfiles
      * importsLen0 is the length of topWrapper appended above code by wrappedCode function
      * It is passed to AmmonitePlugin to decrease this much offset from each AST node
      * corresponding to the actual code so as to correct the line numbers in error report
     */
    def compile(src: Array[Byte],
                printer: Printer,
                importsLen0: Int,
                userCodeNestingLevel: Int,
                fileName: String): Option[Output] = {

      def enumerateVdFiles(d: VirtualDirectory): Iterator[AbstractFile] = {
        val (subs, files) = d.iterator.partition(_.isDirectory)
        files ++ subs.map(_.asInstanceOf[VirtualDirectory]).flatMap(enumerateVdFiles)
      }



      compiler.reporter.reset()
      this.errorLogger = printer.error
      this.warningLogger = printer.warning
      this.infoLogger = printer.info
      val singleFile = makeFile(src, fileName)
      this.importsLen = importsLen0
      this.userCodeNestingLevel = userCodeNestingLevel
      val run = new compiler.Run()
      vd.clear()

      run.compileFiles(List(singleFile))

      val outputFiles = enumerateVdFiles(vd).toVector

      if (reporter.hasErrors) None
      else {

        shutdownPressy()

        val files = for(x <- outputFiles if x.name.endsWith(".class")) yield {
          val segments = x.path.split("/").toList.tail
          val output = writeDeep(dynamicClasspath, segments, "")
          output.write(x.toByteArray)
          output.close()
          (x.path.stripPrefix("(memory)/").stripSuffix(".class").replace('/', '.'), x.toByteArray)
        }

        val imports = lastImports.toList
        Some(Output(files, Imports(imports), usedEarlierDefinitions))

      }
    }


    def parse(fileName: String, line: String): Either[String, Seq[Global#Tree]] = {
      val errors = mutable.Buffer.empty[String]
      val warnings = mutable.Buffer.empty[String]
      val infos = mutable.Buffer.empty[String]
      errorLogger = errors.append(_)
      warningLogger = warnings.append(_)
      infoLogger = infos.append(_)
      reporter.reset()
      val parser = CompilerCompatibility.newUnitParser(compiler, line, fileName)

      val trees = CompilerCompatibility.trees(compiler)(parser)
      if (reporter.hasErrors) Left(errors.mkString(newLine))
      else Right(trees)
    }
  }
}
