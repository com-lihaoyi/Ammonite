package ammonite.runtime

import ammonite.util.{ImportData, Imports, Printer}
import ammonite.util.Util.newLine

import scala.collection.mutable
import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.{AbstractReporter, StoreReporter}
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util._
import scala.util.Try
import com.typesafe.scalalogging.LazyLogging

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
trait Compiler {
  def compiler: nsc.Global
  def compile(src: Array[Byte], printer: Printer, importsLen0: Int, fileName: String): Compiler.Output

  def search(name: scala.reflect.runtime.universe.Type): Option[String]

  /**
    * Either the statements that were parsed or the error message
    */
  def parse(line: String): Either[String, Seq[Global#Tree]]
  var importsLen = 0

}
object Compiler {

  /**
    * If the Option is None, it means compilation failed
    * Otherwise it's a Traversable of (filename, bytes) tuples
    */
  type Output = Option[(Vector[(String, Array[Byte])], Imports)]

  /**
    * Converts a bunch of bytes into Scalac's weird VirtualFile class
    */
  def makeFile(src: Array[Byte], name: String = "Main.sc") = {
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

  /**
    * Code to initialize random bits and pieces that are needed
    * for the Scala compiler to function, common between the
    * normal and presentation compiler
    */
  def initGlobalBits(classpath: Seq[java.io.File],
                     dynamicClasspath: VirtualDirectory,
                     settings: Settings) = {
    val vd = new io.VirtualDirectory("(memory)", None)
    val settingsX = settings
    val jCtx = new JavaContext()
    val (dirDeps, jarDeps) = classpath.partition(_.isDirectory)

    val jarCP =
      jarDeps
        .filter(x => x.getName.endsWith(".jar") || Classpath.canBeOpenedAsJar(x))
        .map(x => new DirectoryClassPath(new FileZipArchive(x), jCtx))
        .toVector

    val dirCP =
      dirDeps.map(x => new DirectoryClassPath(new PlainDirectory(new Directory(x)), jCtx))

    val dynamicCP = Seq(new DirectoryClassPath(dynamicClasspath, jCtx))
    val jcp = new JavaClassPath(jarCP ++ dirCP ++ dynamicCP, jCtx)

    if (Classpath.traceClasspathIssues) {
      settings.Ylogcp.value = true
      println("jardeps")
      jarDeps.foreach(p => println(s"${p.getName.takeRight(4)} $p"))
      println("finished")
    }

    settings.outputDirs.setSingleOutput(vd)

    settings.nowarnings.value = true
    (vd, jcp)
  }

  def apply(classpath: Seq[java.io.File],
            dynamicClasspath: VirtualDirectory,
            evalClassloader: => ClassLoader,
            pluginClassloader: => ClassLoader,
            shutdownPressy: () => Unit,
            settings: Settings): Compiler = new Compiler {

    val PluginXML = "scalac-plugin.xml"
    lazy val plugins0 = {
      import scala.collection.JavaConverters._
      val loader = pluginClassloader

      val urls = loader.getResources(PluginXML).asScala.toVector

      val plugins = for {
        url <- urls
        elem = scala.xml.XML.load(url.openStream())
        name = (elem \\ "plugin" \ "name").text
        className = (elem \\ "plugin" \ "classname").text
        // Hardcode exclusion of "com.lihaoyi" %% "acyclic", since that's
        // pretty useless and can cause problems conflicting with other plugins
        if className != "acyclic.plugin.RuntimePlugin"
        if name.nonEmpty && className.nonEmpty
        classOpt = try Some(loader.loadClass(className))
        catch { case _: ClassNotFoundException => None }
      } yield (name, className, classOpt)

      val notFound = plugins.collect {
        case (name, className, None) => (name, className)
      }
      if (notFound.nonEmpty) {
        for ((name, className) <- notFound.sortBy(_._1))
          Console.err.println(s"Implementation $className of plugin $name not found.")
      }

      plugins.collect { case (name, _, Some(cls)) => name -> cls }
    }

    var lastImports = Seq.empty[ImportData]

    val (vd, compiler) = {
      val (vd, jcp) = initGlobalBits(
        classpath,
        dynamicClasspath,
        settings
      )
      val scalac = new nsc.Global(settings) { g =>
        override lazy val plugins = List(new AmmonitePlugin(g, lastImports = _, importsLen)) ++ {
          for {
            (name, cls) <- plugins0
            plugin = Plugin.instantiate(cls, g)
            initOk = try CompilerCompatibility.pluginInit(plugin, Nil, g.globalError)
            catch {
              case ex: Exception =>
                Console.err.println(s"Warning: disabling plugin $name, initialization failed: $ex")
                false
            }
            if initOk
          } yield plugin
        }

        // Actually jcp, avoiding a path-dependent type issue in 2.10 here
        override def classPath = platform.classPath
        override lazy val platform: ThisPlatform = new JavaPlatform {
          val global: g.type = g
          override def classPath = jcp
        }
        override lazy val analyzer =
          CompilerCompatibility.analyzer(g, evalClassloader)
      }
      // Initialize scalac to the parser phase immediately, so we can start
      // using Compiler#parse even if we haven't compiled any compilation
      // units yet due to caching
      val run = new scalac.Run()
      scalac.phase = run.parserPhase
      run.cancel()
      (vd, scalac)
    }

    def search(target: scala.reflect.runtime.universe.Type) = {
      def resolve(path: String*): compiler.Symbol = {
        var curr = path.toList
        var start: compiler.Symbol = compiler.RootClass
        while (curr != Nil) {
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
      while (level > 0) {
        thingsInScope = for {
          (sym, path) <- thingsInScope
          // No clue why this one blows up
          m <- Try(sym.typeSignature.members).toOption.toSeq.flatten
        } yield (m, m.name :: path)
        thingsInScope.find(target.typeSymbol.fullName == _._1.fullName).foreach { path =>
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
    def compile(src: Array[Byte], printer: Printer, importsLen0: Int, fileName: String): Output = {
      def enumerateVdFiles(d: VirtualDirectory): Iterator[AbstractFile] = {
        val (subs, files) = d.iterator.partition(_.isDirectory)
        files ++ subs.map(_.asInstanceOf[VirtualDirectory]).flatMap(enumerateVdFiles)
      }

      //compiler.reporter.reset()
      //this.errorLogger = x => {printer.error(x); println(s"${"#" * 40} error: $x")}
      //this.warningLogger = x => {printer.warning(x); println(s"${"#" * 40} warning: $x")}
      //this.infoLogger = x => {printer.info(x); println(s"${"#" * 40} info: $x")}
      val singleFile = makeFile(src, fileName)
      this.importsLen = importsLen0

      val newReporter = new StoreReporter
      compiler.reporter = newReporter

      val run = new compiler.Run()
      vd.clear()

      run.compileFiles(List(singleFile))

      val outputFiles = enumerateVdFiles(vd).toVector

      if (newReporter.hasErrors) {
        for (info <- newReporter.infos) {
          import info._
          import newReporter._
          severity match {
            case ERROR =>
              Classpath.traceClasspathProblem(s"ERROR: $msg")
              printer.error(Position.formatMessage(pos, msg, false))
            case WARNING =>
              printer.warning(Position.formatMessage(pos, msg, false))
            case INFO =>
              printer.info(Position.formatMessage(pos, msg, false))
          }
        }
        compiler.reporter = null
        None
      } else {
        shutdownPressy()

        val files = for (x <- outputFiles if x.name.endsWith(".class")) yield {
          val segments = x.path.split("/").toList.tail
          val output = Evaluator.writeDeep(dynamicClasspath, segments, "")
          output.write(x.toByteArray)
          output.close()
          (x.path.stripPrefix("(memory)/").stripSuffix(".class").replace('/', '.'), x.toByteArray)
        }

        val imports = lastImports.toList

        Some((files, Imports(imports)))

      }
    }

    def parse(line: String): Either[String, Seq[Global#Tree]] = {
      val errors = mutable.Buffer.empty[String]
      // val warnings = mutable.Buffer.empty[String]
      // val infos = mutable.Buffer.empty[String]
      // errorLogger = errors.append(_)
      // warningLogger = warnings.append(_)
      // infoLogger = infos.append(_)
      // reporter.reset()
      val reporter = new StoreReporter
      compiler.reporter = reporter
      val parser = compiler.newUnitParser(line)
      val trees = CompilerCompatibility.trees(compiler)(parser)
      val res = if (reporter.hasErrors){
      for (info <- reporter.infos) {
          import info._
          import reporter._
          severity match {
            case ERROR =>
              Classpath.traceClasspathProblem(s"ERROR: $msg")
              errors.append(Position.formatMessage(pos, msg, false))
            case _ => ()
          }
        }
         Left(errors.mkString(newLine)) }else Right(trees)
      compiler.reporter = null
      res
    }
  }
}
