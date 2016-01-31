package ammonite.repl
package interp


import acyclic.file
import ammonite.repl.ImportData
import ammonite.repl.Util.ClassFiles
import scala.collection.mutable
import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.plugins.Plugin

import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util._


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
              printer: Printer): Compiler.Output

  def search(name: scala.reflect.runtime.universe.Type): Option[String]
  /**
   * Either the statements that were parsed or the error message
   */
  def parse(line: String): Either[String, Seq[Global#Tree]]

  /**
   * Writes files to dynamicClasspath. Needed for loading cached classes.
   */ 
  def addToClasspath(classFiles: ClassFiles): Unit
}
object Compiler{
  /**
   * If the Option is None, it means compilation failed
   * Otherwise it's a Traversable of (filename, bytes) tuples
   */
  type Output = Option[(Traversable[(String, Array[Byte])], Seq[ImportData])]

  /**
   * Converts a bunch of bytes into Scalac's weird VirtualFile class
   */
  def makeFile(src: Array[Byte], name: String = "Main.scala") = {
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
                     errorLogger: => String => Unit,
                     warningLogger: => String => Unit) = {
    val vd = new io.VirtualDirectory("(memory)", None)
    lazy val settings = new Settings
    val settingsX = settings
    val jCtx = new JavaContext()
    val (dirDeps, jarDeps) = classpath.partition(_.isDirectory)

    val jarCP =
      jarDeps.filter(x => x.getName.endsWith(".jar") || Classpath.canBeOpenedAsJar(x))
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

    val reporter = new AbstractReporter {
      def displayPrompt(): Unit = ???

      def display(pos: Position, msg: String, severity: Severity) = {
        severity match{
          case ERROR =>
            Classpath.traceClasspathProblem(s"ERROR: $msg")
            errorLogger(Position.formatMessage(pos, msg, false))
          case WARNING =>
            warningLogger(Position.formatMessage(pos, msg, false))
        }
      }

      val settings = settingsX
    }
    (settings, reporter, vd, jcp)
  }

  def apply(classpath: Seq[java.io.File],
            dynamicClasspath: VirtualDirectory,
            evalClassloader: => ClassLoader,
            pluginClassloader: => ClassLoader,
            shutdownPressy: () => Unit): Compiler = new Compiler{

    val PluginXML = "scalac-plugin.xml"

    lazy val plugins0 = {
      import scala.collection.JavaConverters._
      val loader = pluginClassloader

      val urls = loader
        .getResources(PluginXML)
        .asScala
        .toVector

      val plugins = for {
        url <- urls
        elem = scala.xml.XML.load(url.openStream())
        name = (elem \\ "plugin" \ "name").text
        className = (elem \\ "plugin" \ "classname").text
        // acyclic seems to conflict with AmmonitePlugin (happens during the
        // tests in particular), so it's filtered out here. Else it raises:
        //
        //   scala.reflect.internal.FatalError: Multiple phases want
        //   to run right after typer; followers:
        //   AmmonitePhase,acyclic; created phase-order.dot

        if name != "acyclic"
        if name.nonEmpty && className.nonEmpty
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

    var lastImports = Seq.empty[ImportData]

    val (vd, reporter, compiler) = {
      val (settings, reporter, vd, jcp) = initGlobalBits(
        classpath, dynamicClasspath, errorLogger, warningLogger
      )
      val scalac = new nsc.Global(settings, reporter) { g =>
        override lazy val plugins = List(new AmmonitePlugin(g, lastImports = _)) ++ {
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

        // Actually jcp, avoiding a path-dependent type issue in 2.10 here
        override def classPath = platform.classPath
        override lazy val platform: ThisPlatform = new JavaPlatform{
          val global: g.type = g
          override def classPath = jcp
        }
        override lazy val analyzer = CompilerCompatibility.analyzer(g, evalClassloader)
      }
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
          m <- util.Try(sym.typeSignature.members).toOption.toSeq.flatten
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
     */
    def compile(src: Array[Byte],
                printer: Printer): Output = {
      compiler.reporter.reset()
      this.errorLogger = printer.error
      this.warningLogger = printer.warning
      val singleFile = makeFile( src)

      val run = new compiler.Run()
      vd.clear()
      run.compileFiles(List(singleFile))
      if (reporter.hasErrors) None
      else Some{
        shutdownPressy()

        val files = for{
          x <- vd.iterator.to[collection.immutable.Traversable]
          if x.name.endsWith(".class")
        } yield {
          val output = dynamicClasspath.fileNamed(x.name).output
          output.write(x.toByteArray)
          output.close()
          (x.name.stripSuffix(".class"), x.toByteArray)
        }
        val imports = lastImports.toList
        (files, imports)
      }
    }

    def addToClasspath(classFiles: Traversable[(String, Array[Byte])]): Unit = {
      for((name, bytes) <- classFiles){
        val output = dynamicClasspath.fileNamed(s"$name.class").output
        output.write(bytes)
        output.close()
      }
    }

    def parse(line: String): Either[String, Seq[Global#Tree]]= {
      val errors = mutable.Buffer.empty[String]
      val warnings = mutable.Buffer.empty[String]
      errorLogger = errors.append(_)
      warningLogger = warnings.append(_)
      reporter.reset()
      val parser = compiler.newUnitParser(line)
      val trees = CompilerCompatibility.trees(compiler)(parser)
      if (reporter.hasErrors) Left(errors.mkString("\n"))
      else Right(trees)
    }
  }
}
