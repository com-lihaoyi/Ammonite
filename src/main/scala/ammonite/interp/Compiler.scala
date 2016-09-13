package ammonite.runtime

import ammonite.util.{ImportData, Imports}
//import ammonite.util.Util.newLine

//import scala.collection.mutable
import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util._
import ammonite.kernel._

import scalaz._
import Scalaz._
import Validation.FlatMap._
import java.io.OutputStream

import ammonite.kernel.kernel._

import language.existentials

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
final class Compiler(classpath: Seq[java.io.File],
                     dynamicClasspath: VirtualDirectory,
                     evalClassloader: => ClassLoader,
                     pluginClassloader: => ClassLoader,
                     val settings: Settings) {

  import Compiler._

  private[this] var importsLen = 0
  private[this] var lastImports = Seq.empty[ImportData]

  private[this] val PluginXML = "scalac-plugin.xml"
  private[this] lazy val plugins0 = {
    import scala.collection.JavaConverters._
    val loader = pluginClassloader

    val urls = loader.getResources(PluginXML).asScala.toVector

    val plugins = for {
      url <- urls
      elem = scala.xml.XML.load(url.openStream())
      name = (elem \\ "plugin" \ "name").text
      className = (elem \\ "plugin" \ "classname").text
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

  private[this] val (vd, jcp) = initGlobalBits(
    classpath,
    dynamicClasspath,
    settings
  )

  private[this] val compiler = {
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
    scalac
  }

  /**
    * Compiles a blob of bytes and spits of a list of classfiles
    * importsLen0 is the length of topWrapper appended above code by wrappedCode function
    * It is passed to AmmonitePlugin to decrease this much offset from each AST node
    * corresponding to the actual code so as to correct the line numbers in error report
    */
  def compile(src: Array[Byte], importsLen0: Int, fileName: String): CompilerOutput = {

    this.importsLen = importsLen0
    val singleFile = makeFile(src, fileName)

    val reporter = new StoreReporter
    compiler.reporter = reporter

    val run = new compiler.Run()
    vd.clear()
    val compilationResult = Validation.fromTryCatchNonFatal(run.compileFiles(List(singleFile)))
    compiler.reporter = null

    val compilationResultMapped = compilationResult leftMap (LogMessage.fromThrowable(_))

    compilationResultMapped.toValidationNel flatMap { _ =>
      val outputFiles = enumerateVdFiles(vd).toVector

      val (errorMessages, otherMessages) = reporter.infos.foldLeft((List[LogError](), List[LogMessage]())) {
        case ((error, other), reporter.Info(pos, msg, reporter.ERROR)) =>
          (LogError(Position.formatMessage(pos, msg, false)) :: error, other)
        case ((error, other), reporter.Info(pos, msg, reporter.WARNING)) =>
          (error, LogWarning(Position.formatMessage(pos, msg, false)) :: other)
        case ((error, other), reporter.Info(pos, msg, reporter.INFO)) =>
          (error, LogInfo(Position.formatMessage(pos, msg, false)) :: other)
      }

      (errorMessages) match {
        case h :: t =>
          val errorNel = NonEmptyList(h, t: _*)
          Failure(errorNel)
        case Nil =>
          //shutdownPressy()
          val files = for (x <- outputFiles if x.name.endsWith(".class")) yield {
            val segments = x.path.split("/").toList.tail
            val output = writeDeep(dynamicClasspath, segments, "")
            output.write(x.toByteArray)
            output.close()
            (x.path.stripPrefix("(memory)/").stripSuffix(".class").replace('/', '.'), x.toByteArray)
          }

          val imports = lastImports.toList
          Success((otherMessages, files, Imports(imports)))
      }
    }
  }

  def parse(line: String): ValidationNel[LogError, Seq[Global#Tree]] = {
    val reporter = new StoreReporter
    compiler.reporter = reporter
    val parser = compiler.newUnitParser(line)
    val trees = CompilerCompatibility.trees(compiler)(parser)
    compiler.reporter = null
    val errors: List[LogError] = reporter.infos.toList collect {
      case reporter.Info(pos, msg, reporter.ERROR) => LogError(Position.formatMessage(pos, msg, false))
    }
    (errors) match {
      case h :: t =>
        val errorNel = NonEmptyList(h, t: _*)
        Failure(errorNel)
      case Nil =>
        Success(trees)
    }
  }

}

object Compiler {

  private def writeDeep(d: VirtualDirectory, path: List[String], suffix: String): OutputStream = (path: @unchecked) match {
    case head :: Nil => d.fileNamed(path.head + suffix).output
    case head :: rest =>
      writeDeep(
        d.subdirectoryNamed(head).asInstanceOf[VirtualDirectory],
        rest,
        suffix
      )
  }


  private def enumerateVdFiles(d: VirtualDirectory): Iterator[AbstractFile] = {
    val (subs, files) = d.iterator.partition(_.isDirectory)
    files ++ subs.map(_.asInstanceOf[VirtualDirectory]).flatMap(enumerateVdFiles)
  }

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
  def initGlobalBits(classpath: Seq[java.io.File], dynamicClasspath: VirtualDirectory, settings: Settings) = {
    val vd = new io.VirtualDirectory("(memory)", None)
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

}
