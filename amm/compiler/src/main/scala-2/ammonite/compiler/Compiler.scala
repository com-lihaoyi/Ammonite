package ammonite.compiler


import java.io.OutputStream
import java.nio.file.{Files, Path, Paths}

import ammonite.compiler.iface.{Compiler => ICompiler, Preprocessor}
import ammonite.util.{Classpath, ImportData, Imports, Printer}
import ammonite.util.Util.newLine

import scala.collection.mutable
import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.classpath.{
  AggregateClassPath,
  DirectoryClassPath,
  FileUtils,
  VirtualDirectoryClassPath
}
import scala.tools.nsc.{CustomZipAndJarFileLookupFactory, Global, Settings}
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.plugins.Plugin


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
trait Compiler extends ICompiler {
  def compiler: nsc.Global
  var importsLen = 0
  var userCodeNestingLevel = -1

}
object Compiler{
  /**
    * Writes files to dynamicClasspath. Needed for loading cached classes.
    */
  def addToClasspath(classFiles: Traversable[(String, Array[Byte])],
                     dynamicClasspath: VirtualDirectory,
                     outputDir: Option[Path]): Unit = {

    val outputDir0 = outputDir.map(os.Path(_, os.pwd))
    for((name, bytes) <- classFiles){
      val elems = name.split('/').toList
      val output = writeDeep(dynamicClasspath, elems)
      output.write(bytes)
      output.close()

      for (dir <- outputDir0)
        os.write.over(dir / elems, bytes, createFolders = true)
    }

  }

  def writeDeep(d: VirtualDirectory,
                path: List[String]): OutputStream = path match {
    case head :: Nil => d.fileNamed(head).output
    case head :: rest =>
      writeDeep(
        d.subdirectoryNamed(head).asInstanceOf[VirtualDirectory],
        rest
      )
    // We should never write to an empty path, and one of the above cases
    // should catch this and return before getting here
    case Nil => ???
  }

  /**
    * Converts a bunch of bytes into Scalac's weird VirtualFile class
    */
  def makeFile(src: Array[Byte], name: String) = {
    val segments = name.split("/", -1)

    val singleFile = new io.VirtualFile(segments.last, segments.mkString("/"))
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



  def apply(classpath: Seq[java.net.URL],
            dynamicClasspath: VirtualDirectory,
            outputDir: Option[Path],
            evalClassloader: => ClassLoader,
            pluginClassloader: => ClassLoader,
            shutdownPressy: () => Unit,
            reporterOpt: Option[MakeReporter.Reporter],
            settings: Settings,
            classPathWhitelist: Set[Seq[String]],
            initialClassPath: Seq[java.net.URL],
            lineNumberModifier: Boolean = true): Compiler = new Compiler{

    def preprocessor(fileName: String, markGeneratedSections: Boolean): Preprocessor =
      new DefaultPreprocessor(parse(fileName, _), markGeneratedSections)

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

      val (dirDeps, jarDeps) = classpath.partition { u =>
        u.getProtocol == "file" &&
        Files.isDirectory(Paths.get(u.toURI))
      }

      val jcp = initGlobalClasspath(
        dirDeps,
        jarDeps,
        dynamicClasspath,
        settings,
        classPathWhitelist,
        initialClassPath
      )
      val vd = new io.VirtualDirectory("(memory)", None)
      if (Classpath.traceClasspathIssues) {
        settings.Ylogcp.value = true
        println("jardeps")
        jarDeps.foreach(p => println(s"${p.getPath.takeRight(4)} $p"))
        println("finished")
      }

      settings.outputDirs.setSingleOutput(vd)

      // Otherwise the presence of `src`'s source files mixed with
      // classfiles causes scalac to get confused
      settings.termConflict.value = "object"

      val reporter = reporterOpt.getOrElse {
        import scala.reflect.internal.util.Position
        MakeReporter.makeReporter(
          (pos, msg) => errorLogger(Position.formatMessage(pos, msg, false)),
          (pos, msg) => warningLogger(Position.formatMessage(pos, msg, false)),
          (pos, msg) => infoLogger(Position.formatMessage(pos, msg, false)),
          settings
        )
      }

      val scalac = CompilerCompatibility.initGlobal(
        settings, reporter, jcp,
        evalClassloader,
        createPlugins = g => {
          List(
            new ammonite.compiler.AmmonitePlugin(
              g,
              lastImports = _,
              uses => usedEarlierDefinitions = Some(uses),
              userCodeNestingLevel,
              importsLen,
              lineNumberModifier
            )
          ) ++ {
            val pluginSettings = settings.pluginOptions.value
              .map(s => s.split(":", 2))
              .collect {
                case Array(k, v) => k -> v
              }
              .groupBy(_._1)
              .map { case (k, l) => k -> l.map(_._2) }

            for {
              (name, cls) <- plugins0
              plugin = Plugin.instantiate(cls, g)
              initOk =
              try plugin.init(pluginSettings.getOrElse(name, Nil), g.globalError)
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
                fileName: String): Option[ICompiler.Output] = {

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
          val output = writeDeep(dynamicClasspath, segments)
          output.write(x.toByteArray)
          output.close()
          (x.path.stripPrefix("(memory)/"), x.toByteArray)
        }

        for {
          dir <- outputDir.map(os.Path(_, os.pwd))
          f <- outputFiles
        } {
          val segments = f.path.split("/").toList.tail
          os.write.over(dir / segments, f.toByteArray, createFolders = true)
        }

        val imports = lastImports.toList
        Some(ICompiler.Output(files, Imports(imports), usedEarlierDefinitions))

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
      val parser = compiler.newUnitParser(line, fileName)

      val trees = parser.parseStatsOrPackages()
      if (reporter.hasErrors) Left(errors.mkString(newLine))
      else Right(trees)
    }
  }


  def prepareJarCp(jarDeps: Seq[java.net.URL], settings: Settings) = {
    jarDeps.filter(x => x.getPath.endsWith(".jar") || Classpath.canBeOpenedAsJar(x))
      .flatMap { x =>
        if (x.getProtocol == "file") {
          val path = Paths.get(x.toURI)
          if (Files.exists(path)) {
            val arc = new FileZipArchive(path.toFile)
            Seq(CompilerCompatibility.createZipJarFactory(arc, settings))
          }
          else
            Nil
        } else {
          val arc = new internal.CustomURLZipArchive(x)
          Seq(CustomZipAndJarFileLookupFactory.create(arc, settings))
        }
      }
      .toVector
  }
  def prepareDirCp(dirDeps: Seq[java.net.URL]) = {
    dirDeps.flatMap { x =>
      val path = Paths.get(x.toURI)
      if (Files.exists(path))
        Seq(new DirectoryClassPath(path.toFile))
      else
        Nil
    }
  }
  /**
    * Code to initialize random bits and pieces that are needed
    * for the Scala compiler to function, common between the
    * normal and presentation compiler
    */
  def initGlobalClasspath(dirDeps: Seq[java.net.URL],
                          jarDeps: Seq[java.net.URL],
                          dynamicClasspath: VirtualDirectory,
                          settings: Settings,
                          classPathWhitelist: Set[Seq[String]],
                          initialClassPath: Seq[java.net.URL]) = {

    val (initialDirDeps, newDirDeps) = dirDeps.partition(initialClassPath.contains)
    val (initialJarDeps, newJarDeps) = jarDeps.partition(initialClassPath.contains)
    val newJarCp = prepareJarCp(newJarDeps, settings)
    val initialJarCp = prepareJarCp(initialJarDeps, settings)

    val newDirCp = prepareDirCp(newDirDeps)
    val initialDirCp = prepareDirCp(initialDirDeps)
    val dynamicCP = new VirtualDirectoryClassPath(dynamicClasspath){

      override def getSubDir(packageDirName: String): Option[AbstractFile] = {
        val pathParts = packageDirName.split('/')
        var file: AbstractFile = dir
        for (dirPart <- pathParts) {
          file = file.lookupName(dirPart, directory = true)
          if (file == null) return None
        }
        Some(file)

      }
      override def findClassFile(className: String): Option[AbstractFile] = {
        val relativePath = FileUtils.dirPath(className)
        val pathParts = relativePath.split('/')
        var file: AbstractFile = dir
        for (dirPart <- pathParts.init) {
          file = file.lookupName(dirPart, directory = true)
          if (file == null) return None
        }

        file.lookupName(pathParts.last + ".class", directory = false) match {
          case null => None
          case file => Some(file)
        }
      }

    }

    val staticCP = new scala.tools.nsc.WhiteListClasspath(
      initialJarCp ++ initialDirCp,
      classPathWhitelist
    )
    val jcp = new AggregateClassPath(Seq(staticCP, dynamicCP) ++ newJarCp ++ newDirCp)


    jcp
  }
}
