package ammonite.interp

// package ammonite.runtime

// //import java.io.File

// //import scala.collection.mutable
// import scala.tools.nsc.Settings
// //import ammonite.ops._
// //import fastparse.all._

// //import annotation.tailrec
// //import ammonite.util.ImportTree
// //import ammonite.util.Util.{CacheDetails, newLine, normalizeNewlines}
// import ammonite.util._

// import scala.reflect.io.VirtualDirectory

// import ammonite.kernel.kernel._
// import ammonite.kernel._
// import scalaz.{Name => _, _}
// import Validation.FlatMap._
// import java.io.OutputStream

// /**
//   * A convenient bundle of all the functionality necessary
//   * to interpret Scala code. Doesn't attempt to provide any
//   * real encapsulation for now.
//   */
// class Interpreter() { interp =>

//   // private val dynamicClasspath = new VirtualDirectory("(memory)", None)
//   // var compiler: Compiler = null
//   // var pressy: Pressy = _

//   // //private def evalClassloader = eval.frame.classloader

//   // // def reInit() = {
//   // //   if (compiler != null)
//   // //     init()
//   // // }

//   // def init() = {
//   //   // Note we not only make a copy of `settings` to pass to the compiler,
//   //   // we also make a *separate* copy to pass to the presentation compiler.
//   //   // Otherwise activating autocomplete makes the presentation compiler mangle
//   //   // the shared settings and makes the main compiler sad
//   //   val settings =
//   //     Option(compiler).fold(new Settings)(_.settings.copy)
//   //   compiler = new Compiler(
//   //     Classpath.classpath,
//   //     dynamicClasspath,
//   //     frame.classloader,
//   //     frame.pluginClassloader,
//   //     settings
//   //   )
//   //   pressy = Pressy(
//   //     Classpath.classpath,
//   //     dynamicClasspath,
//   //     frame.classloader,
//   //     settings.copy()
//   //   )
//   // }

//   // private val importHooks = Ref(
//   //   Map[Seq[String], ImportHook](
//   //     Seq("file") -> ImportHook.File,
//   //     Seq("exec") -> ImportHook.Exec,
//   //     Seq("url") -> ImportHook.Http,
//   //     Seq("ivy") -> ImportHook.Ivy,
//   //     Seq("cp") -> ImportHook.Classpath,
//   //     Seq("plugin", "ivy") -> ImportHook.PluginIvy,
//   //     Seq("plugin", "cp") -> ImportHook.PluginClasspath
//   //   ))

//   //reInit()

//   //init()

//   // def loadIvy(coordinates: (String, String, String), verbose: Boolean = true) = {
//   //   val (groupId, artifactId, version) = coordinates
//   //   val cacheKey =
//   //     (interpApi.resolvers().hashCode.toString, groupId, artifactId, version)

//   //   val fetched =
//   //     storage.ivyCache().get(cacheKey)

//   //   val psOpt =
//   //     fetched.map(_.map(new java.io.File(_))).filter(_.forall(_.exists()))

//   //   psOpt match {
//   //     case Some(ps) => ps
//   //     case None =>
//   //       val resolved = ammonite.runtime.tools
//   //         .IvyThing(
//   //           () => interpApi.resolvers(),
//   //           printer,
//   //           verboseOutput
//   //         )
//   //         .resolveArtifact(
//   //           groupId,
//   //           artifactId,
//   //           version,
//   //           if (verbose) 2 else 1
//   //         )
//   //         .toSet

//   //       // #433 only non-snapshot versions are cached
//   //       if (!version.endsWith("SNAPSHOT")) {
//   //         storage.ivyCache() = storage
//   //           .ivyCache()
//   //           .updated(
//   //             cacheKey,
//   //             resolved.map(_.getAbsolutePath)
//   //           )
//   //       }

//   //       resolved
//   //   }
//   // }
//   // abstract class DefaultLoadJar extends LoadJar {

//   //   lazy val ivyThing = ammonite.runtime.tools.IvyThing(
//   //     () => interpApi.resolvers(),
//   //     printer,
//   //     verboseOutput
//   //   )

//   //   def handleClasspath(jar: File): Unit

//   //   def cp(jar: Path): Unit = {
//   //     handleClasspath(new java.io.File(jar.toString))
//   //     reInit()
//   //   }
//   //   def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit = {
//   //     val resolved = loadIvy(coordinates, verbose)
//   //     resolved.foreach(handleClasspath)
//   //     reInit()
//   //   }
//   // }

//   // private def handleEvalClasspath(jar: File) = {
//   //   eval.frames.head.addClasspath(Seq(jar))
//   //   evalClassloader.add(jar.toURI.toURL)
//   // }

//   // private def handlePluginClasspath(jar: File) = {
//   //   eval.frames.head.pluginClassloader.add(jar.toURI.toURL)
//   // }

//   // lazy val interpApi: InterpAPI = new InterpAPI { outer =>
//   //   lazy val resolvers =
//   //     Ref(ammonite.runtime.tools.Resolvers.defaultResolvers)

//   //   object load extends DefaultLoadJar with Load {

//   //     def handleClasspath(jar: File) = handleEvalClasspath(jar)

//   //     def apply(line: String) = processExec(line) match {
//   //       case Res.Failure(ex, s) => throw new CompilationError(s)
//   //       case Res.Exception(t, s) => throw t
//   //       case _ =>
//   //     }

//   //     def exec(file: Path): Unit = apply(normalizeNewlines(read(file)))

//   //     def module(file: Path) = {
//   //       val (pkg, wrapper) = Util.pathToPackageWrapper(file, wd)
//   //       processModule(
//   //         ImportHook.Source.File(wd / "Main.sc"),
//   //         normalizeNewlines(read(file)),
//   //         wrapper,
//   //         pkg,
//   //         true,
//   //         ""
//   //       ) match {
//   //         case Res.Failure(ex, s) => throw new CompilationError(s)
//   //         case Res.Exception(t, s) => throw t
//   //         case x => //println(x)
//   //       }
//   //       reInit()
//   //     }

//   //     object plugin extends DefaultLoadJar {
//   //       def handleClasspath(jar: File) = handlePluginClasspath(jar)
//   //     }

//   //   }
//   // }

// }

// object Interpreter {
//   // val SheBang = "#!"

//   // private def evaluationResult(wrapperName: Seq[Name], imports: Imports, tag: String, value: Any) = {
//   //   Evaluated(
//   //     wrapperName,
//   //     Imports(
//   //       for (id <- imports.value) yield {
//   //         val filledPrefix =
//   //           if (id.prefix.isEmpty) {
//   //             // For some reason, for things not-in-packages you can't access
//   //             // them off of `_root_`
//   //             wrapperName
//   //           } else {
//   //             id.prefix
//   //           }
//   //         val rootedPrefix: Seq[Name] =
//   //           if (filledPrefix.headOption.exists(_.backticked == "_root_"))
//   //             filledPrefix
//   //           else Seq(Name("_root_")) ++ filledPrefix

//   //         id.copy(prefix = rootedPrefix)
//   //       }
//   //     ),
//   //     value
//   //   )
//   // }
//   // /**
//   //   * This gives our cache tags for compile caching. The cache tags are a hash
//   //   * of classpath, previous commands (in-same-script), and the block-code.
//   //   * Previous commands are hashed in the wrapper names, which are contained
//   //   * in imports, so we don't need to pass them explicitly.
//   //   */
//   // private def cacheTag(code: String, imports: Seq[ImportData], classpathHash: Array[Byte]): String = {
//   //   val bytes = Util.md5Hash(
//   //     Iterator(
//   //       Util.md5Hash(Iterator(code.getBytes)),
//   //       Util.md5Hash(imports.iterator.map(_.toString.getBytes)),
//   //       classpathHash
//   //     ))
//   //   bytes.map("%02x".format(_)).mkString
//   // }

//   // private def skipSheBangLine(code: String) = {
//   //   if (code.startsWith(SheBang))
//   //     code.substring(code.indexOf(newLine))
//   //   else
//   //     code
//   // }

//   // private type EvaluateCallback = (Preprocessor.Output, Int, Name) => Res[Evaluated]

//   // private type CacheData = (Imports, Seq[CacheDetails])

//   // type ProcessedData = (Imports, Seq[CacheDetails], Seq[ImportTree])

//   // private def indexWrapperName(wrapperName: Name, wrapperIndex: Int): Name = {
//   //   Name(wrapperName.raw + (if (wrapperIndex == 1) "" else "_" + wrapperIndex))
//   // }

//   def writeDeep(d: VirtualDirectory, path: List[String], suffix: String): OutputStream = (path: @unchecked) match {
//     case head :: Nil => d.fileNamed(path.head + suffix).output
//     case head :: rest =>
//       writeDeep(
//         d.subdirectoryNamed(head).asInstanceOf[VirtualDirectory],
//         rest,
//         suffix
//       )
//   }

//   /**
//     * Writes files to dynamicClasspath. Needed for loading cached classes.
//     */
//   def addToClasspath(classFiles: Traversable[(String, Array[Byte])], dynamicClasspath: VirtualDirectory): Unit = {
//     for ((name, bytes) <- classFiles) {
//       val output =
//         writeDeep(dynamicClasspath, name.split('.').toList, ".class")
//       output.write(bytes)
//       output.close()
//     }
//   }

// }
