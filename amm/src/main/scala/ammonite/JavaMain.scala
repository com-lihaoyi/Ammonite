package ammonite

import java.io.{InputStream, OutputStream, PrintStream}

import ammonite.interp.{CodeWrapper, Watchable}
import ammonite.main.{Cli, Defaults}
import ammonite.runtime.{ImportHook, Storage}
import ammonite.util.{Bind, Colors, Res}
import coursierapi.Dependency

import collection.JavaConverters._
import scala.reflect.api
import scala.reflect.api.{TypeCreator, Universe}
class JavaBind[T](val name: String, val value: T) {

  import scala.reflect.runtime.universe.TypeTag
  import scala.reflect.runtime.universe.runtimeMirror

  implicit val typeTag: TypeTag[T] = {
    val cls = value.getClass
    val mirror = runtimeMirror(cls.getClassLoader)
    val tpe = mirror.classSymbol(cls).toType
    val typeCreator = new TypeCreator {
      override def apply[U <: Universe with Singleton](m: api.Mirror[U]): U#Type = {
        if (m != mirror) throw new RuntimeException("wrong mirror") else tpe.asInstanceOf[U#Type]
      }
    }

    TypeTag[T](mirror, typeCreator)
  }

  def toBind(): Bind[T] = new Bind(name, value)
}

class JavaMain(val main: Main) { javaMain =>
  val in: InputStream = main.inputStream
  val out: OutputStream = main.outputStream
  val err: OutputStream = main.errorStream
  val printError = new PrintStream(err)
  val printOut = new PrintStream(out)

  def run(replArgs: Array[JavaBind[_]]): (Res[Any], Seq[(Watchable, Long)]) = {
    main.run(replArgs.map(_.toBind()): _*)
  }

  def runWithWatch(replArgs: Array[JavaBind[_]]): Unit = {
    val runner = new JavaMainRunner(replArgs.map(_.toBind()))
    runner.runRepl()
  }

  class JavaMainRunner(binds: Seq[Bind[_]]) extends
    MainRunner(Cli.Config(), printOut, printError, in, out, err) {
    override def runRepl(): Unit = {
      watchLoop(isRepl = true, printing = false, _.run(binds: _*))
    }

    override def initMain(isRepl: Boolean): Main = {
      javaMain.main
    }
  }
}

object JavaMain {
  // a builder to main
  class Builder {
    private var predefCode: String = ""
    private var predefFile: Option[os.Path] = None
    private var defaultPredef: Boolean = true
    private var storageBackend: Storage = new Storage.Folder(Defaults.ammoniteHome)
    private var wd: os.Path = os.pwd
    private var welcomeBanner: Option[String] = Some(Defaults.welcomeBanner)
    private var inputStream: InputStream = System.in
    private var outputStream: OutputStream = System.out
    private var errorStream: OutputStream = System.err
    private var verboseOutput: Boolean = true
    private var remoteLogging: Boolean = true
    private var colors: Colors = Colors.Default
    private var replCodeWrapper: CodeWrapper = CodeWrapper
    private var scriptCodeWrapper: CodeWrapper = CodeWrapper
    private var alreadyLoadedDependencies: Seq[Dependency] = Defaults.alreadyLoadedDependencies()
    private var importHooks: Map[Seq[String], ImportHook] = ImportHook.defaults
    private var classPathWhitelist: Set[Seq[String]] = Set.empty

    def setPredefCode(code: String): Builder ={
      this.predefCode = code
      this
    }

    def setPredefFile(file: String): Builder = {
      predefFile = Some(os.Path(file))
      this
    }

    def setDefaultPredef(default: Boolean): Builder = {
      defaultPredef = default
      this
    }

    def setStorageBackend(storage: Storage): Builder = {
      storageBackend = storage
      this
    }

    def setWorkDir(wd: String): Builder = {
      this.wd = os.Path(wd)
      this
    }

    def setWelcomeBanner(banner: String): Builder = {
      this.welcomeBanner = Some(banner)
      this
    }

    def setInputStream(inputStream: InputStream): Builder = {
      this.inputStream = inputStream
      this
    }

    def setOutputStream(outputStream: OutputStream): Builder = {
      this.outputStream = outputStream;
      this
    }

    def setErrorStream(errorStream: OutputStream): Builder = {
      this.errorStream = errorStream;
      this
    }

    def setVerboseOutput(verboseOutput: Boolean):  Builder = {
      this.verboseOutput = verboseOutput;
      this
    }

    def setRemoteLogging(remoteLogging: Boolean): Builder = {
      this.remoteLogging = remoteLogging
      this
    }

    def setColors(colors: Colors): Builder = {
      this.colors = colors
      this
    }

    def setReplCodeWrapper(codeWrapper: CodeWrapper): Builder = {
      this.replCodeWrapper = codeWrapper
      this
    }

    def setScriptCodeWrapper(codeWrapper: CodeWrapper): Builder = {
      this.scriptCodeWrapper = scriptCodeWrapper
      this
    }

    def setAlreadyLoadedDependencies(dependency: java.util.List[Dependency]): Builder = {
      this.alreadyLoadedDependencies = dependency.asScala.toSeq
      this
    }

    def setImportHooks(hooks: java.util.Map[java.util.List[String], ImportHook]): Builder = {
      this.importHooks = hooks.asScala.map { case (values, hook) =>
        values.asScala.toSeq -> hook
      }.toMap
      this
    }

    def setClassPathWhitelist(classPaths: java.util.Set[java.util.List[String]]): Builder = {
      this.classPathWhitelist = classPaths.asScala.map(_.asScala.toSeq).toSet
      this
    }

    def build(): JavaMain = {
      val main = new Main(predefCode, predefFile, defaultPredef, storageBackend, wd, welcomeBanner,
        inputStream, outputStream,
        errorStream, verboseOutput, remoteLogging, colors, replCodeWrapper, scriptCodeWrapper,
        alreadyLoadedDependencies, importHooks, classPathWhitelist)

      new JavaMain(main)
    }
  }
}
