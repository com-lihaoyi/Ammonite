package ammonite.repl

import java.io.{File, InputStream, OutputStream}

import ammonite.ops._
import ammonite.repl.interp.ImportHook
import fastparse.Utils.literalize

import ammonite.repl.main.Router
import ammonite.repl.main.Router.{ArgSig, EntryPoint}
import ammonite.repl.util.Parsers.backtickWrap
import ammonite.repl.util._



/**
  * Contains the various entry points to the Ammonite REPL.
  *
  * Configuration of the basic REPL is done by passing in arguments when
  * constructing the [[Main]] instance, and the various entrypoints such
  * as [[run]] [[runScript]] and so on are methods on that instance.
  *
  * It is more or less equivalent to the [[Repl]] object itself, and has
  * a similar set of parameters, but does not have any of the [[Repl]]'s
  * implementation-related code and provides a more convenient set of
  * entry-points that a user can call.
  *
  * Note that the [[instantiateRepl]] function generates a new [[Repl]]
  * every time it is called!
  *
  * @param predef Any additional code you want to run before the REPL session
  *               starts. Can contain multiple blocks separated by `@`s
  * @param defaultPredef Do you want to include the "standard" predef imports
  *                      provided by Ammonite? These include tools like `time`,
  *                      `grep`, the `|` or `|?` pipes from ammonite-ops, and
  *                      other helpers. Can be disabled to give a clean
  *                      namespace for you to fill using your own predef.
  * @param storageBackend Where will all of Ammonite's persistent data get
  *                       stored? Things like any `predef.scala` file,
  *                       compilation/ivy caches, etc.. Defaults include
  *                       [[Storage.Folder]] and [[Storage.InMemory]], though
  *                       you can create your own.
  * @param wd The working directory of the REPL; when it load scripts, where
  *           the scripts will be considered relative to when assigning them
  *           packages
  */
case class Main(predef: String = "",
                defaultPredef: Boolean = true,
                storageBackend: Storage = new Storage.Folder(Main.defaultAmmoniteHome),
                wd: Path = ammonite.ops.cwd,
                welcomeBanner: Option[String] = Some(Main.defaultWelcomeBanner),
                inputStream: InputStream = System.in,
                outputStream: OutputStream = System.out,
                errorStream: OutputStream = System.err){
  /**
    * Instantiates an ammonite.repl.Repl using the configuration
    */
  def instantiateRepl(replArgs: Seq[Bind[_]] = Nil) = {
    val augmentedPredef = Main.maybeDefaultPredef(defaultPredef, Main.defaultPredefString)
    new Repl(
      inputStream, outputStream, errorStream,
      storage = storageBackend,
      predef = augmentedPredef + "\n" + predef,
      wd = wd,
      welcomeBanner = welcomeBanner,
      replArgs = replArgs
    )
  }
  def run(replArgs: Bind[_]*) = {
    Timer("Repl.run Start")
    val res = instantiateRepl(replArgs).run()
    Timer("Repl.run End")
    res
  }

  /**
    * Run a Scala script file! takes the path to the file as well as an array
    * of `args` and a map of keyword `kwargs` to pass to that file.
    */
  def runScript(path: Path,
                mainMethodName: Option[String],
                args: Seq[String],
                kwargs: Seq[(String, String)]): Res[Imports] = {

    val repl = instantiateRepl()
    val (pkg, wrapper) = Util.pathToPackageWrapper(path, wd)
    for{
      imports <- repl.interp.processModule(
        ImportHook.Source.File(path),
        read(path),
        wrapper,
        pkg,
        autoImport = true
      )
      _ <- {
        repl.interp.reInit()

        val fullName = (pkg :+ wrapper).map(_.backticked).mkString(".")
        repl.interp.processModule(
          ImportHook.Source.File(cwd/"<console>"),
          s"val routes = ammonite.repl.main.Router.generateRoutes[$fullName.type]($fullName)",
          Name("MainRouter"),
          Seq(Name("$sess")),
          autoImport = false
        )
      }
      entryPoints =
        repl.interp
            .eval
            .sess
            .frames
            .head
            .classloader
            .loadClass("$sess.MainRouter")
            .getMethods
            .find(_.getName == "routes")
            .get
            .invoke(null)
            .asInstanceOf[Seq[Router.EntryPoint]]
            .filter(_.name != "$main")
      res <- mainMethodName match {
        case None =>
          entryPoints.find(_.name == "main") match {
            case None => Res.Success(imports)
            case Some(entry) =>
              Main.runEntryPoint(entry, args, kwargs).getOrElse(Res.Success(imports))
          }
        case Some(s) =>
          entryPoints.find(_.name == s) match{
            case None =>
              val suffix =
                if (entryPoints.isEmpty) ""
                else{
                  val methods = for(ep <- entryPoints) yield{
                    val args = ep.argSignatures.map(Main.renderArg).mkString(", ")
                    val details = Main.entryDetails(ep)
                    s"def ${ep.name}($args)$details"
                  }
                  s"""
                     |
                     |Existing methods:
                     |
                     |${methods.mkString("\n\n")}""".stripMargin
                }
              Res.Failure(
                None,
                s"Unable to find method: ${backtickWrap(s)}" + suffix
              )
            case Some(entry) =>
              Main.runEntryPoint(entry, args, kwargs).getOrElse(Res.Success(imports))
          }

      }
    } yield res
  }

  /**
    * Run a snippet of code
    */
  def runCode(code: String) = {
    instantiateRepl().interp.replApi.load(code)
  }
}

object Main{
  def renderArg(arg: ArgSig) = backtickWrap(arg.name) + ": " + arg.typeString
  def entryDetails(ep: EntryPoint) = {
    ep.argSignatures.collect{
      case ArgSig(name, tpe, Some(doc), default) =>
        "\n" + name + " // " + doc
    }.mkString
  }
  def runEntryPoint(entry: Router.EntryPoint,
                    args: Seq[String],
                    kwargs: Seq[(String, String)]): Option[Res.Failing] = {

    def expectedMsg = {
      val commaSeparated =
        entry.argSignatures
          .map(renderArg)
          .mkString(", ")
      val details = entryDetails(entry)
      "(" + commaSeparated + ")" + details
    }

    entry.invoke(args, kwargs) match{
      case Router.Result.Success(x) => None
      case Router.Result.Error.Exception(x) => Some(Res.Exception(x, ""))
      case Router.Result.Error.TooManyArguments(x) =>
        Some(Res.Failure(
          None,
          s"""Too many args were passed to this script: ${x.map(literalize(_)).mkString(", ")}
             |expected arguments $expectedMsg""".stripMargin

        ))
      case Router.Result.Error.RedundantArguments(x) =>
        Some(Res.Failure(
          None,
          s"""Redundant values were passed for arguments: ${x.map(literalize(_)).mkString(", ")}
             |expected arguments: $expectedMsg""".stripMargin
        ))
      case Router.Result.Error.InvalidArguments(x) =>
        Some(Res.Failure(
          None,
          "The following arguments failed to be parsed:\n" +
          x.map{
            case Router.Result.ParamError.Missing(p) =>
              s"(${renderArg(p)}) was missing"
            case Router.Result.ParamError.Invalid(p, v, ex) =>
              s"(${renderArg(p)}) failed to parse input ${literalize(v)} with $ex"
            case Router.Result.ParamError.DefaultFailed(p, ex) =>
              s"(${renderArg(p)})'s default value failed to evaluate with $ex"
          }.mkString("\n") + "\n" + s"expected arguments: $expectedMsg"
        ))
    }
  }

  val defaultWelcomeBanner = {
    def ammoniteVersion = ammonite.Constants.version
    def scalaVersion = scala.util.Properties.versionNumberString
    def javaVersion = System.getProperty("java.version")
    s"""Welcome to the Ammonite Repl $ammoniteVersion
       |(Scala $scalaVersion Java $javaVersion)""".stripMargin
  }
  val ignoreUselessImports = """
    |notify => _,
    |  wait => _,
    |  equals => _,
    |  asInstanceOf => _,
    |  synchronized => _,
    |  notifyAll => _,
    |  isInstanceOf => _,
    |  == => _,
    |  != => _,
    |  getClass => _,
    |  ne => _,
    |  eq => _,
    |  ## => _,
    |  hashCode => _,
    |  _
    |"""

  val defaultPredefString = s"""
    |import ammonite.repl.frontend.ReplBridge.repl
    |import ammonite.ops.Extensions.{
    |  $ignoreUselessImports
    |}
    |import ammonite.repl.tools._
    |import ammonite.repl.tools.IvyConstructor.{ArtifactIdExt, GroupIdExt}
    |import ammonite.repl.frontend.ReplBridge.repl.{
    |  Internal => _,
    |  $ignoreUselessImports
    |}
    |import ammonite.repl.main.Router.{doc, export}
    |import ammonite.repl.Main.pathScoptRead
    |""".stripMargin


  def defaultAmmoniteHome = Path(System.getProperty("user.home"))/".ammonite"

  /**
    * The command-line entry point, which does all the argument parsing before
    * delegating to [[Main.run]]
    */
  def main(args0: Array[String]) = {
    var fileToExecute: Option[Path] = None
    var codeToExecute: Option[String] = None
    var logTimings = false
    var ammoniteHome: Option[Path] = None
    var passThroughArgs: Seq[String] = Vector.empty
    var predefFile: Option[Path] = None
    var mainMethodName: Option[String] = None
    val replParser = new scopt.OptionParser[Main]("ammonite") {
      // Primary arguments that correspond to the arguments of
      // the `Main` configuration object
      head("ammonite", ammonite.Constants.version)
      opt[String]('p', "predef")
        .action((x, c) => c.copy(predef = x))
        .text("Any commands you want to execute at the start of the REPL session")
      opt[Unit]("no-default-predef")
        .action((x, c) => c.copy(defaultPredef = false))
        .text("Disable the default predef and run Ammonite with the minimal predef possible")

      // Secondary arguments that correspond to different methods of
      // the `Main` configuration arguments
      arg[String]("<file-args>...")
        .optional()
        .foreach{ x => fileToExecute = Some(Path(x, cwd)) }
        .text("The Ammonite script file you want to execute")
      opt[String]('c', "code")
        .foreach(x => codeToExecute = Some(x))
        .text("Pass in code to be run immediately in the REPL")
      opt[Unit]('t', "time")
        .foreach(_ => logTimings = true)
        .text("Print time taken for each step")
      opt[String]('x', "execute")
        .foreach{ x => mainMethodName = Some(x)}
        .text(
          """What main method you want to execute, if any. Defaults to a `main`
            |method if it exists, but you can also run other methods defined in
            |the script.
            |
            |This flag must come last among the flags passed to Ammonite, and any
            |further arguments, both positional and named (e.g. `--foo bar`) are
            |forwarded to the script you are running.
            |
            |You can also use `--` as a shorthand for `-x main`, to pass arguments
            |to the main method
          """.stripMargin.replace("\n", "\n" + " " * 8)
        )
      arg[String]("<args>...")
        .optional()
        .unbounded()
        .foreach{ x => passThroughArgs = passThroughArgs :+ x }
        .text("Any arguments you want to pass to the Ammonite script file")
      opt[File]('h', "home")
        .valueName("<file>")
        .foreach( x => ammoniteHome = Some(Path(x, cwd)))
        .text("The home directory of the REPL; where it looks for config and caches")
      opt[String]('f', "predef-file")
        .foreach(x => predefFile = Some(Path(x, cwd)))
        .text("Lets you load your predef from a custom location")

    }

    val (take, drop) = args0.indexOf("--") match {
      case -1 =>
        args0.indexOf("-x") match {
          case -1 =>
            args0.indexOf("--execute") match {
              case -1 => (Int.MaxValue, Int.MaxValue)
              case n => (n+2, n+2)
            }
          case n => (n+2, n+2)
        }
      case n => (n, n+1)
    }

    val before = args0.take(take)
    var keywordTokens = args0.drop(drop).toList
    var kwargs = Vector.empty[(String, String)]

    while(keywordTokens.nonEmpty){
      if (keywordTokens(0).startsWith("--")){
        kwargs = kwargs :+ (keywordTokens(0).drop(2), keywordTokens(1))
        keywordTokens = keywordTokens.drop(2)
      }else{
        passThroughArgs = passThroughArgs :+ keywordTokens(0)
        keywordTokens = keywordTokens.drop(1)
      }
    }

    for(c <- replParser.parse(before, Main())){
      Timer.show = logTimings
      val main = Main(
        c.predef,
        c.defaultPredef,
        predefFile match{
          case None => new Storage.Folder(ammoniteHome.getOrElse(defaultAmmoniteHome))
          case Some(pf) =>
            new Storage.Folder(ammoniteHome.getOrElse(defaultAmmoniteHome)){
              override val predef = pf
            }
        }
      )
      (fileToExecute, codeToExecute) match{
        case (None, None) => println("Loading..."); main.run()
        case (Some(path), None) =>
          main.runScript(path, mainMethodName, passThroughArgs, kwargs.toSeq) match{
            case Res.Failure(exOpt, msg) =>
              Console.err.println(msg)
              System.exit(1)
            case Res.Exception(ex, s) =>
              val trace = ex.getStackTrace
              val i = trace.indexWhere(_.getMethodName == "$main") + 1
              ex.setStackTrace(trace.take(i))
              throw ex
            case Res.Success(_) =>
            // do nothing on success, everything's already happened
          }

        case (None, Some(code)) => main.runCode(code)
      }
    }
  }

  def maybeDefaultPredef(enabled: Boolean, predef: String) =
    if (enabled) predef else ""

  /**
    * Additional [[scopt.Read]] instance to teach it how to read Ammonite paths
    */
  implicit def pathScoptRead: scopt.Read[Path] = scopt.Read.stringRead.map(Path(_, cwd))
}

case class EntryConfig(file: Option[Path])

case class ArgParseException(name: String,
                             value: String,
                             typeName: String,
                             cause: Throwable)
  extends Exception(
    "\n" +
    s"""Cannot parse value "${pprint.PPrinter.escape(value)}" """ +
    s"into arg `$name: $typeName`",
    cause
  )

