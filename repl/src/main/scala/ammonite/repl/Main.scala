package ammonite.repl

import java.io.{File, InputStream, OutputStream}

import ammonite.ops._

import scala.reflect.internal.annotations.compileTimeOnly
import scala.reflect.runtime.universe.TypeTag
import language.experimental.macros
import reflect.macros.Context


/**
  * Contains the various entry points to the Ammonite REPL.
  *
  * Configuration of the basic REPL is done by passing in arguments when
  * constructing the [[Main]] instance, and the various entrypoints such
  * as [[run]] [[debug]] and so on are methods on that instance.
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
  *                      `grep`, the `|` `||` `|?` pipes from ammonite-ops, and
  *                      other helpers. Can be disabled to give a clean
  *                      namespace for you to fill using your own predef.
  * @param storageBackend Where will all of Ammonite's persistent data get
  *                       stored? Things like any `predef.scala` file,
  *                       compilation/ivy caches, etc.. Defaults include
  *                       [[Storage.Folder]] and [[Storage.InMemory]], though
  *                       you can create your own.
  */
case class Main(predef: String = "",
                defaultPredef: Boolean = true,
                storageBackend: Storage = Storage.InMemory(),
                inputStream: InputStream = System.in,
                outputStream: OutputStream = System.out,
                errorStream: OutputStream = System.err){
  /**
    * Instantiates an ammonite.repl.Repl using the configuration
    */
  def instantiateRepl() = {
    val augmentedPredef = Main.maybeDefaultPredef(defaultPredef, Main.defaultPredefString)
    new Repl(
      inputStream, outputStream, errorStream,
      storage = storageBackend,
      predef = augmentedPredef + "\n" + predef
    )
  }
  def run() = {
    Timer("Repl.run Start")
    val res = instantiateRepl().run()
    Timer("Repl.run End")
    res
  }

  /**
    * The debug entry-point: embed this inside any Scala program to open up
    * an ammonite REPL in-line with access to that program's variables for
    * inspection.
    */
  def debug(replArgs: Bind[_]*): Any = {

    val repl = new Repl(
      System.in, System.out, System.err,
      storage = storageBackend,
      predef = Main.defaultPredefString,
      replArgs
    )

    repl.run()
  }

  /**
    * Run a Scala script file! takes the path to the file as well as an array
    * of `args` and a map of keyword `kwargs` to pass to that file.
    */
  def runScript(path: Path,
                args: Seq[String],
                kwargs: Map[String, String]): Res[Seq[ImportData]] = {

    val repl = instantiateRepl()
    repl.interp.processModule(read(path), path.last) match{
      case x: Res.Failing => x
      case Res.Success(imports) =>
        repl.interp.init()
        imports.find(_.toName == "main") match {
          case None => Res.Success(imports)
          case Some(i) =>
            val quotedArgs =
              args.map(pprint.PPrinter.escape)
                .map(s => s"""arg("$s")""")

            val quotedKwargs =
              kwargs.mapValues(pprint.PPrinter.escape)
                .map { case (k, s) => s"""$k=arg("$s")""" }
            repl.interp.processExec(
              s"""|import ammonite.repl.ScriptInit.{arg, callMain, pathRead}
                  |callMain{
                  |  main(${(quotedArgs ++ quotedKwargs).mkString(", ")})
                  |}
                  |""".stripMargin
            ) match {
              case Res.Success(_) => Res.Success(imports)
              case Res.Exception(e: ArgParseException, s) =>
                e.setStackTrace(Array())
                e.cause.setStackTrace(e.cause.getStackTrace.takeWhile( frame =>
                  frame.getClassName != "ammonite.repl.ScriptInit$" ||
                  frame.getMethodName != "parseScriptArg"
                ))
                Res.Exception(e, s)
              case x => x
            }
        }
    }
  }

  /**
    * Run a snippet of code
    */
  def runCode(code: String) = {
    instantiateRepl().interp.replApi.load(code)
  }
}

object Main{
  val defaultPredefString = """
    |import ammonite.ops.Extensions.{
    |  notify => _,
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
    |}
    |import ammonite.repl.tools._
    |import ammonite.repl.tools.IvyConstructor.{ArtifactIdExt, GroupIdExt}
    |import ammonite.repl.frontend.ReplBridge.repl.{
    |  notify => _,
    |  wait => _,
    |  equals => _,
    |  asInstanceOf => _,
    |  synchronized => _,
    |  notifyAll => _,
    |  isInstanceOf => _,
    |  == => _,
    |  Internal => _,
    |  != => _,
    |  getClass => _,
    |  ne => _,
    |  eq => _,
    |  ## => _,
    |  hashCode => _,
    |  _
    |}
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


    val (before, after) = args0.splitAt(passThroughArgs.indexOf("--") match {
      case -1 => Int.MaxValue
      case n => n
    })
    val keywordTokens = after.drop(1)
    assert(
      keywordTokens.length % 2 == 0,
      s"""Only pairs of keyword arguments can come after `--`.
          |Invalid number of tokens: ${keywordTokens.length}""".stripMargin
    )

    val kwargs = for(Array(k, v) <- keywordTokens.grouped(2)) yield{

      assert(
        k.startsWith("--") &&
          scalaparse.syntax
            .Identifiers
            .Id
            .parse(k.stripPrefix("--"))
            .isInstanceOf[fastparse.core.Parsed.Success[_]],
        s"""Only pairs of keyword arguments can come after `--`.
            |Invalid keyword: $k""".stripMargin
      )
      (k.stripPrefix("--"), v)
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
          main.runScript(path, passThroughArgs, kwargs.toMap) match{
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
/**
  * Code used to de-serialize command-line arguments when calling an Ammonite
  * script. Basically looks for a [[scopt.Read]] for the type of each argument
  * and uses that to de-serialize the given [[String]] into that argument.
  *
  * Needs a bit of macro magic to work.
  */
object ScriptInit{

  def parseScriptArg[T: scopt.Read: TypeTag](name: String, value: String) = try {
    implicitly[scopt.Read[T]].reads(value)
  } catch{ case e: Throwable =>
    val typeName = implicitly[TypeTag[T]].tpe.toString
    throw ArgParseException(name, value, typeName, e)
  }


  /**
    * Dummy marker method used to make the compiler happy, at least until the [[callMain]]
    * macro swoops in and rewrites things to use the real deserialization calls to the
    * real expected types.
    */
  @compileTimeOnly("This is a marker function and should not exist after macro expansion")
  def arg(s: String): Nothing = ???

  /**
    * Takes the call to the main method, with [[arg]]s wrapping every argument,
    * and converts them to the relevant [[scopt.Read]] calls to properly
    * de-serialize them
    */
  def callMain[T](t: T): T = macro callMainImpl[T]


  def callMainImpl[T](c: Context)(t: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val apply = t.tree.asInstanceOf[Apply]
    val paramSymbols = apply.symbol.typeSignature.asInstanceOf[MethodType].params
    val reads = paramSymbols.zip(apply.args).map{ case (param, term) =>
      val assign = term match{
        case q"ammonite.repl.ScriptInit.arg($inner)" =>

          val newPrefix = q"ammonite.repl.ScriptInit.parseScriptArg"
          q"$newPrefix[${param.typeSignature}](${param.name.decoded}, $inner)"
        case x => x // This case should only be for default args, which we leave unchanged
      }
      assign
    }
    c.Expr[T](q"${apply.fun}(..$reads)")
  }

  /**
    * Additional [[scopt.Read]] instance to teach it how to read Ammonite paths
    */
  implicit def pathRead: scopt.Read[Path] = scopt.Read.stringRead.map(Path(_, cwd))
}