package ammonite.repl

import java.io.File

import ammonite.ops._

import scala.reflect.internal.annotations.compileTimeOnly
import scala.reflect.runtime.universe.TypeTag

/**
  * The various entry-points to the Ammonite repl
  */
object Main{
  case class Config(predef: String = "",
                    predefFile: Option[Path] = None,
                    code: Option[String] = None,
                    ammoniteHome: Path = defaultAmmoniteHome,
                    file: Option[Path] = None,
                    args: Seq[String] = Vector.empty,
                    kwargs: Map[String, String] = Map.empty)

  def defaultAmmoniteHome = Path(System.getProperty("user.home"))/".ammonite"

  /**
    * The command-line entry point, which does all the argument parsing before
    * delegating to [[run]]
    */
  def main(args: Array[String]) = {
    val parser = new scopt.OptionParser[Config]("ammonite") {
      head("ammonite", ammonite.Constants.version)
      opt[String]('p', "predef")
        .action((x, c) => c.copy(predef = x))
        .text("Any commands you want to execute at the start of the REPL session")
      opt[String]('f', "predef-file")
        .action((x, c) => c.copy(predefFile = Some(if (x(0) == '/') Path(x) else cwd/RelPath(x))))
        .text("Lets you load your predef from a custom location")
      opt[String]('c', "code")
        .action((x, c) => c.copy(code = Some(x)))
        .text("Pass in code to be run immediately in the REPL")
      opt[File]('h', "home")
        .valueName("<file>")
        .action((x, c) => c.copy(ammoniteHome = Path(x)))
        .text("The home directory of the REPL; where it looks for config and caches")
      arg[File]("<file-args>...")
        .optional()
        .action { (x, c) => c.copy(file = Some(Path(x))) }
        .text("The Ammonite script file you want to execute")
      arg[String]("<args>...")
        .optional()
        .unbounded()
        .action { (x, c) => c.copy(args = c.args :+ x) }
        .text("Any arguments you want to pass to the Ammonite script file")
    }
    val (before, after) = args.splitAt(args.indexOf("--") match {
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
            .isInstanceOf[fastparse.core.Result.Success[_]],
        s"""Only pairs of keyword arguments can come after `--`.
            |Invalid keyword: $k""".stripMargin
      )
      (k.stripPrefix("--"), v)
    }

    for(c <- parser.parse(before, Config())){
      run(
        c.predef,
        c.ammoniteHome,
        c.code,
        c.predefFile,
        c.file,
        c.args,
        kwargs.toMap
      )
    }
  }

  implicit def ammoniteReplArrowBinder[T](t: (String, T))(implicit typeTag: TypeTag[T]) = {
    Bind(t._1, t._2)(typeTag)
  }

  /**
    * The debug entry-point: embed this inside any Scala program to open up
    * an ammonite REPL in-line with access to that program's variables for
    * inspection.
    */
  def debug(replArgs: Bind[_]*): Any = {

    val storage = Storage(defaultAmmoniteHome, None)
    val repl = new Repl(
      System.in, System.out,
      storage = Ref(storage),
      predef = "",
      replArgs
    )

    repl.run()
  }

  /**
    * The main entry-point after partial argument de-serialization.
    */
  def run(predef: String = "",
          ammoniteHome: Path = defaultAmmoniteHome,
          code: Option[String] = None,
          predefFile: Option[Path] = None,
          file: Option[Path] = None,
          args: Seq[String] = Vector.empty,
          kwargs: Map[String, String] = Map.empty) = {

    Timer("Repl.run Start")
    def storage = Storage(ammoniteHome, predefFile)
    lazy val repl = new Repl(
      System.in, System.out,
      storage = Ref(storage),
      predef = predef
    )
    (file, code) match{
      case (None, None) => println("Loading..."); repl.run()
      case (Some(path), None) => runScript(repl, path, args, kwargs)
      case (None, Some(code)) => repl.interp.replApi.load(code)
    }
    Timer("Repl.run End")
  }

  def runScript(repl: Repl, path: Path, args: Seq[String], kwargs: Map[String, String]): Unit = {
    val imports = repl.interp.processModule(read(path))
    repl.interp.init()
    imports.find(_.toName == "main").foreach { i =>
      val quotedArgs =
        args.map(pprint.PPrinter.escape)
            .map(s => s"""arg("$s")""")

      val quotedKwargs =
        kwargs.mapValues(pprint.PPrinter.escape)
          .map { case (k, s) => s"""$k=arg("$s")""" }

      repl.interp.replApi.load(s"""
        |import ammonite.repl.ScriptInit.{arg, callMain, pathRead}
        |callMain{
        |main(${(quotedArgs ++ quotedKwargs).mkString(", ")})
        |}
      """.stripMargin)
    }
  }
}

/**
  * Code used to de-serialize command-line arguments when calling an Ammonite
  * script. Basically looks for a [[scopt.Read]] for the type of each argument
  * and uses that to de-serialize the given [[String]] into that argument.
  *
  * Needs a bit of macro magic to work.
  */
object ScriptInit{
  import language.experimental.macros
  import reflect.macros.Context
  def callMainImpl[T](c: Context)(t: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val apply = t.tree.asInstanceOf[Apply]
    val paramSymbols = apply.symbol.typeSignature.asInstanceOf[MethodType].params
    val reads = paramSymbols.zip(apply.args).map{ case (tpe, term) =>
      term match{
        case q"$prefix($inner)" => q"implicitly[scopt.Read[$tpe]].reads($inner)"
        case x => x
      }
    }
    c.Expr[T](q"${apply.fun}(..$reads)")
  }
  @compileTimeOnly("This is a marker function and should not exist after macro expansion")
  def arg[T](s: String): T = ???

  /**
    * Takes the call to the main method, with [[arg]]s wrapping every argument,
    * and converts them to the relevant [[scopt.Read]] calls to properly
    * de-serialize them
    */
  def callMain[T](t: T): T = macro callMainImpl[T]

  /**
    * Additional [[scopt.Read]] instance to teach it how to read Ammonite paths
    */
  implicit def pathRead = scopt.Read.stringRead.map(Path(_))
}