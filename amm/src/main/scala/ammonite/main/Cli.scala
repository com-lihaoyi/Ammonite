package ammonite.main

import ammonite.runtime.Storage
import ammonite.util.Util

import scala.annotation.tailrec




object Cli{
  case class Arg[T, V](name: String,
                       shortName: Option[Char],
                       doc: String,
                       action: (T, V) => T)
                      (implicit val reader: scopt.Read[V]){
    def runAction(t: T, s: String) = action(t, reader.reads(s))
  }
  case class Config(predefCode: String = "",
                    defaultPredef: Boolean = true,
                    homePredef: Boolean = true,
                    wd: os.Path = os.pwd,
                    welcomeBanner: Option[String] = Some(Defaults.welcomeBanner),
                    verboseOutput: Boolean = true,
                    remoteLogging: Boolean = true,
                    watch: Boolean = false,
                    code: Option[String] = None,
                    home: os.Path = Defaults.ammoniteHome,
                    predefFile: Option[os.Path] = None,
                    help: Boolean = false,
                    colored: Option[Boolean] = None,
                    classBased: Boolean = false)


  import ammonite.repl.tools.Util.pathScoptRead
  val genericSignature = Seq(
    Arg[Config, String](
      "predef-code", None,
      "Any commands you want to execute at the start of the REPL session",
      (c, v) => c.copy(predefCode = v)
    ),

    Arg[Config, String](
      "code", Some('c'),
      "Pass in code to be run immediately in the REPL",
      (c, v) => c.copy(code = Some(v))
    ),
    Arg[Config, os.Path](
      "home", Some('h'),
      "The home directory of the REPL; where it looks for config and caches",
      (c, v) => c.copy(home = v)
    ),
    Arg[Config, os.Path](
      "predef", Some('p'),
      """Lets you load your predef from a custom location, rather than the
        |default location in your Ammonite home""".stripMargin,
      (c, v) => c.copy(predefFile = Some(v))
    ),
    Arg[Config, Unit](
      "no-home-predef", None,
      """Disables the default behavior of loading predef files from your
        |~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You can
        |choose an additional predef to use using `--predef
        |""".stripMargin,
      (c, v) => c.copy(homePredef = false)
    ),

    Arg[Config, Unit](
      "no-default-predef", None,
      """Disable the default predef and run Ammonite with the minimal predef
        |possible
        |""".stripMargin,
      (c, v) => c.copy(defaultPredef = false)
    ),

    Arg[Config, Unit](
      "silent", Some('s'),
      """Make ivy logs go silent instead of printing though failures will
        |still throw exception""".stripMargin,
      (c, v) => c.copy(verboseOutput = false)
    ),
    Arg[Config, Unit](
      "help", None,
      """Print this message""".stripMargin,
      (c, v) => c.copy(help = true)
    ),
    Arg[Config, Boolean](
      "color", None,
      """Enable or disable colored output; by default colors are enabled
        |in both REPL and scripts if the console is interactive, and disabled
        |otherwise""".stripMargin,
      (c, v) => c.copy(colored = Some(v))
    ),
    Arg[Config, Unit](
      "watch", Some('w'),
      "Watch and re-run your scripts when they change",
      (c, v) => c.copy(watch = true)
    )
  )
  val replSignature = Seq(
    Arg[Config, String](
      "banner", Some('b'),
      "Customize the welcome banner that gets shown when Ammonite starts",
      (c, v) => c.copy(welcomeBanner = if (v.nonEmpty) Some(v) else None)
    ),
    Arg[Config, Unit](
      "no-remote-logging", None,
      """Disable remote logging of the number of times a REPL starts and runs
        |commands
        |""".stripMargin,
      (c, v) => c.copy(remoteLogging= false)
    ),
    Arg[Config, Unit](
      "class-based", None,
      """Wrap user code in classes rather than singletons, typically for Java serialization
        |friendliness.
        |""".stripMargin,
      (c, v) => c.copy(classBased=true)
    )

  )

  val ammoniteArgSignature = genericSignature ++ replSignature

  def showArg(arg: Arg[_, _]) =
    "  " + arg.shortName.fold("")("-" + _ + ", ") + "--" + arg.name

  def formatBlock(args: Seq[Arg[_, _]], leftMargin: Int) = {

    for(arg <- args) yield {
      showArg(arg).padTo(leftMargin, ' ').mkString +
      Predef.augmentString(arg.doc).lines.mkString(Util.newLine + " " * leftMargin)
    }
  }
  def ammoniteHelp = {
    val leftMargin = ammoniteArgSignature.map(showArg(_).length).max + 2


    s"""Ammonite REPL & Script-Runner, ${ammonite.Constants.version}
       |usage: amm [ammonite-options] [script-file [script-options]]
       |
       |${formatBlock(genericSignature, leftMargin).mkString(Util.newLine)}
       |
       |REPL-specific args:
       |${formatBlock(replSignature, leftMargin).mkString(Util.newLine)}
    """.stripMargin
  }

  def groupArgs[T](flatArgs: List[String],
                   args: Seq[Arg[T, _]],
                   initial: T): Either[String, (T, List[String])] = {

    val argsMap0: Seq[(String, Arg[T, _])] = args
      .flatMap{x => Seq(x.name -> x) ++ x.shortName.map(_.toString -> x)}

    val argsMap = argsMap0.toMap

    @tailrec def rec(keywordTokens: List[String],
                     current: T): Either[String, (T, List[String])] = {
      keywordTokens match{
        case head :: rest if head(0) == '-' =>
          val realName = if(head(1) == '-') head.drop(2) else head.drop(1)

          argsMap.get(realName) match {
            case Some(cliArg) =>
              if (cliArg.reader == scopt.Read.unitRead) {
                rec(rest, cliArg.runAction(current, ""))
              } else rest match{
                case next :: rest2 => rec(rest2, cliArg.runAction(current, next))
                case Nil => Left(s"Expected a value after argument $head")
              }

            case None => Right((current, keywordTokens))
          }

        case _ => Right((current, keywordTokens))

      }
    }
    rec(flatArgs, initial)
  }
}
