package ammonite.main

import mainargs.{main, arg, Flag, Leftover, ParserForClass}
import ammonite.repl.tools.Util.PathRead
@main
case class Config(core: Config.Core,
                  predef: Config.Predef,
                  repl: Config.Repl,
                  scripts: Config.Scripts,
                  rest: String*)

object Config{

  @main
  case class Core(
    @arg(
      name = "no-default-predef",
      doc = "Disable the default predef and run Ammonite with the minimal predef possible")
    noDefaultPredef: Flag,
    @arg(
      short = 's',
      doc = """Make ivy logs go silent instead of printing though failures will
        still throw exception""")
    silent: Flag,

    @arg(
      short = 'w',
      doc = "Watch and re-run your scripts when they change")
    watch: Flag,
    @arg(doc = "Run a BSP server against the passed scripts")
    bsp: Flag,
    @arg(
      short = 'c',
      doc = "Pass in code to be run immediately in the REPL")
    code: Option[String] = None,
    @arg(
      short = 'h',
      doc = "The home directory of the REPL; where it looks for config and caches")
    home: os.Path = Defaults.ammoniteHome,
    @arg(
      name = "predef",
      short = 'p',
      doc ="""Lets you load your predef from a custom location, rather than the
        "default location in your Ammonite home""")
    predefFile: Option[os.Path] = None,
    @arg(
      doc = """Enable or disable colored output; by default colors are enabled
          in both REPL and scripts if the console is interactive, and disabled
          otherwise""")
    color: Option[Boolean] = None,
    @arg(
      doc ="""Hide parts of the core of Ammonite and some of its dependencies. By default,
        the core of  Ammonite and all of its dependencies can be seen by users from the
        Ammonite session. This option mitigates that via class loader isolation.""")
    thin: Flag,
    @arg(doc = "Print this message")
    help: Flag,
    @arg(name = "version", short = 'v', doc = "Show Ammonite's version")
    showVersion: Flag
  )
  implicit val coreParser = ParserForClass[Core]

  @main
  case class Predef(
    @arg(
      name = "predef-code",
      doc = "Any commands you want to execute at the start of the REPL session")
    predefCode: String = "",

    @arg(
      name = "no-home-predef",
      doc = """Disables the default behavior of loading predef files from your
        ~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You can
        choose an additional predef to use using `--predef""")
    noHomePredef: Flag
  )
  implicit val predefParser = ParserForClass[Predef]

  @main
  case class Repl(
  @arg(
      short = 'b',
      doc = "Customize the welcome banner that gets shown when Ammonite starts")
    banner: String = Defaults.welcomeBanner,
    @arg(
      name = "no-remote-logging",
      doc =
      "(deprecated) Disable remote logging of the number of times a REPL starts and runs commands")
    noRemoteLogging: Flag,
    @arg(
      name = "class-based",
      doc =
        "Wrap user code in classes rather than singletons, typically for Java serialization "+
        "friendliness.")
    classBased: Flag
  )
  implicit val replParser = ParserForClass[Repl]

  @main
  case class Scripts(
    @arg(
      name = "no-positional-args",
      doc = "Disallow positional arguments for scripts"
    )
    noPositionalArgs: Flag,
    @arg(
      name = "allow-repeat-args",
      doc = "Allow repeated arguments for scripts"
    )
    allowRepeatArgs: Flag
  )
  implicit val scriptsParser = ParserForClass[Scripts]

  val parser = mainargs.ParserForClass[Config]
}
