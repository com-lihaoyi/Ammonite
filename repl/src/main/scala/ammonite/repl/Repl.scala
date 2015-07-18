package ammonite.repl

import java.io._
import ammonite.repl.Util.IvyMap
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class Repl(input: InputStream,
           output: OutputStream,
           storage: Ref[Storage],
           predef: String = "") {

  val shellPrompt = Ref("@ ")

  val colorSet = Ref[ColorSet](ColorSet.Default)
  val frontEnd = Ref[FrontEnd](FrontEnd.JLine)

  val printer = new PrintStream(output, true)
  val interp: Interpreter = new Interpreter(
    shellPrompt,
    frontEnd,
    pprint.Config.Colors.PPrintConfig.copy(
      width = frontEnd().width,
      height = frontEnd().height / 2
    ),
    colorSet,
    printer.print,
    storage,
    predef
  )
  val reader = new InputStreamReader(input)
  def action() = for{
    (code, stmts) <- frontEnd().action(
      input,
      reader,
      output,
      colorSet().prompt + shellPrompt() + scala.Console.RESET,
      interp.pressy.complete(_, interp.eval.previousImportBlock, _),
      storage().history()
    )
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.processLine(code, stmts, _.foreach(printer.print))
  } yield {
    printer.println()
    out
  }


  def run() = {
    @tailrec def loop(): Unit = {
      val res = action()
      if (interp.handleOutput(res)) loop()
    }
    loop()
  }
}

object Repl{
  case class Config(predef: String = "", ammoniteHome: java.io.File = defaultAmmoniteHome)

  def defaultAmmoniteHome = new java.io.File(System.getProperty("user.home") + "/.ammonite")
  def main(args: Array[String]) = {
    val parser = new scopt.OptionParser[Config]("ammonite") {
      head("ammonite", ammonite.Constants.version)
      opt[String]('p', "predef")
        .action((x, c) => c.copy(predef = x))
        .text("Any commands you want to execute at the start of the REPL session")
      opt[File]('h', "home")
        .valueName("<file>")
        .action((x, c) => c.copy(ammoniteHome = x))
        .text("The home directory of the REPL; where it looks for config and caches")
    }
    parser.parse(args, Config()).foreach(c => run(c.predef, c.ammoniteHome))
  }
  def run(predef: String = "", ammoniteHome: java.io.File = defaultAmmoniteHome) = {
    println("Loading Ammonite Repl...")
    val storage = Storage(ammoniteHome)
    val shell = new Repl(
      System.in, System.out,
      storage = Ref(storage),
      predef = predef + "\n" + storage.loadPredef
    )
    shell.run()

  }
}

