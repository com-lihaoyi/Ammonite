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
           pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig,
           shellPrompt0: String = "@ ",
           storage: Ref[Storage],
           predef: String = "") {

  val shellPrompt = Ref(shellPrompt0)

  val colorSet = Ref[ColorSet](ColorSet.Default)
  val frontEnd = Ref[FrontEnd](FrontEnd.JLine)

  val printer = new PrintStream(output, true)
  val interp: Interpreter = new Interpreter(
    shellPrompt,
    frontEnd,
    pprintConfig.copy(
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
  def defaultAmmoniteHome = System.getProperty("user.home") + "/.ammonite"
  def main(args: Array[String]) = run(args.lift(0).getOrElse(defaultAmmoniteHome))
  def run(ammoniteHome: String = defaultAmmoniteHome) = {
    println("Loading Ammonite Repl...")
    val storage = Storage(new java.io.File(ammoniteHome))
    val shell = new Repl(
      System.in, System.out,
      storage = Ref(storage),
      predef = storage.loadPredef
    )
    shell.run()

  }
}

