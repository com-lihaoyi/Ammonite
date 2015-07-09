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
           history0: Ref[History],
           ivyCache0: Ref[IvyMap],
           predef: String = "") {

  val shellPrompt = Ref(shellPrompt0)

  val colorSet = Ref[ColorSet](ColorSet.Default)
  val frontEnd = Ref[FrontEnd](FrontEnd.JLine)
  def consoleDim(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"tput $s 2> /dev/tty").!!.trim.toInt
  }

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
    history0(),
    ivyCache0,
    predef
  )

  def action() = for{
    (code, stmts) <- frontEnd().action(
      input,
      output,
      colorSet().prompt + shellPrompt() + scala.Console.RESET,
      interp.pressy.complete(_, interp.eval.previousImportBlock, _),
      history0()
    )
    _ = {
      history0() = history0() :+ code
    }
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.processLine(stmts, _.foreach(printer.print))
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
      history0 = Ref(storage.loadHistory, storage.saveHistory),
      ivyCache0 = Ref(storage.loadIvyCache, storage.saveIvyCache),
      predef = storage.loadPredef
    )
    shell.run()

  }
}

