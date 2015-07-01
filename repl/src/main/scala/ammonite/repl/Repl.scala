package ammonite.repl

import java.io._
import ammonite.{pprint}
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
           initialHistory: Seq[String] = Nil,
           saveHistory: History => Unit = _ => (),
           predef: String = "") {

  val shellPrompt = Ref(shellPrompt0)

  val history = new History
  history ++= initialHistory

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
      maxWidth = () => frontEnd().width,
      lines = () => frontEnd().height / 2
    ),
    colorSet,
    printer.print,
    history,
    predef
  )

  def action() = for{
    (code, stmts) <- frontEnd().action(
      input,
      output,
      colorSet().prompt + shellPrompt() + scala.Console.RESET,
      interp.pressy.complete(_, interp.eval.previousImportBlock, _),
      history
    )
    _ = {
      history += code
      saveHistory(history)
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
      initialHistory = storage.loadHistory,
      saveHistory = { s =>
        storage.saveHistory(s)
      },
      predef = storage.loadPredef
    )
    shell.run()

  }
}

