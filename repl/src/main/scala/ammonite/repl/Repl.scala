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
           saveHistory: String => Unit = _ => (),
           predef: String = Repl.defaultPredef) {

  val shellPrompt = Ref(shellPrompt0)

  var history = Vector.empty[String]

  val colorSet = Ref[ColorSet](ColorSet.Default)
  val frontEnd = Ref[FrontEnd](FrontEnd.JLine)
  def consoleDim(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"tput $s 2> /dev/tty").!!.trim.toInt
  }
  val cols = Cell(consoleDim("cols"))
  val lines = Cell(consoleDim("lines"))
  val printer = new PrintStream(output, true)
  val interp: Interpreter = new Interpreter(
    shellPrompt,
    frontEnd,
    pprintConfig.copy(
      maxWidth = () => cols(),
      lines = () => lines() / 2
    ),
    colorSet,
    printer.print,
    initialHistory ++ history,
    predef
  )

  def action() = for{
    (code, stmts) <- frontEnd().action(
      input,
      output,
      colorSet().prompt + shellPrompt() + scala.Console.RESET,
      interp.pressy.complete(_, interp.eval.previousImportBlock, _),
      initialHistory ++ history
    )
    _ = {
      history = history :+ code
      saveHistory(code)
      cols.update()
      lines.update()
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
  val defaultPredef = ""
  def main(args: Array[String]) = run()
  def run(predef: String = defaultPredef) = {
    println("Loading Ammonite Repl...")

    val saveFile = new java.io.File(System.getProperty("user.home")) + "/.amm"
    val delimiter = "\n\n\n"
    val shell = new Repl(
      System.in, System.out,
      initialHistory = try{
        io.Source.fromFile(saveFile).mkString.split(delimiter)
      }catch{case e: FileNotFoundException =>
        Nil
      },
      saveHistory = { s =>
        val fw = new FileWriter(saveFile, true)
        try fw.write(delimiter + s)
        finally fw.close()
      },
      predef = predef
    )
    shell.run()

  }
}

