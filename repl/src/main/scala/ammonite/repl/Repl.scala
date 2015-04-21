package ammonite.repl

import java.io._
import ammonite.{pprint}
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter

import scala.annotation.tailrec
import scala.util.Try

class Repl(input: InputStream,
           output: OutputStream,
           colorSet: ColorSet = ColorSet.Default,
           pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig,
           shellPrompt0: String = "@",
           initialHistory: Seq[String] = Nil,
           saveHistory: String => Unit = _ => (),
           predef: String = Repl.defaultPredef) {

  val shellPrompt = Ref(shellPrompt0)

  val frontEnd = JLineFrontend(
    input,
    output,
    colorSet.prompt + shellPrompt() + scala.Console.RESET,
    interp.pressy.complete(_, interp.eval.previousImportBlock, _),
    initialHistory
  )

  val interp: Interpreter = new Interpreter(
    frontEnd.update,
    shellPrompt,
    pprintConfig.copy(maxWidth = frontEnd.width),
    colorSet,
    stdout = new PrintStream(output).println,
    initialHistory = initialHistory
  )

  def action(input: Res[String], save: Boolean) = for{
    // Condition to short circuit early if `interp` hasn't finished evaluating
    line <- input
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.processLine(line, (f, x) => if (save) {saveHistory(x); f(x)}, _.foreach(print))
  } yield {
    println()
    out
  }

  def run() = {
    @tailrec
    def loop(): Unit = {
      val res = action(frontEnd.action(interp.buffered), true)
      if (interp.handleOutput(res)) loop()
    }

    for (line <- predef.lines map (_.trim) if !line.isEmpty) {
      val res = action(Res.Success(line), false)
      interp.handleOutput(res)
    }
    loop()
  }
}

object Repl{
  val defaultPredef = """"""
  def main(args: Array[String]) = {
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
      }
    )
    shell.run()

  }
}
