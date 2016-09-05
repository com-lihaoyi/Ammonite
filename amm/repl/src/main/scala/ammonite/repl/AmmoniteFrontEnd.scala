package ammonite.repl

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import ammonite.terminal._
import fastparse.core.Parsed
import ammonite.util.{Colors, Res}
import ammonite.runtime.Parsers

class AmmoniteFrontEnd extends FrontEnd {

  def width = FrontEndUtils.width
  def height = FrontEndUtils.height

  def action(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             colors: Colors,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: IndexedSeq[String],
             addHistory: String => Unit) = {
    val res = readLine(reader, output, prompt) match {
      case None => Res.Exit(())
      case Some(code) =>
        addHistory(code)
        Parsers.Splitter.parse(code) match {
          case Parsed.Success(value, idx) =>
            Res.Success((code, value))
          case Parsed.Failure(_, index, extra) =>
            Res.Failure(
              None,
              fastparse.core.ParseError.msg(extra.input, extra.traced.expected, index)
            )
        }
    }
    res
  }

  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String) = {
    val writer = new OutputStreamWriter(output)
    Terminal.readLine(prompt, reader, writer)
  }
}
