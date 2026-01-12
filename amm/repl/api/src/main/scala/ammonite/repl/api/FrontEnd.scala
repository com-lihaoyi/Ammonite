package ammonite.repl.api

import java.io.{InputStream, OutputStream}

import ammonite.util.{Colors, Res}

/** JLine interface */
trait FrontEnd {
  def width: Int
  def height: Int
  def action(
      input: InputStream,
      reader: java.io.Reader,
      output: OutputStream,
      prompt: String,
      colors: Colors,
      compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
      history: IndexedSeq[String],
      addHistory: String => Unit,
      fileName: String
  ): Res[(String, Seq[String])]
}
