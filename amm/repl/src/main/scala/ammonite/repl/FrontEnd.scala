package ammonite.repl

import java.io.{InputStream, OutputStream}

import ammonite.util.Res

/**
  * All the mucky JLine interfacing code
  */
trait FrontEnd {
  def width: Int
  def height: Int
  def action(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             addHistory: String => Unit): Res[(String, Seq[String])]
}
