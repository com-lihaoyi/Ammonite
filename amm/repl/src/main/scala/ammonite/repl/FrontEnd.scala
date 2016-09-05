package ammonite.repl

import java.io.{InputStream, OutputStream}

import fastparse.core.Parsed
import jline.console.{ConsoleReader, completer}
import acyclic.file
import ammonite.util.{Colors, Catching, Res}
import ammonite.runtime.Parsers
import ammonite.util.Util.newLine

import scala.annotation.tailrec
import scala.tools.nsc.interpreter.JList

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
             colors: Colors,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: IndexedSeq[String],
             addHistory: String => Unit): Res[(String, Seq[String])]
}
