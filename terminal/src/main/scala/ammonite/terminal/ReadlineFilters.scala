package ammonite.terminal
import acyclic.file
import FilterTools._
import ammonite.terminal.LazyList._

object ReadlineFilters {

  // www.bigsmoke.us/readline/shortcuts
  // Ctrl-b     <- one char
  // Ctrl-f     -> one char
  // Alt-b      <- one word
  // Alt-f      -> one word
  // Ctrl-a     <- start of line
  // Ctrl-e     -> end of line
  // Ctrl-x-x   Toggle start/end

  // Backspace  <- delete char
  // Del        -> delete char
  // Ctrl-u     <- delete all
  // Ctrl-k     -> delete all
  // Alt-d      -> delete word
  // Ctrl-w     <- delete word

  // Ctrl-u/-   Undo
  // Ctrl-l     clear screen

  // Ctrl-k     -> cut all
  // Alt-d      -> cut word
  // Alt-Backspace  <- cut word
  // Ctrl-y     paste last cut

  def navFilter = Filters(
    Case("b")((b, c, m) => (b, c-1)), // <- one char
    Case("f")((b, c, m) => (b, c-1)), // -> one char
    Case(Alt+"b")((b, c, m) => wordLeft(b, c)), // <- one word
    Case(Alt+"f")((b, c, m) => wordRight(b, c)), // -> one  word
    Case(Ctrl('a'))((b, c, m) => BasicFilters.moveStart(b, c, m.width)), // <- one line
    Case(Ctrl('e'))((b, c, m) => BasicFilters.moveEnd(b, c, m.width)) // -> one line
  )

  class cutPasteFilter() extends TermCore.DelegateFilter{
    var currentCut = Vector.empty[Char]
    def cutAllLeft(b: Vector[Char], c: Int) = {
      currentCut = b.take(c)
      (b.drop(c), 0)
    }
    def cutAllRight(b: Vector[Char], c: Int) = {
      currentCut = b.drop(c)
      (b.take(c), c)
    }

    def cutWordRight(b: Vector[Char], c: Int) = {
      val start = consumeWord(b, c, 1, 0)
      currentCut = b.slice(c, start)
      (b.take(c) ++ b.drop(start), c)
    }

    def cutWordLeft(b: Vector[Char], c: Int) = {
      val start = consumeWord(b, c, -1, 1)
      currentCut = b.slice(start, c)
      (b.take(start) ++ b.drop(c), start)
    }

    def paste(b: Vector[Char], c: Int) = {
      (b.take(c) ++ currentCut ++ b.drop(c), c + currentCut.length)
    }

    def filter = Filters(
      Case(Ctrl('u'))((b, c, m) => cutAllLeft(b, c)),
      Case(Ctrl('k'))((b, c, m) => cutAllRight(b, c)),
      Case(Alt + "d")((b, c, m) => cutWordRight(b, c)),
      Case(Ctrl('w'))((b, c, m) => cutWordLeft(b, c)),
      Case(Ctrl('y'))((b, c, m) => paste(b, c))
    )
  }
  def consumeWord(b: Vector[Char], c: Int, delta: Int, offset: Int) = {
    var current = c
    // Move at least one character! Otherwise
    // you get stuck at the end of a word.
    current += delta
    while(b.isDefinedAt(current) && !b(current).isLetterOrDigit) current += delta
    while(b.isDefinedAt(current) && b(current).isLetterOrDigit) current += delta
    current + offset
  }


  def firstRow(cursor: Int, buffer: Vector[Char], width: Int) = {
    cursor < width && (buffer.indexOf('\n') >= cursor || buffer.indexOf('\n') == -1)
  }
  def lastRow(cursor: Int, buffer: Vector[Char], width: Int) = {
    (buffer.length - cursor) < width && (buffer.lastIndexOf('\n') < cursor || buffer.lastIndexOf('\n') == -1)
  }
  class HistoryFilter(history: => Seq[String]) extends TermCore.DelegateFilter{
    var index = -1
    var currentHistory = Vector[Char]()

    def continue(b: Vector[Char], newIndex: Int, rest: LazyList[Int], c: Int) = {
      if (index == -1 && newIndex != -1) currentHistory = b

      index = newIndex

      if (index == -1) TS(rest, currentHistory, c)
      else TS(rest, history(index).toVector, c)
    }
    def filter = {
      case TI(TS(p"\u001b[A$rest", b, c), w) if firstRow(c, b, w) =>
        continue(b, (index + 1) min (history.length - 1), rest, 99999)
      case TI(TS(p"\u001b[B$rest", b, c), w) if lastRow(c, b, w) =>
        continue(b, (index - 1) max -1, rest, 0)
    }
  }


  def wordLeft(b: Vector[Char], c: Int) = b -> consumeWord(b, c, -1, 1)
  def wordRight(b: Vector[Char], c: Int) = b -> consumeWord(b, c, 1, 0)


}
