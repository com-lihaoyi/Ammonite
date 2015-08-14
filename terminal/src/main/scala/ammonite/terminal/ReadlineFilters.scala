package ammonite.terminal
import acyclic.file
import FilterTools._
import ammonite.terminal.LazyList._
import SpecialKeys._
/**
 * Filters for injection of readline-specific hotkeys, the sort that
 * are available in bash, python and most other interactive command-lines
 */
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

  /**
   * Basic readline-style navigation, using all the obscure alphabet 
   * hotkeys rather than using arrows
   */
  lazy val navFilter = orElseAll(
    Case(Ctrl('b'))((b, c, m) => (b, c - 1)), // <- one char
    Case(Ctrl('f'))((b, c, m) => (b, c + 1)), // -> one char
    Case(Alt + "b")((b, c, m) => GUILikeFilters.wordLeft(b, c)), // <- one word
    Case(Alt + "f")((b, c, m) => GUILikeFilters.wordRight(b, c)), // -> one  word
    Case(Home)((b, c, m) => BasicFilters.moveStart(b, c, m.width)), // <- one line
    Case(Ctrl('a'))((b, c, m) => BasicFilters.moveStart(b, c, m.width)),
    Case(End)((b, c, m) => BasicFilters.moveEnd(b, c, m.width)), // -> one line
    Case(Ctrl('e'))((b, c, m) => BasicFilters.moveEnd(b, c, m.width))
  )

  /**
   * All the cut-pasting logic, though for many people they simply
   * use these shortcuts for deleting and don't use paste much at all.
   */
  case class CutPasteFilter() extends TermCore.DelegateFilter{
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
      val start = GUILikeFilters.consumeWord(b, c, 1, 0)
      currentCut = b.slice(c, start)
      (b.take(c) ++ b.drop(start), c)
    }

    def cutWordLeft(b: Vector[Char], c: Int) = {
      val start = GUILikeFilters.consumeWord(b, c, -1, 1)
      currentCut = b.slice(start, c)
      (b.take(start) ++ b.drop(c), start)
    }

    def paste(b: Vector[Char], c: Int) = {
      (b.take(c) ++ currentCut ++ b.drop(c), c + currentCut.length)
    }

    def filter = orElseAll(
      Case(Ctrl('u'))((b, c, m) => cutAllLeft(b, c)),
      Case(Ctrl('k'))((b, c, m) => cutAllRight(b, c)),
      Case(Alt + "d")((b, c, m) => cutWordRight(b, c)),
      Case(Ctrl('w'))((b, c, m) => cutWordLeft(b, c)),
      Case(Ctrl('y'))((b, c, m) => paste(b, c)),
      Case(Alt + "\u007f")((b, c, m) => cutWordLeft(b, c))
    )
  }
  

  /**
   * Provides history navigation up and down, saving the current line. 
   */
  case class HistoryFilter(history: () => Seq[String]) extends TermCore.DelegateFilter{
    var index = -1
    var currentHistory = Vector[Char]()

    def swapInHistory(indexIncrement: Int)(b: Vector[Char], rest: LazyList[Int]): TermState = {
      val newIndex = constrainIndex(index + indexIncrement)
      if (index == -1) currentHistory = b
      index = newIndex
      val h = if (index == -1) currentHistory else history()(index).toVector
      TS(rest, h, h.length)
    }

    def constrainIndex(n: Int): Int = {
      val max = history().length - 1
      if (n < -1) -1 else if (max < n) max else n
    }

    def firstRow(cursor: Int, buffer: Vector[Char], width: Int) = {
      cursor < width && (buffer.indexOf('\n') >= cursor || buffer.indexOf('\n') == -1)
    }
    def lastRow(cursor: Int, buffer: Vector[Char], width: Int) = {
      (buffer.length - cursor) < width && (buffer.lastIndexOf('\n') < cursor || buffer.lastIndexOf('\n') == -1)
    }

    val previousHistory = swapInHistory(1) _
    val nextHistory = swapInHistory(-1) _

    def filter = {
      case TermInfo(TS(p"\u001b[A$rest", b, c), w) if firstRow(c, b, w) =>
        previousHistory(b, rest)
      case TermInfo(TS(p"\u0010$rest", b, c), w) if lastRow(c, b, w) =>
        previousHistory(b, rest)
      case TermInfo(TS(p"\u001b[B$rest", b, c), w) if lastRow(c, b, w) =>
        nextHistory(b, rest)
      case TermInfo(TS(p"\u000e$rest", b, c), w) if firstRow(c, b, w) =>
        nextHistory(b, rest)
    }
  }
}
