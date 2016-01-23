package ammonite.terminal
import acyclic.file
import scala.collection.mutable
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
    Case(HomeScreen)((b, c, m) => BasicFilters.moveStart(b, c, m.width)), // <- one line
    Case(Ctrl('a'))((b, c, m) => BasicFilters.moveStart(b, c, m.width)),
    Case(End)((b, c, m) => BasicFilters.moveEnd(b, c, m.width)), // -> one line
    Case(EndScreen)((b, c, m) => BasicFilters.moveEnd(b, c, m.width)), // -> one line
    Case(Ctrl('e'))((b, c, m) => BasicFilters.moveEnd(b, c, m.width))
  )

  /**
   * All the cut-pasting logic, though for many people they simply
   * use these shortcuts for deleting and don't use paste much at all.
   */
  case class CutPasteFilter() extends TermCore.DelegateFilter{
    var currentCut = Vector.empty[Char]
    def cutCharLeft(b: Vector[Char], c: Int) = {
      /* Do not edit current cut. Zsh(zle) & Bash(readline) do not edit the yank ring for Ctrl-h */
      (b patch(from = c - 1, patch = Nil, replaced = 1), c - 1)
    }

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
      Case(Ctrl('h'))((b, c, m) => cutCharLeft(b, c)),
      Case(Ctrl('y'))((b, c, m) => paste(b, c)),
      Case(Alt + "\u007f")((b, c, m) => cutWordLeft(b, c))
    )
  }

  /**
   * Provides history navigation up and down, saving the current line.
   *
   * Up/down will only list commands that share prefix with the last manually
   * edited line.
   */
  case class HistoryFilter(history: () => Seq[String]) extends TermCore.DelegateFilter {
    // The index of the currently active line.
    var index = 0
    // The command in front of history. Has not been executed yet.
    var currentCommand = Vector[Char]()
    // The prefix we search by while looking up/down.
    var currentPrefix = Vector[Char]()
    // The last command displayed to the user by this HistoryFilter.
    var lastCommand = Vector[Char]()
    // Did the user press up last time? Used to deduplicate results.
    var lastUp = false
    // Which commands has the user seen. Used to deduplicate results.
    val visited = mutable.Set[String]()

    val commands = mutable.Map.empty[Int, String]

    def swapInHistory(up: Boolean)
                     (b: Vector[Char], rest: LazyList[Int]): TermState = {
      if (lastUp != up) {
        visited.clear() // User switched direction.
        lastUp = up
      }
      // Exclude active line from search results, user wants something else.
      val cmd = b.mkString
      if (index == 0) commands += index -> cmd
      if (b.nonEmpty && b != lastCommand) { // The user manually edited b.
        commands += index -> cmd
        currentPrefix = b
      }
      val command = nextCommand(up)
      lastCommand = command._1.toVector
      index = command._2
      // TODO(olafur) how do I underline prefix??
      TS(rest, lastCommand, lastCommand.length)
    }

    def getCommands: Seq[(String, Int)] = {
      (commands(0) +: history()).zipWithIndex.map {
        case (cmd, i) =>
          commands.getOrElse(i, cmd) -> i
      }
    }

    def nextCommand(up: Boolean): (String, Int) = {
      val allCommands = getCommands
      visited += allCommands(index)._1
      val candidateCommands = {
        if (up) allCommands.drop(index + 1)
        else allCommands.take(index).reverse
      }
      val prefix = currentPrefix.mkString
      candidateCommands.find {
        case (cmd, _) if !visited.contains(cmd) =>
          visited += cmd
          cmd.startsWith(prefix)
        case _ => false
      }.getOrElse {
        allCommands(index)
      }
    }

    val previousHistory = swapInHistory(up = true) _
    val nextHistory = swapInHistory(up = false) _

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

