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
    Case(LinuxCtrlLeft)((b, c, m) => GUILikeFilters.wordLeft(b, c)), // <- one word
    Case(Alt + "f")((b, c, m) => GUILikeFilters.wordRight(b, c)), // -> one  word
    Case(LinuxCtrlRight)((b, c, m) => GUILikeFilters.wordRight(b, c)), // -> one word
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

  object HistoryFilter{
    /**
      * @param startIndex The first index to start looking from
      * @param searchTerm The term we're searching from; can be empty
      * @param history The history we're searching through
      * @param indexIncrement Which direction to search, +1 or -1
      */
    def findNewHistoryIndex(startIndex: Int,
                            searchTerm: Vector[Char],
                            history: IndexedSeq[String],
                            indexIncrement: Int) = {

      def rec(i: Int): Option[Int] = history.lift(i) match{
        case None if i == -1 => Some(-1)
        case None => None
        case Some(s)
          if s.contains(searchTerm) && !s.contentEquals(searchTerm) => Some(i)
        case _ => rec(i + indexIncrement)
      }

      val newHistoryIndex = rec(startIndex).getOrElse(-1)
      val newBuffer =
        if (newHistoryIndex == -1) searchTerm
        else history(newHistoryIndex).toVector

      val newCursor =
        if (newHistoryIndex == -1) newBuffer.length
        else history(newHistoryIndex).indexOfSlice(searchTerm) + searchTerm.length

      (newHistoryIndex, newBuffer, newCursor)
    }
  }
  /**
   * Provides history navigation up and down, saving the current line.
   */
  case class HistoryFilter(history: () => IndexedSeq[String]) extends TermCore.DelegateFilter{
    /**
      * `-1` means we haven't started looking at history, `n >= 0` means we're
      * currently at history command `n`
      */
    var historyIndex = -1

    /**
      * The term we're searching for, if any.
      */
    var searchTerm = Vector.empty[Char]

    /**
      * Kicks the HistoryFilter from passive-mode into search-history mode
      */
    def startHistory(b: Vector[Char], rest: LazyList[Int]): TermState = {
      searchTerm = b
      searchHistory(0, 1, rest)
    }

    def searchHistory(start: Int, increment: Int, rest: LazyList[Int]) = {
      val (newHistoryIndex, newBuffer, newCursor) = HistoryFilter.findNewHistoryIndex(
        start,
        searchTerm,
        history(),
        increment
      )

      historyIndex = newHistoryIndex
      historyIndex match {
        case -1 => TS(rest, searchTerm, searchTerm.length)
        case i => TS(rest, newBuffer, newCursor)
      }
    }

    def filter = orElseAll(
      {
        // Ways to kick off the history search if you're not already in it
        case TermInfo(TS(p"\u001b[A$rest", b, c), w)
          if firstRow(c, b, w) && historyIndex == -1 =>
          startHistory(b, rest)
        case TermInfo(TS(p"\u0010$rest", b, c), w)
          if lastRow(c, b, w) && historyIndex == -1 =>
          startHistory(b, rest)

        // Things you can do when you're already in the history search

        // Navigating up and down the history. Each up or down searches for
        // the next thing that matches your current searchTerm
        case TermInfo(TS(p"\u001b[A$rest", b, c), w) if historyIndex != -1 =>
          Debug("Up\t" + historyIndex)
          searchHistory(historyIndex + 1, 1, rest)

        case TermInfo(TS(p"\u0010$rest", b, c), w) if historyIndex != -1 =>
          searchHistory(historyIndex + 1, 1, rest)

        case TermInfo(TS(p"\u001b[B$rest", b, c), w) if historyIndex != -1 =>
          Debug("Down\t" + historyIndex)
          searchHistory(historyIndex - 1, -1, rest)

        case TermInfo(TS(p"\u000e$rest", b, c), w) if historyIndex != -1  =>
          searchHistory(historyIndex - 1, -1, rest)


        // Intercept Backspace and delete a character, preserving search
        case TS(127 ~: inputs, buffer, cursor) if historyIndex != -1 =>
          searchTerm = searchTerm.dropRight(1)
          searchHistory(historyIndex, 1, inputs)

        // Intercept Enter and submit the currently-searched command for evaluation
        case TS(char ~: inputs, buffer, cursor)
          if historyIndex != -1 && char == 13 || char == 10 =>
          val current = history()(historyIndex).toVector
          historyIndex = -1
          TS(char ~: inputs, current, cursor)

        // Intercept other control characters and drop out of search
        case TS(char ~: inputs, buffer, cursor)
          if historyIndex != -1 && char.toChar.isControl =>
          val current = history()(historyIndex).toVector
          historyIndex = -1
          TS(char ~: inputs, current, cursor)

        // Intercept every other printable character.
        case TS(char ~: inputs, buffer, cursor)
          if historyIndex != -1 =>
          searchTerm = searchTerm :+ char.toChar
          searchHistory(historyIndex, 1, inputs)
      }
    )
  }
}
