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
                            indexIncrement: Int,
                            skipped: Vector[Char]) = {

      def rec(i: Int): Int = history.lift(i) match{
        case None if i == -1 => -1
        case None => -1
        case Some(s)
          if s.contains(searchTerm)
          && !s.contentEquals(skipped) => i
        case _ => rec(i + indexIncrement)
      }

      val newHistoryIndex = rec(startIndex)
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
    var searchTerm: Option[Vector[Char]] = None

    /**
      * Kicks the HistoryFilter from passive-mode into search-history mode
      */
    def startHistory(b: Vector[Char], rest: LazyList[Int], defaultCursor: Int): TermState = {
      if (b.nonEmpty) searchTerm = Some(b)
      up(rest, defaultCursor, Vector())
    }

    def searchHistory(start: Int,
                      increment: Int,
                      rest: LazyList[Int],
                      defaultCursor: Int,
                      skipped: Vector[Char]) = {
      val sanitizedSearch = searchTerm.getOrElse(Vector())
      val (newHistoryIndex, newBuffer, newCursor) = HistoryFilter.findNewHistoryIndex(
        start,
        sanitizedSearch,
        history(),
        increment,
        skipped
      )
      historyIndex = newHistoryIndex

      historyIndex match {
        case -1 => TS(rest, sanitizedSearch, sanitizedSearch.length )
        case i => TS(rest, newBuffer, if (sanitizedSearch.isEmpty) defaultCursor else newCursor)
      }
    }

    def activeHistory = searchTerm.nonEmpty || historyIndex != -1
    def activeSearch = searchTerm.nonEmpty
    def up(rest: LazyList[Int], c: Int, skipped: Vector[Char]) = {
      searchHistory(historyIndex + 1, 1, rest, c, skipped)
    }
    def down(rest: LazyList[Int], c: Int, skipped: Vector[Char]) = {
      searchHistory(historyIndex - 1, -1, rest, c, skipped)
    }
    def filter = {
      // Ways to kick off the history search if you're not already in it
      case TS(18 ~: rest, b, c) =>
        if (activeSearch) up(rest, c, b)
        else {
          searchTerm = Some(b)
          startHistory(b, rest, c)
        }
      // Ways to kick off the history search if you're not already in it
      case TermInfo(TS(p"\u001b[A$rest", b, c), w) if firstRow(c, b, w) && !activeHistory =>
        startHistory(b, rest, c)

      case TermInfo(TS(p"\u0010$rest", b, c), w) if lastRow(c, b, w) && !activeHistory =>
        startHistory(b, rest, c)

      // Things you can do when you're already in the history search

      // Navigating up and down the history. Each up or down searches for
      // the next thing that matches your current searchTerm
      case TS(p"\u001b[A$rest", b, c) if activeHistory => up(rest, c, b)
      case TS(p"\u0010$rest", b, c) if activeHistory   => up(rest, c, b)
      case TS(p"\u001b[B$rest", b, c) if activeHistory => down(rest, c, b)
      case TS(p"\u000e$rest", b, c) if activeHistory   => down(rest, c, b)

      // Intercept Backspace and delete a character, preserving search
      case TS(127 ~: inputs, buffer, cursor) if activeSearch =>
        searchTerm = searchTerm.map(_.dropRight(1))
        searchHistory(historyIndex, 1, inputs, cursor - 1, buffer)

      // Intercept Enter other control characters and drop
      // out of search, forwarding that character downstream
      case TS(char ~: inputs, buffer, cursor) if activeSearch && char.toChar.isControl =>
        val current =
          if (historyIndex == -1) buffer
          else history()(historyIndex).toVector
        historyIndex = -1
        searchTerm = None
        TS(char ~: inputs, current, cursor)

      // Intercept every other printable character.
      case TS(char ~: inputs, buffer, cursor) if activeSearch =>

        searchTerm = searchTerm.map(_ :+ char.toChar)
        searchHistory(historyIndex.max(0), 1, inputs, cursor, Vector())
    }
  }
}
