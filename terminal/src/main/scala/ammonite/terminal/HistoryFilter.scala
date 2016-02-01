package ammonite.terminal

import ammonite.terminal.FilterTools._
import ammonite.terminal.LazyList._


object HistoryFilter{
  /**
    * @param startIndex The first index to start looking from
    * @param searchTerm The term we're searching from; can be empty
    * @param history The history we're searching through
    * @param indexIncrement Which direction to search, +1 or -1
    * @param skipped Any buffers which we should skip in our search results,
    *                e.g. because the user has seen them before.
    */
  def findNewHistoryIndex(startIndex: Int,
                          searchTerm: Vector[Char],
                          history: IndexedSeq[String],
                          indexIncrement: Int,
                          skipped: Vector[Char]) = {
    /**
      * `Some(i)` means we found a reasonable result at history element `i`
      * `None` means we couldn't find anything, and should show a not-found
      * error to the user
      */
    def rec(i: Int): Option[Int] = history.lift(i) match{
      // If i < 0, it means the user is pressing `down` too many times, which
      // means it doesn't show anything but we shouldn't show an error
      case None if i < 0 => Some(-1)
      case None => None
      case Some(s) if s.contains(searchTerm) && !s.contentEquals(skipped) =>
        Some(i)
      case _ => rec(i + indexIncrement)
    }

    val newHistoryIndex = rec(startIndex)
    val foundIndex = newHistoryIndex.find(_ != -1)
    val newBuffer = foundIndex match{
      case None => searchTerm
      case Some(i) => history(i).toVector
    }

    val newCursor = foundIndex match{
      case None => newBuffer.length
      case Some(i) => history(i).indexOfSlice(searchTerm) + searchTerm.length
    }

    (newHistoryIndex, newBuffer, newCursor)
  }
  val emptySearchMessage =
    s" ${Console.BLUE}...enter the string to search for, then `up` for more${Console.RESET}"
  val cannotFindSearchMessage =
    s" ${Console.BLUE}...can't be found in history; re-starting search${Console.RESET}"
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
    *
    * - `None` means we're not searching for anything, e.g. we're just
    *   browsing history
    *
    * - `Some(term)` where `term` is not empty is what it normally looks
    *   like when we're searching for something
    *
    * - `Some(term)` where `term` is empty only really happens when you
    *   start searching and delete things, or if you `Ctrl-R` on an empty
    *   prompt
    */
  var searchTerm: Option[Vector[Char]] = None

  /**
    * Kicks the HistoryFilter from passive-mode into search-history mode
    */
  def startHistory(b: Vector[Char], c: Int): (Vector[Char], Int) = {
    if (b.nonEmpty) searchTerm = Some(b)
    up(Vector(), c)
  }

  def searchHistory(start: Int,
                    increment: Int,
                    buffer: Vector[Char],
                    skipped: Vector[Char]) = {

    def nextHistoryIndexFor(v: Vector[Char]) = {
      HistoryFilter.findNewHistoryIndex(start, v, history(), increment, skipped)
    }

    val (newHistoryIndex, newBuffer, newCursor) = searchTerm match{
      // We're not searching for anything, just browsing history.
      // Pass in Vector.empty so we scroll through all items
      case None =>
        val (i, b, c) = nextHistoryIndexFor(Vector.empty)
        (i, b, 99999)

      // We're searching for some item with a particular search term
      case Some(b) if b.nonEmpty =>
        val (i, b1, c) = nextHistoryIndexFor(b)

        val msg =
          if (i.nonEmpty) ""
          else HistoryFilter.cannotFindSearchMessage

        (i, b1 ++ msg, c)

      // We're searching for nothing in particular; in this case,
      // show a help message instead of an unhelpful, empty buffer
      case Some(b) if b.isEmpty =>
        val msg = HistoryFilter.emptySearchMessage.toVector
        // The cursor in this case always goes to zero
        (Some(start), msg, 0)

    }

    historyIndex = newHistoryIndex.getOrElse(-1)

    (newBuffer, newCursor)
  }

  def activeHistory = searchTerm.nonEmpty || historyIndex != -1
  def activeSearch = searchTerm.nonEmpty
  def up(b: Vector[Char], c: Int) = {
    searchHistory(historyIndex + 1, 1, b, b)
  }
  def down(b: Vector[Char], c: Int) = {
    searchHistory(historyIndex - 1, -1, b, b)
  }
  def wrap(rest: LazyList[Int], out: (Vector[Char], Int)) = {
    TS(rest, out._1, out._2)
  }
  def ctrlR(b: Vector[Char], c: Int) = {
    if (activeSearch) up(b, c)
    else {
      searchTerm = Some(b)
      up(Vector(), c)
    }
  }
  def printableChar(char: Char)(b: Vector[Char], c: Int) = {
    searchTerm = searchTerm.map(_ :+ char)
    searchHistory(historyIndex.max(0), 1, b :+ char, Vector())
  }

  def backspace(b: Vector[Char], c: Int) = {
    searchTerm = searchTerm.map(_.dropRight(1))
    searchHistory(historyIndex, 1, b, Vector())
  }

  def filter = {
    // Ways to kick off the history search if you're not already in it
    case TS(18 ~: rest, b, c) => wrap(rest, ctrlR(b, c))

    // Ways to kick off the history search if you're not already in it
    case TermInfo(TS(p"\u001b[A$rest", b, c), w) if firstRow(c, b, w) && !activeHistory =>
      wrap(rest, startHistory(b, c))

    case TermInfo(TS(p"\u0010$rest", b, c), w) if lastRow(c, b, w) && !activeHistory =>
      wrap(rest, startHistory(b, c))

    // Things you can do when you're already in the history search

    // Navigating up and down the history. Each up or down searches for
    // the next thing that matches your current searchTerm
    case TS(p"\u001b[A$rest", b, c) if activeHistory => wrap(rest, up(b, c))
    case TS(p"\u0010$rest", b, c) if activeHistory   => wrap(rest, up(b, c))
    case TS(p"\u001b[B$rest", b, c) if activeHistory => wrap(rest, down(b, c))
    case TS(p"\u000e$rest", b, c) if activeHistory   => wrap(rest, down(b, c))

    // Intercept Backspace and delete a character, preserving search
    case TS(127 ~: rest, buffer, cursor) if activeSearch =>
      wrap(rest, backspace(buffer, cursor))

    // Intercept Enter other control characters and drop
    // out of history, forwarding that character downstream
    case TS(char ~: inputs, buffer, cursor) if activeSearch && char.toChar.isControl =>
      val newBuffer =
        // If we're back to -1, it means we've wrapped around and are
        // displaying the original search term with a wrap-around message
        // in the terminal. Drop the message and just preserve the search term
        if (historyIndex == -1) searchTerm.get
        // Otherwise, pick whatever history entry we're at and use that
        else history()(historyIndex).toVector
      historyIndex = -1
      searchTerm = None
      TS(char ~: inputs, newBuffer, cursor)

    // Intercept every other printable character when search is on and
    // enter it into the current search
    case TS(char ~: rest, buffer, cursor) if activeSearch =>
      wrap(rest, printableChar(char.toChar)(buffer, cursor))

    // If you're not in search but are in history, entering any printable
    // characters kicks you out of it and preserves the current buffer. This
    // makes it harder for you to accidentally lose work due to history-moves
    case TS(char ~: rest, buffer, cursor) if activeHistory && !char.toChar.isControl =>
      historyIndex = -1
      TS(char ~: rest, buffer, cursor)
  }
}