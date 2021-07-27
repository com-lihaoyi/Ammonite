package ammonite.terminal.filters

import ammonite.terminal.FilterTools._
import ammonite.terminal.LazyList._
import ammonite.terminal.SpecialKeys.Ctrl
import ammonite.terminal._

import Filter._
/**
  * Provides history navigation up and down, saving the current line, a well
  * as history-search functionality (`Ctrl R` in bash) letting you quickly find
  * & filter previous commands by entering a sub-string.
  */
class HistoryFilter(history: () => IndexedSeq[String],
                    commentStartColor: fansi.Attrs) extends DelegateFilter{

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
    * Records the last buffer that the filter has observed while it's in
    * search/history mode. If the new buffer differs from this, assume that
    * some other filter modified the buffer and drop out of search/history
    */
  var prevBuffer: Option[Vector[Char]] = None

  /**
    * Kicks the HistoryFilter from passive-mode into search-history mode
    */
  def startHistory(b: Vector[Char], c: Int): (Vector[Char], Int, String) = {
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
    val (newHistoryIndex, newBuffer, newMsg, newCursor) = searchTerm match{
      // We're not searching for anything, just browsing history.
      // Pass in Vector.empty so we scroll through all items
      case None =>
        val (i, b, c) = nextHistoryIndexFor(Vector.empty)
        (i, b, "", 99999)

      // We're searching for some item with a particular search term
      case Some(b) if b.nonEmpty =>
        val (i, b1, c) = nextHistoryIndexFor(b)

        val msg =
          if (i.nonEmpty) ""
          else commentStartColor(HistoryFilter.cannotFindSearchMessage).render

        (i, b1, msg, c)

      // We're searching for nothing in particular; in this case,
      // show a help message instead of an unhelpful, empty buffer
      case Some(b) if b.isEmpty =>
        val msg = commentStartColor(HistoryFilter.emptySearchMessage).render
        // The cursor in this case always goes to zero
        (Some(start), Vector(), msg, 0)

    }

    historyIndex = newHistoryIndex.getOrElse(-1)

    (newBuffer, newCursor, newMsg)
  }

  def activeHistory = searchTerm.nonEmpty || historyIndex != -1
  def activeSearch = searchTerm.nonEmpty
  def up(b: Vector[Char], c: Int) = {
    searchHistory(historyIndex + 1, 1, b, b)
  }
  def down(b: Vector[Char], c: Int) = {
    searchHistory(historyIndex - 1, -1, b, b)
  }
  def wrap(rest: LazyList[Int], out: (Vector[Char], Int, String)) = {
    TS(rest, out._1, out._2, out._3)
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

  /**
    * Predicate to check if either we're searching for a term or if we're in
    * history-browsing mode and some predicate is true.
    *
    * Very often we want to capture keystrokes in search-mode more aggressively
    * than in history-mode, e.g. search-mode drops you out more aggressively
    * than history-mode does, and its up/down keys cycle through history more
    * aggressively on every keystroke while history-mode only cycles when you
    * reach the top/bottom line of the multi-line input.
    */
  def searchOrHistoryAnd(cond: Boolean) = {
    activeSearch || (activeHistory && cond)
  }

  val dropHistoryChars = Set(9, 13, 10) // Tab or Enter

  def endHistory() = {
    historyIndex = -1
    searchTerm = None
  }

  def filter = Filter.wrap{
    (ti: TermInfo) => {
      prelude.op(ti) match {
        case None =>
          prevBuffer = Some(ti.ts.buffer)
          filter0.op(ti) match{
            case Some(ts: TermState) =>
              prevBuffer = Some(ts.buffer)
              Some(ts)
            case x => x
          }
        case some => some
      }
    }
  }

  def prelude: Filter = partial{
    case TS(inputs, b, c, _) if activeHistory && prevBuffer.exists(_ != b) =>
      endHistory()
      prevBuffer = None
      TS(inputs, b, c)
  }


  def filter0: Filter = Filter.merge(
    // Ways to kick off the history/search if you're not already in it
    action(Ctrl('r'))(
      ts => wrap(ts.inputs, ctrlR(ts.buffer, ts.cursor))
    ),
    action(SpecialKeys.Up, ti => firstRowInfo(ti) && !activeHistory)(
      ts => wrap(ts.inputs, startHistory(ts.buffer, ts.cursor))
    ),
    action(Ctrl('p'), ti => firstRowInfo(ti) && !activeHistory)(
      ts => wrap(ts.inputs, startHistory(ts.buffer, ts.cursor))
    ),
    // `Page-Up` from first character starts history
    action(SpecialKeys.FnUp, ti => ti.ts.cursor == 0 && !activeHistory)(
      ts => wrap(ts.inputs, startHistory(ts.buffer, ts.cursor))
    ),
    // Things you can do when you're already in the history search

    // Navigating up and down the history. Each up or down searches for
    // the next thing that matches your current searchTerm
    action(
      SpecialKeys.Up,
      ti => searchOrHistoryAnd(firstRowInfo(ti)))(
      ts => wrap(ts.inputs, up(ts.buffer, ts.cursor))
    ),
    action(
      Ctrl('p'),
      ti => searchOrHistoryAnd(firstRowInfo(ti)))(
      ts => wrap(ts.inputs, up(ts.buffer, ts.cursor))
    ),
    action(
      SpecialKeys.FnUp, // Page Up from first character cycles history up
      ti => searchOrHistoryAnd(ti.ts.cursor == 0))(
      ts => wrap(ts.inputs, up(ts.buffer, ts.cursor))
    ),
    action(
      SpecialKeys.Down,
      ti => searchOrHistoryAnd(lastRow(ti.ts.cursor, ti.ts.buffer, ti.width)))(
      ts => wrap(ts.inputs, down(ts.buffer, ts.cursor))
    ),
    action(
      Ctrl('n'),
      ti => searchOrHistoryAnd(lastRow(ti.ts.cursor, ti.ts.buffer, ti.width)))(
      ts => wrap(ts.inputs, down(ts.buffer, ts.cursor))
    ),
    action(  // `Page-Down` from last character cycles history down
      SpecialKeys.FnDown,
      ti => searchOrHistoryAnd(ti.ts.cursor == ti.ts.buffer.length))(
      ts => wrap(ts.inputs, down(ts.buffer, ts.cursor))
    ),
    // Intercept Backspace and delete a character in search-mode, preserving it, but
    // letting it fall through and dropping you out of history-mode if you try to make
    // edits
    action(SpecialKeys.Backspace, ti => activeSearch)(
      ts => wrap(ts.inputs, backspace(ts.buffer, ts.cursor))
    ),
    partial{
      // Any other control characters drop you out of search mode, but only the
      // set of `dropHistoryChars` drops you out of history mode
      case TS(char ~: inputs, buffer, cursor, _)
        if char.toChar.isControl && searchOrHistoryAnd(dropHistoryChars(char)) =>
        val newBuffer =
        // If we're back to -1, it means we've wrapped around and are
        // displaying the original search term with a wrap-around message
        // in the terminal. Drop the message and just preserve the search term
          if (historyIndex == -1) searchTerm.get
          // If we're searching for an empty string, special-case this and return
          // an empty buffer rather than the first history item (which would be
          // the default) because that wouldn't make much sense
          else if (searchTerm.exists(_.isEmpty)) Vector()
          // Otherwise, pick whatever history entry we're at and use that
          else history()(historyIndex).toVector
        endHistory()

        TS(char ~: inputs, newBuffer, cursor)

      // Intercept every other printable character when search is on and
      // enter it into the current search
      case TS(char ~: rest, buffer, cursor, _) if activeSearch =>
        wrap(rest, printableChar(char.toChar)(buffer, cursor))

      // If you're not in search but are in history, entering any printable
      // characters kicks you out of it and preserves the current buffer. This
      // makes it harder for you to accidentally lose work due to history-moves
      case TS(char ~: rest, buffer, cursor, _) if activeHistory && !char.toChar.isControl =>
        historyIndex = -1
        TS(char ~: rest, buffer, cursor)
    }
  )
}

object HistoryFilter{


  def mangleBuffer(historyFilter: HistoryFilter,
                   buffer: fansi.Str,
                   cursor: Int,
                   startColor: fansi.Attr) = {
    if (!historyFilter.activeSearch) buffer
    else {
      val (searchStart, searchEnd) =
        if (historyFilter.searchTerm.get.isEmpty) (cursor, cursor+1)
        else {
          val start = buffer.plainText.indexOfSlice(historyFilter.searchTerm.get)

          val end = start + (historyFilter.searchTerm.get.length max 1)
          (start, end)
        }


      val newStr = buffer.overlay(startColor, searchStart, math.min(buffer.length, searchEnd))
      newStr
    }

  }
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
    val searchTermStr = new String(searchTerm.toArray)
    val skippedStr = new String(skipped.toArray)
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
      case Some(s) if s.contains(searchTermStr) && !s.contentEquals(skippedStr) =>
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
    s" ...enter the string to search for, then `up` for more"
  val cannotFindSearchMessage =
    s" ...can't be found in history; re-starting search"


}
