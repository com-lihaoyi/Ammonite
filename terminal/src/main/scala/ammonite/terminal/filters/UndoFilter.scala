package ammonite.terminal.filters

import ammonite.terminal.FilterTools._
import ammonite.terminal.LazyList.~:
import ammonite.terminal._

import scala.collection.mutable


abstract class Enum{
  protected[this] def Item[T](constructor: String => T)
                             (implicit i: sourcecode.Name): T = constructor(i.value)
}
sealed class UndoState(override val toString: String)
object UndoState extends Enum{
  val Default, Typing, Deleting, Navigating = Item(new UndoState(_))
}

case class UndoFilter() extends TermCore.DelegateFilter{
  val undoStack = mutable.Buffer(Vector[Char]() -> 0)
  var undoIndex = 0
  /**
    * An enum representing what the user is "currently" doing. Used to
    * collapse sequential actions into one undo step: e.g. 10 plain
    * chars typed becomes 1 undo step, or 10 chars deleted becomes one undo
    * step, but 4 chars typed followed by 3 chars deleted followed by 3 chars
    * typed gets grouped into 3 different undo steps
    */
  var state = UndoState.Default
  def currentUndo = undoStack(undoStack.length - undoIndex - 1)
  def undo(b: Vector[Char], c: Int) = {
    if (undoIndex < undoStack.length - 1) {
      undoIndex += 1
      state = UndoState.Default
    }
    currentUndo
  }
  def redo(b: Vector[Char], c: Int) = {
    if (undoIndex > 0) {
      undoIndex -= 1
      state = UndoState.Default
    }
    currentUndo
  }
  def wrap(bc: (Vector[Char], Int), rest: LazyList[Int], msg: Ansi.Str) = {
    val (b, c) = bc
    TS(rest, b, c, msg)
  }

  def pushUndos(b: Vector[Char], c: Int) = {
    val (lastB, lastC) = currentUndo
    Debug("")
    // Since we don't have access to the `typingFilter` in this code, we
    // instead attempt to reverse-engineer "what happened" to the buffer by
    // comparing the old one with the new.
    //
    // It turns out that it's not that hard to identify the few cases we care
    // about, since they're all result in either 0 or 1 chars being different
    // between old and new buffers.

    val newState =
    // Nothing changed means nothing changed
      if (lastC == c && lastB == b) state
      // if cursor advanced 1, and buffer grew by 1 at the cursor, we're typing
      else if (lastC + 1 == c && lastB == b.patch(c-1, Nil, 1)) UndoState.Typing
      // cursor moved left 1, and buffer lost 1 char at that point, we're deleting
      else if (lastC - 1 == c && lastB.patch(c, Nil, 1) == b) UndoState.Deleting
      // cursor didn't move, and buffer lost 1 char at that point, we're also deleting
      else if (lastC == c && lastB.patch(c - 1, Nil, 1) == b) UndoState.Deleting
      // cursor moved around but buffer didn't change, we're navigating
      else if (lastC != c && lastB == b) UndoState.Navigating
      // otherwise, sit in the "Default" state where every change is recorded.
      else UndoState.Default

    if (state != newState || newState == UndoState.Default && (lastB, lastC) != (b, c)) {
      state = newState
      undoStack.dropRight(undoIndex)
      undoIndex = 0
      undoStack.append(b -> c)
    }else{
      if (undoIndex == 0 && (b, c) != undoStack.last) {
        undoStack(undoStack.length - 1) = (b, c)
      }
    }

    state = newState
  }

  val undoMsg = Console.BLUE + " ...undoing last action, `Alt -` or `Esc -` to redo"
  val redoMsg = Console.BLUE + " ...redoing last action"
  def filter = TermCore.Filter.merge(
    {
      case TS(q ~: rest, b, c, _) =>
        pushUndos(b, c)
        None
    },
    TermCore.Filter{
      case TS(31 ~: rest, b, c, _) => wrap(undo(b, c), rest, undoMsg)
      case TS(27 ~: 45 ~: rest, b, c, _) => wrap(redo(b, c), rest, "")
    }
  )
}