package ammonite.terminal

import FilterTools._
import TermCore.DelegateFilter
import LazyList._
import SpecialKeys._
import acyclic.file
/**
 * Filters have hook into the various {Ctrl,Shift,Fn,Alt}x{Up,Down,Left,Right}
 * combination keys, and make them behave similarly as they would on a normal
 * GUI text editor: alt-{left, right} for word movement, hold-down-shift for
 * text selection, etc.
 */
object GUILikeFilters {
  case class SelectionFilter() extends DelegateFilter{
    var mark: Option[Int] = None
    def setMark(c: Int) = {
      if (mark == None) mark = Some(c)
    }
    def filter = orElseAll(

      Case(ShiftUp){(b, c, m) => setMark(c); BasicFilters.moveUp(b, c, m.width)},
      Case(ShiftDown){(b, c, m) => setMark(c); BasicFilters.moveDown(b, c, m.width)},
      Case(ShiftRight){(b, c, m) => setMark(c); (b, c + 1)},
      Case(ShiftLeft){(b, c, m) => setMark(c); (b, c - 1)},
      Case(AltShiftUp){(b, c, m) => setMark(c); BasicFilters.moveUp(b, c, m.width)},
      Case(AltShiftDown){(b, c, m) => setMark(c); BasicFilters.moveDown(b, c, m.width)},
      Case(AltShiftRight){(b, c, m) => setMark(c); wordRight(b, c)},
      Case(AltShiftLeft){(b, c, m) => setMark(c); wordLeft(b, c)},
      Case(FnShiftRight){(b, c, m) => setMark(c); BasicFilters.moveEnd(b, c, m.width)},
      Case(FnShiftLeft){(b, c, m) => setMark(c); BasicFilters.moveStart(b, c, m.width)},
      {
        case TS(9 ~: rest, b, c) if mark.isDefined => // Tab
          val indent = 4
          val markValue = mark.get
          val (chunks, chunkStarts, chunkIndex) = FilterTools.findChunks(b, c)
          val min = chunkStarts.lastIndexWhere(_ < math.min(c, markValue))
          val max = chunkStarts.indexWhere(_ > math.max(c, markValue))
          val splitPoints = chunkStarts.slice(min, max)
          val broken =
            for((Seq(l, r), i) <- (0 +: splitPoints :+ 99999).sliding(2).zipWithIndex)
            yield {
              val padding = if (i == 0) Vector.empty[Char] else Vector.fill(indent)(' ')
              padding ++ b.slice(l, r)
            }
          val deeperOffset = indent * splitPoints.length
          val (newMark, newC) =
            if (markValue > c) (markValue + deeperOffset, c + indent)
            else (markValue + indent, c + deeperOffset)

          mark = Some(newMark)
          TS(rest, broken.flatten.toVector, newC)

        // Intercept every other character. If it's a  printable character,
        // delete the current selection and write the printable character.
        // If it's a special command, just cancel the current selection.
        case TS(char ~: inputs, buffer, cursor) if mark.isDefined =>
          if (char == Alt(0) || char.toChar.isControl) {
            mark = None
            TS(char ~: inputs, buffer, cursor)
          }else{
            val Seq(min, max) = Seq(mark.get, cursor).sorted
            mark = None
            val newBuffer = buffer.take(min) ++ buffer.drop(max)
            val newInputs =
              if (char == 127) inputs
              else char ~: inputs
            TS(newInputs, newBuffer, min)
          }

      }
    )
  }

  val fnFilter = orElseAll(
    Case(FnUp)((b, c, m) => (b, c - 9999)),
    Case(FnDown)((b, c, m) => (b, c + 9999)),
    Case(FnRight)((b, c, m) => BasicFilters.moveEnd(b, c, m.width)),
    Case(FnLeft)((b, c, m) => BasicFilters.moveStart(b, c, m.width))
  )
  val altFilter = orElseAll(
    Case(AltUp){(b, c, m) => BasicFilters.moveUp(b, c, m.width)},
    Case(AltDown){(b, c, m) => BasicFilters.moveDown(b, c, m.width)},
    Case(AltRight){(b, c, m) => wordRight(b, c)},
    Case(AltLeft){(b, c, m) => wordLeft(b, c)}
  )

  val fnAltFilter = orElseAll(
    Case(FnAltUp){(b, c, m) => (b, c)},
    Case(FnAltDown){(b, c, m) => (b, c)},
    Case(FnAltRight){(b, c, m) => (b, c)},
    Case(FnAltLeft){(b, c, m) => (b, c)}
  )
  val fnAltShiftFilter = orElseAll(
    Case(FnAltShiftRight){(b, c, m) => (b, c)},
    Case(FnAltShiftLeft){(b, c, m) => (b, c)}
  )


  def consumeWord(b: Vector[Char], c: Int, delta: Int, offset: Int) = {
    var current = c
    // Move at least one character! Otherwise
    // you get stuck at the end of a word.
    current += delta
    while(b.isDefinedAt(current) && !b(current).isLetterOrDigit) current += delta
    while(b.isDefinedAt(current) && b(current).isLetterOrDigit) current += delta
    current + offset
  }

  def wordLeft(b: Vector[Char], c: Int) = b -> consumeWord(b, c, -1, 1)
  def wordRight(b: Vector[Char], c: Int) = b -> consumeWord(b, c, 1, 0)

}
