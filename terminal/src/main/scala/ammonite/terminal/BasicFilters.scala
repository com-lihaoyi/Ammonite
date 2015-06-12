package ammonite.terminal
import acyclic.file
import ammonite.terminal.LazyList._
import FilterTools._

/**
 * Filters for simple operation of a terminal: cursor-navigation
 * (including with all the modifier keys), enter/ctrl-c-exit, etc.
 */
object BasicFilters {
  val default = {
    advancedNavFilter orElse
    navFilter orElse
    exitFilter orElse
    enterFilter orElse
    loggingFilter orElse
    typingFilter
  }

  val multilineFilter: TermCore.Filter = {
    case TS(13 ~: rest, b, c) =>
      val open = b.count(_ == '(')
      val close = b.count(_ == ')')
      Debug(open + "\t" + close)
      if (open == close) Result(b.mkString)
      else {
        val (first, last) = b.splitAt(c)
        TermState(rest, (first :+ '\n') ++ last, c + 1)
      }
  }

  lazy val navFilter = orElseAll(
    Case(Alt+"[A")((b, c, m) => moveUp(b, c, m.width)),
    Case(Alt+"[B")((b, c, m) => moveDown(b, c, m.width)),
    Case(Alt+"[C")((b, c, m) => (b, c + 1)),
    Case(Alt+"[D")((b, c, m) => (b, c - 1)),
    Case(Alt+"[5~")((b, c, m) => (b, c - 9999)),
    Case(Alt+"[6~")((b, c, m) => (b, c + 9999)),
    Case(Alt+"[F")((b, c, m) => moveEnd(b, c, m.width)),
    Case(Alt+"[H")((b, c, m) => moveStart(b, c, m.width))
  )

  lazy val advancedNavFilter = orElseAll(
    Case(Alt*2+"[A"){(b, c, m) => Debug("alt-up"); (b, c)},
    Case(Alt*2+"[B"){(b, c, m) => Debug("alt-down"); (b, c)},
    Case(Alt*2+"[C"){(b, c, m) => Debug("alt-right"); (b, c)},
    Case(Alt*2+"[D"){(b, c, m) => Debug("alt-left"); (b, c)},

    Case(Alt+"[1;2A"){(b, c, m) => Debug("shift-up"); (b, c)},
    Case(Alt+"[1;2B"){(b, c, m) => Debug("shift-down"); (b, c)},
    Case(Alt+"[1;2C"){(b, c, m) => Debug("shift-right"); (b, c)},
    Case(Alt+"[1;2D"){(b, c, m) => Debug("shift-left"); (b, c)},

    Case(Alt*2+"[5~"){(b, c, m) => Debug("fn-alt-up"); (b, c)},
    Case(Alt*2+"[6~"){(b, c, m) => Debug("fn-alt-down"); (b, c)},
    Case(Alt+"[1;9F"){(b, c, m) => Debug("fn-alt-right"); (b, c)},
    Case(Alt+"[1;9H"){(b, c, m) => Debug("fn-alt-left"); (b, c)},

    Case(Alt*2+"[5~"){(b, c, m) => Debug("fn-alt-up"); (b, c)},
    Case(Alt*2+"[6~"){(b, c, m) => Debug("fn-alt-down"); (b, c)},
    Case(Alt+"[1;2F"){(b, c, m) => Debug("fn-shift-right"); (b, c)},
    Case(Alt+"[1;2H"){(b, c, m) => Debug("fn-shift-left"); (b, c)},

    Case(Alt+"[1;10A"){(b, c, m) => Debug("alt-shift-up"); (b, c)},
    Case(Alt+"[1;10B"){(b, c, m) => Debug("alt-shift-down"); (b, c)},
    Case(Alt+"[1;10C"){(b, c, m) => Debug("alt-shift-right"); (b, c)},
    Case(Alt+"[1;10D"){(b, c, m) => Debug("alt-shift-left"); (b, c)},

    // Same as the case fn-alt-{up,down} without the shift
    Case(Alt*2+"[5~"){(b, c, m) => Debug("fn-alt-shift-up"); (b, c)},
    Case(Alt*2+"[6~"){(b, c, m) => Debug("fn-alt-shift-down"); (b, c)},
    Case(Alt+"[1;10F"){(b, c, m) => Debug("fn-alt-shift-right"); (b, c)},
    Case(Alt+"[1;10H"){(b, c, m) => Debug("fn-alt-shift-left"); (b, c)}
  )


  lazy val loggingFilter: TermCore.Filter = {
    case TS(Ctrl('t') ~: rest, b, c) =>
      println("Char Display Mode Enabled! Ctrl-C to exit")
      var curr = rest
      while (curr.head != 3) {
        println("Char " + curr.head)
        curr = curr.tail
      }
      TS(curr, b, c)
  }
  lazy val typingFilter: TermCore.Filter = {
    case TS(p"\u001b[3~$rest", b, c) =>
      Debug("fn-delete")
      val (first, last) = b.splitAt(c)
      TS(rest, first ++ last.drop(1), c)

    case TS(127 ~: rest, b, c) => // Backspace
      val (first, last) = b.splitAt(c)
      TS(rest, first.dropRight(1) ++ last, c - 1)

    case TS(char ~: rest, b, c) =>
      Debug("NORMAL CHAR " + char)
      val (first, last) = b.splitAt(c)
      TS(rest, (first :+ char.toChar) ++ last, c + 1)
  }

  lazy val enterFilter: TermCore.Filter = {
    case TS(13 ~: rest, b, c) => // Enter
      Result(b.mkString)
  }
  lazy val exitFilter: TermCore.Filter = {
    case TS(Ctrl('c') ~: rest, b, c) =>
      TS(rest, Vector.empty, 0)
    case TS(Ctrl('d') ~: rest, b, c) => Exit
  }

  def moveStart(b: Vector[Char],
                c: Int,
                w: Int) = {
    val (chunks, chunkStarts, chunkIndex) = findChunks(b, c)
    val currentColumn = (c - chunkStarts(chunkIndex)) % w
    b -> (c - currentColumn)

  }
  def moveEnd(b: Vector[Char],
              c: Int,
              w: Int) = {
    val (chunks, chunkStarts, chunkIndex) = findChunks(b, c)
    val currentColumn = (c - chunkStarts(chunkIndex)) % w
    val c1 = chunks.lift(chunkIndex + 1) match{
      case Some(next) =>
        val boundary = chunkStarts(chunkIndex + 1) - 1
        if ((boundary - c) > (w - currentColumn)) {
          val delta= w - currentColumn
          c + delta
        }
        else boundary
      case None =>
        c + 1 * 9999
    }
    b -> c1
  }


  def moveUpDown(b: Vector[Char],
                 c: Int,
                 w: Int,
                 boundaryOffset: Int,
                 nextChunkOffset: Int,
                 checkRes: Int,
                 check: (Int, Int) => Boolean,
                 isDown: Boolean) = {
    val (chunks, chunkStarts, chunkIndex) = findChunks(b, c)
    val offset = chunkStarts(chunkIndex + boundaryOffset)
    if (check(checkRes, offset)) checkRes
    else chunks.lift(chunkIndex + nextChunkOffset) match{
      case None => c + nextChunkOffset * 9999
      case Some(next) =>
        val boundary = chunkStarts(chunkIndex + boundaryOffset)
        val currentColumn = (c - chunkStarts(chunkIndex)) % w

        if (isDown) boundary + math.min(currentColumn, next)
        else boundary + math.min(currentColumn - next % w, 0) - 1
    }
  }
  def moveUp(b: Vector[Char], c: Int, w: Int) = {
    b -> moveUpDown(b, c, w, 0, -1, c - w, _ > _, false)
  }
  def moveDown(b: Vector[Char], c: Int, w: Int) = {
    b -> moveUpDown(b, c, w, 1, 1, c + w, _ <= _, true)
  }
}
