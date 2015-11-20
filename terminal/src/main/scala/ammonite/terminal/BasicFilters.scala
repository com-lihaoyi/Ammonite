package ammonite.terminal
import acyclic.file
import ammonite.terminal.LazyList._
import FilterTools._
import SpecialKeys._
import ammonite.terminal.TermCore.Filter

/**
 * Filters for simple operation of a terminal: cursor-navigation
 * (including with all the modifier keys), enter/ctrl-c-exit, etc.
 */
object BasicFilters {
  def all = {
    multilineFilter orElse
    navFilter orElse
    exitFilter orElse
    enterFilter orElse
    clearFilter orElse
    loggingFilter orElse
    typingFilter
  }

  def injectNewLine(b: Vector[Char], c: Int, rest: LazyList[Int]) = {
    val (first, last) = b.splitAt(c)
    TermState(rest, (first :+ '\n') ++ last, c + 1)
  }
  def multilineFilter: TermCore.Filter = {
    case TS(13 ~: rest, b, c) if b.count(_ == '(') != b.count(_ == ')') =>
      injectNewLine(b, c, rest)
  }

  def navFilter = orElseAll(
    Case(Up)((b, c, m) => moveUp(b, c, m.width)),
    Case(Down)((b, c, m) => moveDown(b, c, m.width)),
    Case(Right)((b, c, m) => (b, c + 1)),
    Case(Left)((b, c, m) => (b, c - 1))
  )

  def tabColumn(indent: Int, b: Vector[Char], c: Int, rest: LazyList[Int]) = {
    val (chunks, chunkStarts, chunkIndex) = FilterTools.findChunks(b, c)
    val chunkCol = c - chunkStarts(chunkIndex)
    val spacesToInject = indent - (chunkCol % indent)
    val (lhs, rhs) = b.splitAt(c)
    TS(rest, lhs ++ Vector.fill(spacesToInject)(' ') ++ rhs, c + spacesToInject)
  }

  def tabFilter(indent: Int): Filter = {
    case TS(9 ~: rest, b, c) => tabColumn(indent, b, c, rest)
  }

  def loggingFilter: TermCore.Filter = {
    case TS(Ctrl('t') ~: rest, b, c) =>
      println("Char Display Mode Enabled! Ctrl-C to exit")
      var curr = rest
      while (curr.head != 3) {
        println("Char " + curr.head)
        curr = curr.tail
      }
      TS(curr, b, c)
  }
  def typingFilter: TermCore.Filter = {
    case TS(p"\u001b[3~$rest", b, c) =>
//      Debug("fn-delete")
      val (first, last) = b.splitAt(c)
      TS(rest, first ++ last.drop(1), c)

    case TS(127 ~: rest, b, c) => // Backspace
      val (first, last) = b.splitAt(c)
      TS(rest, first.dropRight(1) ++ last, c - 1)

    case TS(char ~: rest, b, c) =>
//      Debug("NORMAL CHAR " + char)
      val (first, last) = b.splitAt(c)
      TS(rest, (first :+ char.toChar) ++ last, c + 1)
  }

  def doEnter(b: Vector[Char], c: Int, rest: LazyList[Int]) = {
    val (chunks, chunkStarts, chunkIndex) = FilterTools.findChunks(b, c)
    if (chunkIndex == chunks.length - 1) Result(b.mkString)
    else injectNewLine(b, c, rest)
  }

  def enterFilter: TermCore.Filter = {
    case TS(13 ~: rest, b, c) => doEnter(b, c, rest) // Enter
    case TS(10 ~: rest, b, c) => doEnter(b, c, rest) // Enter
    case TS(10 ~: 13 ~: rest, b, c) => doEnter(b, c, rest) // Enter
    case TS(13 ~: 10 ~: rest, b, c) => doEnter(b, c, rest) // Enter
  }

  def exitFilter: TermCore.Filter = {
    case TS(Ctrl('c') ~: rest, b, c) =>
      Result("")
    case TS(Ctrl('d') ~: rest, b, c) =>
      // only exit if the line is empty, otherwise, behave like
      // "delete" (i.e. delete one char to the right)
      if (b.isEmpty) Exit else {
        val (first, last) = b.splitAt(c)
        TS(rest, first ++ last.drop(1), c)
      }
    case TS(-1 ~: rest, b, c) => Exit   // java.io.Reader.read() produces -1 on EOF
  }
  def clearFilter: TermCore.Filter = {
    case TS(Ctrl('l') ~: rest, b, c) => ClearScreen(TS(rest, b, c))
  }

  def moveStart(b: Vector[Char],
                c: Int,
                w: Int) = {
    val (_, chunkStarts, chunkIndex) = findChunks(b, c)
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
    Debug("moveDown\t" + b.length + "\t" + c + "\t" + w)
    b -> moveUpDown(b, c, w, 1, 1, c + w, _ <= _, true)
  }
}
