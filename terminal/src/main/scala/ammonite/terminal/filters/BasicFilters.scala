package ammonite.terminal.filters
import acyclic.file
import ammonite.terminal.FilterTools._
import ammonite.terminal.LazyList._
import ammonite.terminal.SpecialKeys._
import ammonite.terminal.Filter
import ammonite.terminal._
import Filter._
/**
 * Filters for simple operation of a terminal: cursor-navigation
 * (including with all the modifier keys), enter/ctrl-c-exit, etc.
 */
object BasicFilters {
  def all = Filter.merge(
    navFilter,
    exitFilter,
    enterFilter,
    clearFilter,
    loggingFilter,
    typingFilter
  )

  def injectNewLine(b: Vector[Char], c: Int, rest: LazyList[Int]) = {
    val (first, last) = b.splitAt(c)
    TermState(rest, (first :+ '\n') ++ last, c + 1)
  }


  def navFilter = Filter.merge(
    simple(Up)((b, c, m) => moveUp(b, c, m.width)),
    simple(Down)((b, c, m) => moveDown(b, c, m.width)),
    simple(Right)((b, c, m) => (b, c + 1)),
    simple(Left)((b, c, m) => (b, c - 1))
  )

  def tabColumn(indent: Int, b: Vector[Char], c: Int, rest: LazyList[Int]) = {
    val (_, chunkStarts, chunkIndex) = FilterTools.findChunks(b, c)
    val chunkCol = c - chunkStarts(chunkIndex)
    val spacesToInject = indent - (chunkCol % indent)
    val (lhs, rhs) = b.splitAt(c)
    TS(rest, lhs ++ Vector.fill(spacesToInject)(' ') ++ rhs, c + spacesToInject)
  }

  def tabFilter(indent: Int): Filter = partial{
    case TS(9 ~: rest, b, c, _) => tabColumn(indent, b, c, rest)
  }

  def loggingFilter: Filter = partial{
    case TS(Ctrl('q') ~: rest, b, c, _) =>
      println("Char Display Mode Enabled! Ctrl-C to exit")
      var curr = rest
      while (curr.head != 3) {
        println("Char " + curr.head)
        curr = curr.tail
      }
      TS(curr, b, c)
  }
  def typingFilter: Filter = Filter.merge(
    action(SpecialKeys.FnDelete){
      case TermState(rest, b, c, _) =>
        val (first, last) = b.splitAt(c)
        TS(rest, first ++ last.drop(1), c)
    },
    action(SpecialKeys.Backspace){
      case TermState(rest, b, c, _) =>
        val (first, last) = b.splitAt(c)
        TS(rest, first.dropRight(1) ++ last, c - 1)
    },
    partial{
      case TS(char ~: rest, b, c, _) =>
        if (!Character.isISOControl(char)){
//          Debug("NORMAL CHAR " + char)
          val (first, last) = b.splitAt(c)
          TS(rest, (first :+ char.toChar) ++ last, c + 1)
        }else{
          TS(rest, b, c)
        }
    }
  )

  def doEnter(b: Vector[Char], c: Int, rest: LazyList[Int]) = {
    val (chunks, _, chunkIndex) = FilterTools.findChunks(b, c)
    if (chunkIndex == chunks.length - 1) Result(b.mkString)
    else injectNewLine(b, c, rest)
  }

  def enterFilter: Filter = action(SpecialKeys.NewLine){
    case TS(rest, b, c, _) => doEnter(b, c, rest) // Enter
  }

  def exitFilter: Filter = Filter.merge(
    action(Ctrl('c'))(_ => Result("")),
    action(Ctrl('d')){case TS(rest, b, c, _) =>
      // only exit if the line is empty, otherwise, behave like
      // "delete" (i.e. delete one char to the right)
      if (b.isEmpty) Exit
      else {
        val (first, last) = b.splitAt(c)
        TS(rest, first ++ last.drop(1), c)
      }
    },
    // java.io.Reader.read() produces -1 on EOF
    wrap(ti => ti.ts.inputs.dropPrefix(Seq(-1)).map(_ => Exit))
  )
  def clearFilter: Filter =
    action(Ctrl('l')){case TS(rest, b, c, _) => ClearScreen(TS(rest, b, c))}


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
    b -> moveUpDown(b, c, w, 1, 1, c + w, _ <= _, true)
  }
}
