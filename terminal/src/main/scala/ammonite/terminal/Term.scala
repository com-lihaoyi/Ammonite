package ammonite.terminal


import scala.annotation.tailrec

import ammonite.terminal.LazyList._


// Test Unicode:  漢語;𩶘da
object Term{
  def main(args: Array[String]): Unit = {
    var history = List.empty[String]
    rec()
    @tailrec def rec(): Unit = {
      TermCore.readLine(
        "@ ",
        System.in,
        System.out,
        // Example multiline support by intercepting Enter key
        new HistoryFilter(history) orElse
        multilineFilter orElse
        defaultFilter,
        // Example displayTransform: underline all non-spaces
        displayTransform = (buffer, cursor) => {
          val buffer2 = buffer.flatMap{
            case ' ' => " "
            case '\n' => "\n"
            case c => Console.UNDERLINED + c + Console.RESET
          }
          (buffer2 , cursor)
        }
      ) match {
        case None => println("Bye!")
        case Some(s) =>
          history = s :: history
          println(s)
          rec()
      }
    }
  }

  def firstRow(cursor: Int, buffer: Vector[Char], width: Int) = {
    cursor < width && (buffer.indexOf('\n') >= cursor || buffer.indexOf('\n') == -1)
  }
  def lastRow(cursor: Int, buffer: Vector[Char], width: Int) = {
    (buffer.length - cursor) < width && (buffer.lastIndexOf('\n') < cursor || buffer.lastIndexOf('\n') == -1)
  }
  class HistoryFilter(history: => Seq[String]) extends TermCore.Filter{
    var index = -1
    var currentHistory = Vector[Char]()

    def continue(b: Vector[Char], newIndex: Int, rest: LazyList[Int], c: Int) = {
      if (index == -1 && newIndex != -1) currentHistory = b

      index = newIndex

      if (index == -1) TS(rest, currentHistory, c)
      else TS(rest, history(index).toVector, c)
    }
    def filter: TermCore.Filter = {
      case TI(TS(pref"\u001b[A$rest", b, c), w) if firstRow(c, b, w) =>
        continue(b, (index + 1) min (history.length - 1), rest, 99999)
      case TI(TS(pref"\u001b[B$rest", b, c), w) if lastRow(c, b, w) =>
        continue(b, (index - 1) max -1, rest, 0)
    }

    def isDefinedAt(x: TermInfo) = filter.isDefinedAt(x)

    def apply(v1: TermInfo) = filter.apply(v1)
  }
  val TS = TermState
  val TI = TermInfo
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
  val defaultFilter = {
    advancedNavFilter orElse
    basicNavFilter orElse
    exitFilter orElse
    enterFilter orElse
    loggingFilter orElse
    typingFilter
  }


  lazy val loggingFilter: TermCore.Filter = {
    case TS(5 ~: rest, b, c) => // Ctrl-E
      println("Char Display Mode Enabled! Ctrl-C to exit")
      var curr = rest
      while (curr.head != 3) {
        println("Char " + curr.head)
        curr = curr.tail
      }
      TS(curr, b, c)
  }
  lazy val typingFilter: TermCore.Filter = {
    case TS(pref"\u001b[3~$rest", b, c) =>
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
    case TS(3 ~: rest, b, c) => // Ctrl-C
      TS(rest, Vector.empty, 0)
    case TS(4 ~: rest, b, c) => Exit // Ctrl-D
  }

  def findChunks(b: Vector[Char], c: Int) = {
    val chunks = TermCore.splitBuffer(b)
    // The index of the first character in each chunk
    val chunkStarts = chunks.inits.map(x => x.length + x.sum).toStream.reverse
    // Index of the current chunk that contains the cursor
    val chunkIndex = chunkStarts.indexWhere(_ > c) match {
      case -1 => chunks.length-1
      case x => x - 1
    }
    (chunks, chunkStarts, chunkIndex)
  }
  def move(b: Vector[Char],
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
    move(b, c, w, 0, -1, c - w, _ > _, false)
  }
  def moveDown(b: Vector[Char], c: Int, w: Int) = {
    move(b, c, w, 1, 1, c + w, _ <= _, true)
  }

  lazy val basicNavFilter : TermCore.Filter = {
    case TI(TS(pref"\u001b[A$rest", b, c), w) => Debug("up"); TS(rest, b, moveUp(b, c, w))
    case TI(TS(pref"\u001b[B$rest", b, c), w) => Debug("down"); TS(rest, b, moveDown(b, c, w))
    case TS(pref"\u001b[C$rest", b, c) => Debug("right"); TS(rest, b, c + 1)
    case TS(pref"\u001b[D$rest", b, c) => Debug("left"); TS(rest, b, c - 1)

    case TS(pref"\u001b[5~$rest", b, c) => Debug("fn-up"); TS(rest, b, c)
    case TS(pref"\u001b[6~$rest", b, c) => Debug("fn-down"); TS(rest, b, c)
    case TS(pref"\u001b[F$rest", b, c) => Debug("fn-right"); TS(rest, b, c + 9999)
    case TS(pref"\u001b[H$rest", b, c) => Debug("fn-left"); TS(rest, b, c - 9999)

  }
  lazy val advancedNavFilter: TermCore.Filter = {
    case TS(pref"\u001b\u001b[A$rest", b, c) => Debug("alt-up"); TS(rest, b, c)
    case TS(pref"\u001b\u001b[B$rest", b, c) => Debug("alt-down"); TS(rest, b, c)
    case TS(pref"\u001b\u001b[C$rest", b, c) => Debug("alt-right"); TS(rest, b, c)
    case TS(pref"\u001b\u001b[D$rest", b, c) => Debug("alt-left"); TS(rest, b, c)

    case TS(pref"\u001b[1;2A$rest", b, c) => Debug("shift-up"); TS(rest, b, c)
    case TS(pref"\u001b[1;2B$rest", b, c) => Debug("shift-down"); TS(rest, b, c)
    case TS(pref"\u001b[1;2C$rest", b, c) => Debug("shift-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;2D$rest", b, c) => Debug("shift-left"); TS(rest, b, c)

    case TS(pref"\u001b\u001b[5~$rest", b, c) => Debug("fn-alt-up"); TS(rest, b, c)
    case TS(pref"\u001b\u001b[6~$rest", b, c) => Debug("fn-alt-down"); TS(rest, b, c)
    case TS(pref"\u001b[1;9F$rest", b, c) => Debug("fn-alt-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;9H$rest", b, c) => Debug("fn-alt-left"); TS(rest, b, c)

    // Conflicts with iTerm hotkeys, same as fn-{up, down}
    // case TS(pref"\u001b[5~$rest", b, c) => TS(rest, b, c) //fn-shift-up
    // case TS(pref"\u001b[6~$rest", b, c) => TS(rest, b, c) //fn-shift-down
    case TS(pref"\u001b[1;2F$rest", b, c) => Debug("fn-shift-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;2H$rest", b, c) => Debug("fn-shift-left"); TS(rest, b, c)

    case TS(pref"\u001b[1;10A$rest", b, c) => Debug("alt-shift-up"); TS(rest, b, c)
    case TS(pref"\u001b[1;10B$rest", b, c) => Debug("alt-shift-down"); TS(rest, b, c)
    case TS(pref"\u001b[1;10C$rest", b, c) => Debug("alt-shift-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;10D$rest", b, c) => Debug("alt-shift-left"); TS(rest, b, c)

    // Same as the case fn-alt-{up,down} without the shift
    // case TS(pref"\u001b\u001b[5~$rest", b, c) => TS(rest, b, c) //fn-alt-shift-up
    // case TS(pref"\u001b\u001b[6~$rest", b, c) => TS(rest, b, c) //fn-alt-shift-down
    case TS(pref"\u001b[1;10F$rest", b, c) => Debug("fn-alt-shift-right"); TS(rest, b, c)
    case TS(pref"\u001b[1;10H$rest", b, c) => Debug("fn-alt-shift-left"); TS(rest, b, c)
  }
}

