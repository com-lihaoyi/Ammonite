package ammonite.terminal


import acyclic.file

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * The core logic around a terminal; it defines the base `filters` API
 * through which anything (including basic cursor-navigation and typing)
 * interacts with the terminal.
 *
 * Maintains basic invariants, such as "cursor should always be within
 * the buffer", and "ansi terminal should reflect most up to date TermState"
 */
object TermCore {

  val ansiRegex = "\u001B\\[[;\\d]*m".r

  /**
   * Computes how tall a line of text is when wrapped at `width`.
   *
   * Even 0-character lines still take up one row!
   *
   * width = 2
   * 0 -> 1
   * 1 -> 1
   * 2 -> 1
   * 3 -> 2
   * 4 -> 2
   * 5 -> 3
   */
  def fragHeight(length: Int, width: Int) = math.max(1, (length - 1) / width + 1)

  def splitBuffer(buffer: Vector[Char]) = {
    val frags = mutable.Buffer.empty[Int]
    frags.append(0)
    for(c <- buffer){
      if (c == '\n') frags.append(0)
      else frags(frags.length - 1) = frags.last + 1
    }
    frags
  }
  def calculateHeight(buffer: Vector[Char],
                      width: Int,
                      prompt: String): Seq[Int] = {
    val rowLengths = splitBuffer(buffer)

    calculateHeight0(rowLengths, width - prompt.length)
  }

  /**
   * Given a buffer with characters and newlines, calculates how high
   * the buffer is and where the cursor goes inside of it.
   */
  def calculateHeight0(rowLengths: Seq[Int],
                       width: Int): Seq[Int] = {
    val fragHeights =
      rowLengths
        .inits
        .toVector
        .reverse // We want shortest-to-longest, inits gives longest-to-shortest
        .filter(_.nonEmpty) // Without the first empty prefix
        .map{ x =>
          fragHeight(
            // If the frag barely fits on one line, give it
            // an extra spot for the cursor on the next line
            x.last + 1,
            width
          )
        }
//    Debug("fragHeights " + fragHeights)
    fragHeights
  }

  def positionCursor(cursor: Int,
                     rowLengths: Seq[Int],
                     fragHeights: Seq[Int],
                     width: Int) = {
    var leftoverCursor = cursor
    //    Debug("leftoverCursor " + leftoverCursor)
    var totalPreHeight = 0
    var done = false
    // Don't check if the cursor exceeds the last chunk, because
    // even if it does there's nowhere else for it to go
    for(i <- 0 until rowLengths.length -1 if !done) {
      // length of frag and the '\n' after it
      val delta = rowLengths(i) + (if (i == rowLengths.length - 1) 0 else 1)
      //      Debug("delta " + delta)
      val nextCursor = leftoverCursor - delta
      if (nextCursor >= 0) {
        //        Debug("nextCursor " + nextCursor)
        leftoverCursor = nextCursor
        totalPreHeight += fragHeights(i)
      }else done = true
    }
    val cursorY = totalPreHeight + leftoverCursor / width
    val cursorX = leftoverCursor % width
    (cursorY, cursorX)
  }


  type Filter = PartialFunction[TermInfo, TermAction]
  type Action = (Vector[Char], Int) => (Vector[Char], Int)
  trait DelegateFilter extends Filter{
    def filter: Filter
    def isDefinedAt(x: TermInfo) = filter.isDefinedAt(x)
    def apply(v1: TermInfo) = filter(v1)
  }
  /**
   * Blockingly reads a line from the given input stream and returns it.
   *
   * @param prompt The prompt to display when requesting input
   * @param reader The input-stream where characters come in, e.g. System.in
   * @param writer The output-stream where print-outs go, e.g. System.out
   * @param filters A set of actions that can be taken depending on the input,
   *                to manipulate the internal state of the terminal.
   * @param displayTransform code to manipulate the display of the buffer and
   *                         cursor, without actually changing the logical
   *                         values inside them.
   */
  def readLine(prompt: Prompt,
               reader: java.io.Reader,
               writer: java.io.Writer,
               filters: PartialFunction[TermInfo, TermAction] = PartialFunction.empty,
               displayTransform: (Vector[Char], Int) => (Vector[Char], Int) = (x, i) => (x, i))
               : Option[String] = {

    def redrawLine(buffer: Vector[Char],
                   cursor: Int,
                   ups: Int,
                   rowLengths: Seq[Int],
                   fullPrompt: Boolean = true) = {
      ansi.up(ups)

      ansi.left(9999)
      ansi.clearScreen(0)

      writer.write(
        if (fullPrompt) prompt.full
        else prompt.lastLine
      )
      var i = 0
      var currWidth = 0
      while(i < buffer.length){
        if (currWidth >= width - prompt.lastLineNoAnsi.length){
          writer.write(" " * prompt.lastLineNoAnsi.length)
          currWidth = 0
        }

        ansiRegex.findPrefixMatchOf(buffer.drop(i)) match{
          case Some(m) =>
//            Debug("Some(m) " + m.source + " | " + m.source.length)
            writer.write(buffer.slice(i, i + m.end).toArray)
            i += m.end
          case None =>
//            Debug("None")
            writer.write(buffer(i))
            if (buffer(i) == '\n') currWidth += 9999
            currWidth += 1
            i += 1
        }
      }


      val fragHeights = calculateHeight0(rowLengths, width - prompt.lastLineNoAnsi.length)
      val (cursorY, cursorX) = positionCursor(
        cursor,
        rowLengths,
        fragHeights,
        width - prompt.lastLineNoAnsi.length
      )

      ansi.up(fragHeights.sum - 1)
      ansi.left(9999)

//      Debug("DOWN " + cursorY)
//      Debug("RIGHT " + cursorX)
      ansi.down(cursorY)
      ansi.right(cursorX)
      ansi.right(prompt.lastLineNoAnsi.length)
      writer.flush()
    }

    @tailrec
    def readChar(lastState: TermState, ups: Int, fullPrompt: Boolean = true): Option[String] = {
      Debug("")
      Debug("readChar\t" + ups)
      val moreInputComing = reader.ready()
      lazy val (transformedBuffer, cursorOffset) = displayTransform(
        lastState.buffer,
        lastState.cursor
      )
      lazy val lastOffsetCursor = lastState.cursor + cursorOffset
      lazy val rowLengths = splitBuffer(lastState.buffer)
      if (!moreInputComing) redrawLine(
        transformedBuffer,
        lastOffsetCursor,
        ups,
        rowLengths,
        fullPrompt
      )

      lazy val (oldCursorY, _) = positionCursor(
        lastOffsetCursor,
        rowLengths,
        calculateHeight0(rowLengths, width - prompt.lastLineNoAnsi.length),
        width - prompt.lastLineNoAnsi.length
      )

      def updateState(s: LazyList[Int], b: Vector[Char], c: Int): (Int, TermState) = {
        val newCursor = math.max(math.min(c, b.length), 0)
        val nextUps =
          if (moreInputComing) ups
          else oldCursorY

        val newState = TermState(s, b, newCursor)

        (nextUps, newState)
      }
      filters(TermInfo(lastState, width - prompt.lastLineNoAnsi.length)) match {
        case Printing(TermState(s, b, c), stdout) =>
          writer.write(stdout)
          val (nextUps, newState) = updateState(s, b, c)
          readChar(newState, nextUps)

        case TermState(s, b, c) =>
          Debug("TermState c\t" + c)
          val (nextUps, newState) = updateState(s, b, c)
          readChar(newState, nextUps, false)

        case Result(s) =>
          redrawLine(transformedBuffer, lastState.buffer.length, oldCursorY, rowLengths, false)
          writer.write(10)
          writer.write(13)
          writer.flush()
          Some(s)
        case ClearScreen(ts) =>
          ansi.clearScreen(2)
          ansi.up(9999)
          ansi.left(9999)
          readChar(ts, ups)
        case Exit =>
          None
      }
    }

    lazy val ansi = new Ansi(writer)
    lazy val (width, _, initialConfig) = TTY.init()
    try {
      readChar(TermState(LazyList.continually(reader.read()), Vector.empty, 0), 0)
    }finally{

      // Don't close these! Closing these closes stdin/stdout,
      // which seems to kill the entire program

      // reader.close()
      // writer.close()
      TTY.stty(initialConfig)
    }
  }
}

case class TermInfo(ts: TermState, width: Int)

sealed trait TermAction
case class Printing(ts: TermState, stdout: String) extends TermAction
case class TermState(inputs: LazyList[Int],
                     buffer: Vector[Char],
                     cursor: Int) extends TermAction
object TermState{
  def unapply(ti: TermInfo): Option[(LazyList[Int], Vector[Char], Int)] = {
    TermState.unapply(ti.ts)
  }
  def unapply(ti: TermAction): Option[(LazyList[Int], Vector[Char], Int)] = ti match{
    case ts: TermState => TermState.unapply(ts)
    case _ => None
  }

}
case class ClearScreen(ts: TermState) extends TermAction
case object Exit extends TermAction
case class Result(s: String) extends TermAction


object Prompt {

  import Console._

  private val colors = Set(BLACK, BLUE, CYAN, GREEN, MAGENTA, RED, WHITE, YELLOW)
  private val bgColors = Set(BLACK_B, BLUE_B, CYAN_B, GREEN_B, MAGENTA_B, RED_B, WHITE_B, YELLOW_B)

  implicit def apply(prompt: String) = {
    val lastLineBegin = prompt.lastIndexOf("\n")
    if (lastLineBegin == -1) {
      new Prompt(prompt, prompt)
    } else {
      val preLastLines = prompt.substring(0, lastLineBegin)
      val ansiEscapeState = TermCore.ansiRegex
        .findAllIn(preLastLines)
        .toSeq
        .reverseIterator
        .takeWhile(_ != Console.RESET)
        .foldLeft(new AnsiEscapeState) {
          case (state, color) if state.color.isEmpty && colors.contains(color) =>
            state.copy(color = Some(color))
          case (state, bgColor) if state.bgColor.isEmpty && bgColors.contains(bgColor)=>
            state.copy(bgColor = Some(bgColor))
          case (state, REVERSED) =>
            state.copy(reversed = true)
          case (state, BOLD) =>
            state.copy(bold = true)
          case (state, UNDERLINED) =>
            state.copy(underlined = true)
          case (state, _) =>
            state
        }
      val lastLine = ansiEscapeState + prompt.substring(lastLineBegin + 1)
      new Prompt(prompt, lastLine)
    }
  }
}

case class Prompt(
  full: String,
  lastLine: String) {

  val lastLineNoAnsi = TermCore.ansiRegex.replaceAllIn(lastLine, "")
}

case class AnsiEscapeState(
  color: Option[String] = None,
  bgColor: Option[String] = None,
  bold: Boolean = false,
  underlined: Boolean = false,
  reversed: Boolean = false) {
  override def toString: String = {
    val builder = new StringBuilder(25)
    color.map(builder.append(_))
    bgColor.map(builder.append(_))
    if (bold) builder.append(Console.BOLD)
    if (underlined) builder.append(Console.UNDERLINED)
    if (reversed) builder.append(Console.REVERSED)
    builder.toString
  }
}

