package ammonite.terminal

import scala.annotation.tailrec

object AnsiStr{
  val BLACK = AnsiStr.parse(Console.BLACK)
  val RED = AnsiStr.parse(Console.RED)
  val GREEN = AnsiStr.parse(Console.GREEN)
  val YELLOW = AnsiStr.parse(Console.YELLOW)
  val BLUE = AnsiStr.parse(Console.BLUE)
  val MAGENTA  = AnsiStr.parse(Console.MAGENTA)
  val CYAN = AnsiStr.parse(Console.CYAN)
  val WHITE = AnsiStr.parse(Console.WHITE)

  val BLACK_B = AnsiStr.parse(Console.BLACK_B)
  val RED_B = AnsiStr.parse(Console.RED_B)
  val GREEN_B = AnsiStr.parse(Console.GREEN_B)
  val YELLOW_B = AnsiStr.parse(Console.YELLOW_B)
  val BLUE_B = AnsiStr.parse(Console.BLUE_B)
  val MAGENTA_B  = AnsiStr.parse(Console.MAGENTA_B)
  val CYAN_B = AnsiStr.parse(Console.CYAN_B)
  val WHITE_B = AnsiStr.parse(Console.WHITE_B)

  val RESET = AnsiStr.parse(Console.RESET)
  val BOLD = AnsiStr.parse(Console.BOLD)
  val UNDERLINED = AnsiStr.parse(Console.UNDERLINED)
  val REVERSED = AnsiStr.parse(Console.REVERSED)

  val ansiRegex = "\u001B\\[[;\\d]*m".r
  def parse(raw: CharSequence) = {
    val matches = ansiRegex.findAllMatchIn(raw)
    val fragIter = for{
      Seq(start, end) <- matches.flatMap{m => Seq(m.start, m.end)}.sliding(2)
      if start != end
    } yield {
      val frag = raw.subSequence(start, end).toString
      if (frag.charAt(0) == '\u001b') Left(frag) else Right(frag)
    }

    new AnsiStr(fragIter.toVector)
  }
}
/**
  *
  * @param fragments Left means it's some ansi escape, Right means it's content.
  */
class AnsiStr(val fragments: Vector[Either[String, String]]){
  def ++(other: AnsiStr) = new AnsiStr(fragments ++ other.fragments)
  def length = fragments.collect{case Right(s) => s.length}.sum
  def render = fragments.flatMap{case Left(s) => s; case Right(s) => s}
  def plainText = fragments.collect{case Right(s) => s}.flatten

  def split(index: Int) = {
    val (splitState, fragIndex, leftOver) = query(index)
    val leftFrags = fragments.take(fragIndex)
    val rightFrags = fragments.take(fragIndex + 1)
    val middle = fragments(fragIndex).asInstanceOf[Right[String, String]].b
    val (leftPartial, rightPartial) = middle.splitAt(leftOver)
    val cleanSplit = splitState == AnsiEscapeState()

    val leftPartialOpt = if (leftPartial.isEmpty) None else Some(Right(leftPartial))
    val left = leftFrags ++ leftPartialOpt

    val rightPartialOpt = if (rightPartial.isEmpty) Vector() else Vector(Right(rightPartial))
    val rightStartOpt =
      if (cleanSplit) Vector()
      else {
        val color = splitState.color.toVector
        val bgColor = splitState.bgColor.toVector
        val bold = if (splitState.bold) Vector(Console.BOLD) else Vector()
        val underlined = if (splitState.underlined) Vector(Console.UNDERLINED) else Vector()
        val reversed = if (splitState.reversed) Vector(Console.REVERSED) else Vector()
        (color ++ bgColor ++ bold ++ underlined ++ reversed).map(Right(_))
      }
    val right = rightStartOpt ++ rightPartialOpt ++ rightFrags
    (new AnsiStr(left), new AnsiStr(right))
  }

  /**
    * Tells you the state the desired visible-character index into the string,
    * and how far into the sequence of segments this happens
    */
  def query(targetScreenLength: Int): (AnsiEscapeState, Int, Int) = {

    @tailrec def rec(index: Int,
                     state: AnsiEscapeState,
                     screenLength: Int): (AnsiEscapeState, Int, Int) = {
      import Console._
      fragments(index) match{
        case Left(s) =>
          val newState = s match{
            case BLACK | RED | GREEN | YELLOW | BLUE | MAGENTA | CYAN | WHITE =>
              state.copy(color = Some(s))
            case BLACK_B | RED_B | GREEN_B | YELLOW_B | BLUE_B | MAGENTA_B | CYAN_B | WHITE_B =>
              state.copy(bgColor = Some(s))
            case UNDERLINED => state.copy(underlined = true)
            case BOLD => state.copy(bold = true)
            case REVERSED => state.copy(reversed = true)
            case RESET => AnsiEscapeState()
          }
          rec(index + 1, newState, screenLength)
        case Right(s) =>
          val fragLength = s.length
          if (screenLength + fragLength <= targetScreenLength) {
            rec(index + 1, state, screenLength + fragLength)
          }else{
            (state, index, screenLength + fragLength - targetScreenLength)
          }
      }
    }
    rec(0, AnsiEscapeState(), 0)
  }
}
case class AnsiEscapeState(color: List[String] = Nil,
                           bgColor: List[String] = Nil,
                           bold: Int = 0,
                           underlined: Int= 0,
                           reversed: Int = 0)