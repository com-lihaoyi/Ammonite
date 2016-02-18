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

  lazy val ansiRegex = "\u001B\\[[;\\d]*m".r

  implicit def parse(raw: CharSequence): AnsiStr = {
    val matches = ansiRegex.findAllMatchIn(raw)
    val indices = Seq(0) ++ matches.flatMap{m => Seq(m.start, m.end)} ++ Seq(raw.length)

    val fragIter = for{
      Seq(start, end) <- indices.sliding(2).toSeq
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
  lazy val length = plainText.length
  lazy val render = fragments.flatMap{case Left(s) => s; case Right(s) => s}
  lazy val plainText = fragments.collect{case Right(s) => s}.flatten

  override lazy val toString = render.mkString

  def splitAt(index: Int) = {
    val (splitState, fragIndex, leftOver) = query(index)
    val leftFrags = fragments.take(fragIndex)
    val rightFrags = fragments.drop(fragIndex + 1)
    val middle = fragments(fragIndex).asInstanceOf[Right[String, String]].b
    val (leftPartial, rightPartial) = middle.splitAt(leftOver)
    val cleanSplit = splitState == AnsiEscapeState()

    val leftPartialOpt = if (leftPartial.isEmpty) None else Some(Right(leftPartial))
    val left = leftFrags ++ leftPartialOpt

    val rightPartialOpt = if (rightPartial.isEmpty) Vector() else Vector(Right(rightPartial))
    val rightStartOpt =
      if (cleanSplit) Vector()
      else splitState.render.fragments

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
      fragments.lift(index) match{
        case Some(Left(s)) =>
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
        case Some(Right(s)) =>
          val fragLength = s.length
          if (screenLength + fragLength <= targetScreenLength && index + 1 < fragments.length) {
            rec(index + 1, state, screenLength + fragLength)
          }else{
            (state, index, targetScreenLength - screenLength)
          }
      }
    }
    rec(0, AnsiEscapeState(), 0)
  }
}
case class AnsiEscapeState(color: Option[String] = None,
                           bgColor: Option[String] = None,
                           bold: Boolean = false,
                           underlined: Boolean = false,
                           reversed: Boolean = false){
  def render = {
    val color = this.color.toVector
    val bgColor = this.bgColor.toVector
    val bold = if (this.bold) Vector(Console.BOLD) else Vector()
    val underlined = if (this.underlined) Vector(Console.UNDERLINED) else Vector()
    val reversed = if (this.reversed) Vector(Console.REVERSED) else Vector()
    new AnsiStr((color ++ bgColor ++ bold ++ underlined ++ reversed).map(Left(_)))
  }
}