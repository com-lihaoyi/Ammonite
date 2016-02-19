package ammonite.terminal

import scala.annotation.tailrec

object Ansi {
  val Empty = Str(Vector())
  val Black = new Color(Console.BLACK)
  val Red = new Color(Console.RED)
  val Green = new Color(Console.GREEN)
  val Yellow = new Color(Console.YELLOW)
  val Blue = new Color(Console.BLUE)
  val Magenta = new Color(Console.MAGENTA)
  val Cyan = new Color(Console.CYAN)
  val White = new Color(Console.WHITE)

  val BlackB = new Color(Console.BLACK_B)
  val RedB = new Color(Console.RED_B)
  val GreenB = new Color(Console.GREEN_B)
  val YellowB = new Color(Console.YELLOW_B)
  val BlueB = new Color(Console.BLUE_B)
  val MagentaB = new Color(Console.MAGENTA_B)
  val CyanB = new Color(Console.CYAN_B)
  val WhiteB = new Color(Console.WHITE_B)

  val Reset = new Color(Console.RESET)
  val Bold = new Color(Console.BOLD)
  val Underlined = new Color(Console.UNDERLINED)
  val Reversed = new Color(Console.REVERSED)

  /**
    * Represents a single, atomic ANSI escape sequence that results in a
    * color, background or decoration being added to the output
    *
    * @param color the actual ANSI string
    */
  case class Color(color: String)(implicit name: sourcecode.Name) extends Fragment {
    override def toString = color + name.value + Console.RESET
  }
  object Color {

    val OverlapSets = Set(
      Set(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White),
      Set(BlackB, RedB, GreenB, YellowB, BlueB, MagentaB, CyanB, WhiteB),
      Set(Bold),
      Set(Underlined),
      Set(Reversed),
      Set(Reset)
    )

    /**
      * A mapping of who stomps over who. Note that this isn't symmetric!
      * `Reset` stomps over everyone, but nobody stomps over `Reset`. If
      * a color stomps over another, that means that when a stomp-er directly
      * follows a stomp-ee the stomp-ee can be ommitted
      */
    val OverlapMap = {
      val colorSets = for {
        set <- OverlapSets
        color <- set
      } yield (color, set)

      colorSets.toMap ++ Seq(Reset -> OverlapSets.flatten)
    }
    /**
      * Quickly convert string-colors into [[Ansi.Color]]s
      */
    val ParseMap = (for {
      set <- OverlapSets
      color <- set
    } yield (color.color, color)).toMap
  }


  case class Content(value: String) extends Fragment

  sealed trait Fragment


  object Str {


    lazy val ansiRegex = "\u001B\\[[;\\d]*m".r

    implicit def parse(raw: CharSequence): Str = {
      val matches = ansiRegex.findAllMatchIn(raw)
      val indices = Seq(0) ++ matches.flatMap { m => Seq(m.start, m.end) } ++ Seq(raw.length)

      val fragIter = for {
        Seq(start, end) <- indices.sliding(2).toSeq
        if start != end
      } yield {
        val frag = raw.subSequence(start, end).toString
        if (frag.charAt(0) == '\u001b') Color.ParseMap(frag)
        else Content(frag)
      }

      new Str(fragIter.toVector)
    }

    implicit def fromColor(s: Color): Str = Str(Vector(s))
  }
  /**
    *
    * @param fragments Left means it's some ansi escape, Right means it's content.
    */
  case class Str(fragments: Vector[Fragment]) {
    def ++(other: Str) = new Str(fragments ++ other.fragments)

    lazy val length = plainText.length
    lazy val render = fragments.flatMap { case Color(s) => s; case Content(s) => s }
    lazy val plainText = fragments.collect { case Content(s) => s }.flatten

    override lazy val toString = render.mkString

    /**
      * Overlays the desired color over the specified range of the [[Ansi.Str]].
      */
    def overlay(color: Color, start: Int, end: Int) = {
      val (left, rest) = splitAt(start)
      val (middle, right) = rest.splitAt(end - start)
      val newMiddle = new Str(middle.fragments.flatMap {
        case Color(Console.RESET) => Seq(Color(Console.RESET), color)
        case c: Color if Color.OverlapMap(color).contains(c) => Seq()
        case c => Seq(c)
      })

      val (endColor, _, _) = this.query(end)
      val capReset: Str =
        if (endColor.colors.exists(Color.OverlapMap(color))) Empty
        else Reset

      left ++ color ++ newMiddle ++ capReset ++ right
    }

    /**
      * Splits this [[Str]] at the specified plaintext index, producing
      * two children that when rendered will result in the same visual output.
      */
    def splitAt(index: Int) = {
      val (splitState, fragIndex, leftOver) = query(index)
      val leftFrags = fragments.take(fragIndex)
      val rightFrags = fragments.drop(fragIndex + 1)

      val middle = fragments.lift(fragIndex) match {
        case Some(Content(b)) => b
        case Some(Color(_)) => ???
        case None => ""
      }

      val (leftPartial, rightPartial) = middle.splitAt(leftOver)
      val cleanSplit = splitState == State()

      val leftPartialOpt = if (leftPartial.isEmpty) None else Some(Content(leftPartial))
      val left = leftFrags ++ leftPartialOpt

      val rightPartialOpt = if (rightPartial.isEmpty) Vector() else Vector(Content(rightPartial))
      val rightStartOpt =
        if (cleanSplit) Vector()
        else splitState.render.fragments

      val right = rightStartOpt ++ rightPartialOpt ++ rightFrags
      (new Str(left), new Str(right))
    }

    /**
      * Tells you the state the desired visible-character index into the string,
      * and how far into the sequence of segments this happens
      */
    def query(targetScreenLength: Int): (State, Int, Int) = {

      @tailrec def rec(index: Int,
                       state: State,
                       screenLength: Int): (State, Int, Int) = {
        import Console._
        fragments.lift(index) match {
          case Some(s: Color) =>
            val newState = s match {
              case Black | Red | Green | Yellow | Blue | Magenta | Cyan | White =>
                state.copy(color = Some(s))
              case BlackB | RedB | GreenB | YellowB | BlueB | MagentaB | CyanB | WhiteB =>
                state.copy(bgColor = Some(s))
              case Underlined => state.copy(underlined = true)
              case Bold => state.copy(bold = true)
              case Reversed => state.copy(reversed = true)
              case Reset => State()
              case x =>
                throw new Exception("WTF IS DIS " + x)
            }
            rec(index + 1, newState, screenLength)
          case Some(Content(s)) =>
            val fragLength = s.length
            if (screenLength + fragLength <= targetScreenLength && index + 1 < fragments.length) {
              rec(index + 1, state, screenLength + fragLength)
            } else {
              (state, index, targetScreenLength - screenLength)
            }
          case None => (state, index, 0)
        }
      }
      rec(0, State(), 0)
    }
  }

  case class State(color: Option[Color] = None,
                   bgColor: Option[Color] = None,
                   bold: Boolean = false,
                   underlined: Boolean = false,
                   reversed: Boolean = false) {

    def colors = {
      val color = this.color.toVector
      val bgColor = this.bgColor.toVector
      val bold = if (this.bold) Vector(Bold) else Vector()
      val underlined = if (this.underlined) Vector(Underlined ) else Vector()
      val reversed = if (this.reversed) Vector(Reversed) else Vector()
      color ++ bgColor ++ bold ++ underlined ++ reversed
    }
    def render = Str(colors)
  }

}