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
  val ResetFore = new Color("\u001b[39m")

  val BlackB = new Color(Console.BLACK_B)
  val RedB = new Color(Console.RED_B)
  val GreenB = new Color(Console.GREEN_B)
  val YellowB = new Color(Console.YELLOW_B)
  val BlueB = new Color(Console.BLUE_B)
  val MagentaB = new Color(Console.MAGENTA_B)
  val CyanB = new Color(Console.CYAN_B)
  val WhiteB = new Color(Console.WHITE_B)
  val ResetB = new Color("\u001b[49m")

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
  case class Color(color: String)(implicit name: sourcecode.Name) extends Frag {
    override def toString = color + name.value + Console.RESET
  }
  object Color {

    val OverlapSets = Set(
      Set(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White, ResetFore),
      Set(BlackB, RedB, GreenB, YellowB, BlueB, MagentaB, CyanB, WhiteB, ResetB),
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

  /**
    * A piece of an [[Ansi.Str]] which just contains raw string-content,
    * without any special characters
    */
  case class Content(value: String) extends Frag

  /**
    * A piece of an [[Ansi.Str]]
    */
  sealed trait Frag


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
  case class Str(fragments: Vector[Frag]) {
    def ++(other: Str) = new Str(fragments ++ other.fragments)

    lazy val length = plainText.length
    lazy val render = fragments.flatMap { case Color(s) => s; case Content(s) => s }
    lazy val plainText = fragments.collect { case Content(s) => s }.flatten

    override lazy val toString = render.mkString

    /**
      * Overlays the desired color over the specified range of the [[Ansi.Str]].
      */
    def overlay(overlayColor: Color, start: Int, end: Int) = {
      transform{ (frag, originalState, transformedState, index, screenLength) =>
        frag match{
          case c: Content =>
            val fragLength = c.value.length
            if (screenLength < start && screenLength + fragLength >= start){
              // Turning it on
              val (pre, post) = c.value.splitAt(start - screenLength)
              Seq(Content(pre), overlayColor, Content(post))
            }else if (screenLength < end && screenLength + fragLength >= end){
              // Turning it off
              val (pre, post) = c.value.splitAt(end - screenLength)
              Seq(Content(pre)) ++ originalState.diffFrom(transformedState) ++ Seq(Content(post))
            }else Seq(c)
          case c: Color =>
            // Inside the range
            if (screenLength >= start && screenLength <= end){
              val stompedState = transformedState.transform(c).transform(overlayColor)
              stompedState.diffFrom(transformedState)
            }else Seq(c)

        }
      }
    }

    /**
      * Runs a function over the sequence of [[Frag]]s and [[State]]s to
      * transform it into a new sequence of [[Frag]]s. The callback returns
      * 0 or more [[Frag]]s per call, which get stitched together into the
      * final result, with consecutive [[Content]]s collapsed and redundant
      * [[Color]]s removed
      */
    def transform(f: (Frag, State, State, Int, Int) => Seq[Frag]) = {
      var newCurrentState = State()
      val output = collection.mutable.Buffer.empty[Frag]
      def append(f: Frag) = f match{
        case c: Content =>
          output.lastOption match{
            case Some(c0: Content) => output(output.length-1) = Content(c0.value + c.value)
            case _ => output.append(c)
          }
        case c: Color =>
          val transformedState = newCurrentState.transform(c)
          if (transformedState != newCurrentState){
            newCurrentState = transformedState
            output.append(c)
          }
      }

      walk{ (frag, originalState, index, screenLength) =>
        f(frag, originalState, newCurrentState, index, screenLength).foreach(append)
        true
      }
      Str(output.toVector)
    }
    /**
      * Walk over the sequence of [[Frag]]s; the callback gets called with each
      * frag, together with some computed metadata about the ansi-[[State]] and
      * screen length. In response, it returns a boolean that determines whether
      * or not to continue walking or bail early.
      */
    def walk(f: (Frag, State, Int, Int) => Boolean): (State, Int, Int) = {

      @tailrec def rec(index: Int,
                       state: State,
                       screenLength: Int): (State, Int, Int) = {
        if (index >= fragments.length) (state, index, screenLength)
        else if (!f(fragments(index), state, index, screenLength)) (state, index, screenLength)
        else fragments.lift(index) match {
          case Some(s: Color) =>
            val newState = state.transform(s)
            rec(index + 1, newState, screenLength)
          case Some(Content(s)) => rec(index + 1, state, screenLength + s.length)
          case None => (state, index, screenLength)
        }
      }
      rec(0, State(), 0)
    }

    /**
      * Tells you the state the desired visible-character index into the string,
      * and how far into the sequence of segments this happens
      */
    def query(targetScreenLength: Int): (State, Int, Int) = {

      val (endState, endIndex, endScreenLength) = walk{
        case (frag: Color, state, index, screenLength) => true
        case (frag: Content, state, index, screenLength) =>
          val fragLength = frag.value.length
          if (screenLength + fragLength > targetScreenLength) false
          else true
      }
      (endState,  endIndex, targetScreenLength - endScreenLength)
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
    def query_old(targetScreenLength: Int): (State, Int, Int) = {

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

    def transform(c: Color) = c match {
      case Black | Red | Green | Yellow | Blue | Magenta | Cyan | White =>
        this.copy(color = Some(c))
      case ResetFore => this.copy(color = None)
      case BlackB | RedB | GreenB | YellowB | BlueB | MagentaB | CyanB | WhiteB =>
        this.copy(bgColor = Some(c))
      case ResetB => this.copy(bgColor = None)
      case Underlined => this.copy(underlined = true)
      case Bold => this.copy(bold = true)
      case Reversed => this.copy(reversed = true)
      case Reset => State()
      case x =>
        throw new Exception("WTF IS DIS " + x)
    }

    def colors: Vector[Color] = {
      val color = this.color.toVector
      val bgColor = this.bgColor.toVector
      val bold = if (this.bold) Vector(Bold) else Vector()
      val underlined = if (this.underlined) Vector(Underlined ) else Vector()
      val reversed = if (this.reversed) Vector(Reversed) else Vector()
      color ++ bgColor ++ bold ++ underlined ++ reversed
    }
    def render = Str(colors)

    def diffFrom(source: State): Vector[Color] = {

      if (!this.bold && source.bold ||
          !this.underlined && source.underlined ||
          !this.reversed && source.reversed){
        Vector(Reset) ++ colors
      }else{
        val out = collection.mutable.Buffer.empty[Color]
        if (this.color != source.color) out.append(color.getOrElse(ResetFore))
        if (this.bgColor != source.bgColor) out.append(bgColor.getOrElse(ResetB))
        // This can only turn *on* the flags, because if any of them could turn
        // *off* the flags it would have been caught in the if-block above
        if (this.bold != source.bold) out.append(Bold)
        if (this.underlined != source.underlined ) out.append(Underlined)
        if (this.reversed != source.reversed) out.append(Reversed)
        out.toVector
      }
    }
  }

}