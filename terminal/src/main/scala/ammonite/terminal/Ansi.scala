package ammonite.terminal

import acyclic.file

/**
  * Many of the codes were stolen shamelessly from
  *
  * http://misc.flogisoft.com/bash/tip_colors_and_formatting
  */
object Ansi {
  /**
    * Represents a single, atomic ANSI escape sequence that results in a
    * color, background or decoration being added to the output.
    */

  sealed trait Attr{
    /**
      * escapeOpt the actual ANSI escape sequence corresponding to this Attr
      */
    def escapeOpt: Option[String]
    def resetMask: Int
    def applyMask: Int
    def transform(state: Int) = (state & ~resetMask) | applyMask

    def name: String
    def matches(state: Int) = (state & resetMask) == applyMask
    def apply(s: Ansi.Str) = s.overlay(this, 0, s.length)
  }
  case class SomeAttr private[Ansi](escape: String, resetMask: Int, applyMask: Int)
                                   (implicit sourceName: sourcecode.Name) extends Attr{
    def escapeOpt = Some(escape)
    val name = sourceName.value
    override def toString = escape + name + Console.RESET
  }
  case class NoneAttr private[Ansi](resetMask: Int, applyMask: Int)
                                   (implicit sourceName: sourcecode.Name) extends Attr{
    def escapeOpt = None
    val name = sourceName.value
    override def toString = name
  }
  object Attr {
    val Reset = new SomeAttr(Console.RESET, Int.MaxValue, 0)

    /**
      * Quickly convert string-colors into [[Ansi.Attr]]s
      */
    val ParseMap = {
      val pairs = for {
        cat <- categories
        color <- cat.all
        str <- color.escapeOpt
      } yield (str, color)
      (pairs :+ (Console.RESET -> Reset)).toMap
    }
  }


  /**
    * An [[Ansi.Str]]'s `color`s array is filled with Ints, each representing
    * the ANSI state of one character encoded in its bits. Each [[Attr]] belongs
    * to a [[Category]] that occupies a range of bits within each int:
    *
    *   ... 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    *  |--------|  |-----------------------|  |-----------------------|  |  |  |bold
    *           |                          |                          |  |  |reversed
    *           |                          |                          |  |underlined
    *           |                          |                          |foreground-color
    *           |                          |background-color
    *           |unused
    *
    *
    * The `0000 0000 0000 0000` int corresponds to plain text with no decoration
    *
    */
  type State = Int

  /**
    * Represents a set of [[Ansi.Attr]]s all occupying the same bit-space
    * in the state `Int`
    */
  sealed abstract class Category(offset: Int, width: Int)(implicit catName: sourcecode.Name){
    def mask = ((1 << width) - 1) << offset
    val all: Seq[Attr]
    lazy val bitsMap = all.map{ m => m.applyMask -> m}.toMap
    def makeAttr(s: String, applyValue: Int)(implicit name: sourcecode.Name) = {
      new SomeAttr(s, mask, applyValue << offset)(catName.value + "." + name.value)
    }
    def makeNoneAttr(applyValue: Int)(implicit name: sourcecode.Name) = {
      new NoneAttr(mask, applyValue << offset)(catName.value + "." + name.value)
    }
  }

  object Bold extends Category(offset = 0, width = 1){

    val On  = makeAttr(Console.BOLD, 1)
    val Off = makeNoneAttr(          0)
    val all = Seq(On, Off)
  }

  object Reversed extends Category(offset = 1, width = 1){
    val On  = makeAttr(Console.REVERSED,   1)
    val Off = makeAttr("\u001b[27m",       0)
    val all = Seq(On, Off)
  }

  object Underlined extends Category(offset = 2, width = 1){
    val On  = makeAttr(Console.UNDERLINED, 1)
    val Off = makeAttr("\u001b[24m",       0)
    val all = Seq(On, Off)
  }

  object Color extends Category(offset = 3, width = 9){

    val Reset        = makeAttr("\u001b[39m",     0)
    val Black        = makeAttr(Console.BLACK,    1)
    val Red          = makeAttr(Console.RED,      2)
    val Green        = makeAttr(Console.GREEN,    3)
    val Yellow       = makeAttr(Console.YELLOW,   4)
    val Blue         = makeAttr(Console.BLUE,     5)
    val Magenta      = makeAttr(Console.MAGENTA,  6)
    val Cyan         = makeAttr(Console.CYAN,     7)
    val LightGray    = makeAttr("\u001b[37m",     8)
    val DarkGray     = makeAttr("\u001b[90m",     9)
    val LightRed     = makeAttr("\u001b[91m",    10)
    val LightGreen   = makeAttr("\u001b[92m",    11)
    val LightYellow  = makeAttr("\u001b[93m",    12)
    val LightBlue    = makeAttr("\u001b[94m",    13)
    val LightMagenta = makeAttr("\u001b[95m",    14)
    val LightCyan    = makeAttr("\u001b[96m",    15)
    val White        = makeAttr("\u001b[97m",    16)

    val Full =
      for(x <- 0 to 256)
      yield makeAttr(s"\u001b[38;5;${x}m", 17 + x)(s"Color.Full($x)")

    val all = Vector(
      Reset, Black, Red, Green, Yellow, Blue, Magenta, Cyan, LightGray, DarkGray,
      LightRed, LightGreen, LightYellow, LightBlue, LightMagenta, LightCyan, White
    ) ++ Full
  }

  object Back extends Category(offset = 12, width = 9){

    val Reset        = makeAttr("\u001b[49m",       0)
    val Black        = makeAttr(Console.BLACK_B,    1)
    val Red          = makeAttr(Console.RED_B,      2)
    val Green        = makeAttr(Console.GREEN_B,    3)
    val Yellow       = makeAttr(Console.YELLOW_B,   4)
    val Blue         = makeAttr(Console.BLUE_B,     5)
    val Magenta      = makeAttr(Console.MAGENTA_B,  6)
    val Cyan         = makeAttr(Console.CYAN_B,     7)
    val LightGray    = makeAttr("\u001b[47m",       8)
    val DarkGray     = makeAttr("\u001b[100m",      9)
    val LightRed     = makeAttr("\u001b[101m",     10)
    val LightGreen   = makeAttr("\u001b[102m",     11)
    val LightYellow  = makeAttr("\u001b[103m",     12)
    val LightBlue    = makeAttr("\u001b[104m",     13)
    val LightMagenta = makeAttr("\u001b[105m",     14)
    val LightCyan    = makeAttr("\u001b[106m",     15)
    val White        = makeAttr("\u001b[107m",     16)

    val Full =
      for(x <- 0 to 256)
        yield makeAttr(s"\u001b[48;5;${x}m", 17 + x)(s"Back.Full($x)")

    val all = Vector(
      Reset, Black, Red, Green, Yellow, Blue, Magenta, Cyan, LightGray, DarkGray,
      LightRed, LightGreen, LightYellow, LightBlue, LightMagenta, LightCyan, White
    ) ++ Full
  }

  val hardOffMask = Bold.mask
  val categories = Vector(
    Color,
    Back,
    Bold,
    Underlined,
    Reversed
  )

  object Str {

    lazy val ansiRegex = "\u001B\\[[;\\d]*m".r

    implicit def parse(raw: CharSequence): Str = {
      // This will
      val chars = new Array[Char](raw.length)
      val colors = new Array[Int](raw.length)
      var currentIndex = 0
      var currentColor = 0

      val matches = ansiRegex.findAllMatchIn(raw)
      val indices = Seq(0) ++ matches.flatMap { m => Seq(m.start, m.end) } ++ Seq(raw.length)

      for {
        Seq(start, end) <- indices.sliding(2).toSeq
        if start != end
      } {
        val frag = raw.subSequence(start, end).toString
        if (frag.charAt(0) == '\u001b' && Attr.ParseMap.contains(frag)) {
          currentColor = Attr.ParseMap(frag).transform(currentColor)
        } else {
          var i = 0
          while(i < frag.length){
            chars(currentIndex) = frag(i)
            colors(currentIndex) = currentColor
            i += 1
            currentIndex += 1
          }
        }
      }

      Str(chars.take(currentIndex), colors.take(currentIndex))
    }

  }


  /**
    * Encapsulates a string with associated ANSI colors and text decorations.
    *
    * Contains some basic string methods, as well as some ansi methods to e.g.
    * apply particular colors or other decorations to particular sections of
    * the [[Ansi.Str]]. [[render]] flattens it out into a `java.lang.String`
    * with all the colors present as ANSI escapes.
    *
    */
  case class Str private(chars: Array[Char], colors: Array[State]) {
    require(chars.length == colors.length)

    def ++(other: Str) = Str(chars ++ other.chars, colors ++ other.colors)
    def splitAt(index: Int) = {
      val (leftChars, rightChars) = chars.splitAt(index)
      val (leftColors, rightColors) = colors.splitAt(index)
      (new Str(leftChars, leftColors), new Str(rightChars, rightColors))
    }
    def length = chars.length
    override def toString = render

    def plainText = new String(chars.toArray)
    def render = {
      // Pre-size StringBuilder with approximate size (ansi colors tend
      // to be about 5 chars long) to avoid re-allocations during growth
      val output = new StringBuilder(chars.length + colors.length * 5)


      var currentState = 0
      /**
        * Emit the ansi escapes necessary to transition
        * between two states, if necessary.
        */
      def emitDiff(nextState: Int) = if (currentState != nextState){
        // Any of these transitions from 1 to 0 within the hardOffMask
        // categories cannot be done with a single ansi escape, and need
        // you to emit a RESET followed by re-building whatever ansi state
        // you previous had from scratch
        if ((currentState & ~nextState & hardOffMask) != 0){
          output.append(Console.RESET)
          currentState = 0
        }

        var categoryIndex = 0
        while(categoryIndex < categories.length){
          val cat = categories(categoryIndex)
          if ((cat.mask & currentState) != (cat.mask & nextState)){
            val attr = cat.bitsMap(nextState & cat.mask)

            if (attr.escapeOpt.isDefined) {
              output.append(attr.escapeOpt.get)
            }
          }
          categoryIndex += 1
        }
      }

      var i = 0
      while(i < colors.length){
        // Emit ANSI escapes to change colors where necessary
        emitDiff(colors(i))
        currentState = colors(i)
        output.append(chars(i))
        i += 1
      }

      // Cap off the left-hand-side of the rendered string with any ansi escape
      // codes necessary to rest the state to 0
      emitDiff(0)

      output.toString
    }



    /**
      * Overlays the desired color over the specified range of the [[Ansi.Str]].
      */
    def overlay(overlayColor: Attr, start: Int, end: Int) = {
      require(end >= start,
        s"end:$end must be greater than start:$end in AnsiStr#overlay call"
      )
      val colorsOut = new Array[Int](colors.length)
      var i = 0
      while(i < colors.length){
        if (i >= start && i < end) colorsOut(i) = overlayColor.transform(colors(i))
        else colorsOut(i) = colors(i)
        i += 1
      }
      new Str(chars, colorsOut)
    }

  }


}