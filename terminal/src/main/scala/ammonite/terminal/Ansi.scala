package ammonite.terminal

import acyclic.file

/**
  * A library to make it easy to deal with colored Ansi strings within your
  * command-line programs.
  *
  * The main operations you need to know are:
  *
  * - `Ansi.parse(raw: CharSequence): Ansi.String`, to construct colored
  *   Ansi strings from plain (or colored) text
  *
  * - `Ansi.Str`, the primary data-type that you will use to pass-around
  *   colored Ansi strings and manipulate them: concatenating, splitting,
  *   applying or removing colors, etc.
  *
  * - `Ansi.Bold.{On, Off}`, `Ansi.Reversed.{On, Off}`, `Ansi.Underlined.{On, Off}`,
  *   `Ansi.Color.*`, `Ansi.Back.*`, `Ansi.Reset`: [[Ansi.Attr]]s that you use to
  *   apply (or remove) colors and other decorations from `Ansi.Str`s.
  *
  * Unlike normal `java.lang.String`s with Ansi escapes embedded inside,
  * [[Ansi.Str]] allows you to perform a range of operations in an efficient
  * manner:
  *
  * - Extracting the non-Ansi `plainText` version of the string
  *
  * - Get the non-Ansi `length`
  *
  * - Concatenate colored Ansi strings without worrying about leaking
  *   colors between them
  *
  * - Applying colors to certain portions of an existing [[Ansi.Str]],
  *   and ensuring that the newly-applied colors get properly terminated
  *   while existing colors are unchanged
  *
  * - Splitting colored Ansi strings at a `plainText` index
  *
  * - Rendering to colored `java.lang.String`s with Ansi escapes embedded,
  *   which can be passed around or concatenated without worrying about
  *   leaking colors.
  *
  * These are tasks which are possible to do with normal `java.lang.String`,
  * but are tedious, error-prone and typically inefficient. [[Ansi.Str]]
  * allows you to perform these tasks safely and easily.
  */
object Ansi {

  /**
    * Encapsulates a string with associated ANSI colors and text decorations.
    *
    * This is your primary data-type when you are dealing with colored Ansi
    * strings.
    *
    * Contains some basic string methods, as well as some ansi methods to e.g.
    * apply particular colors or other decorations to particular sections of
    * the [[Ansi.Str]]. [[render]] flattens it out into a `java.lang.String`
    * with all the colors present as ANSI escapes.
    *
    */
  case class Str private(private val chars: Array[Char], private val colors: Array[State]) {
    require(chars.length == colors.length)

    /**
      * Concatenates two [[Ansi.Str]]s, preserving the colors in each one and
      * avoiding any interference between them
      */
    def ++(other: Str) = Str(chars ++ other.chars, colors ++ other.colors)

    /**
      * Splits an [[Ansi.Str]] into two sub-strings, preserving the colors in
      * each one.
      *
      * @param index the plain-text index of the point within the [[Ansi.Str]]
      *              you want to use to split it.
      */
    def splitAt(index: Int) = {
      val (leftChars, rightChars) = chars.splitAt(index)
      val (leftColors, rightColors) = colors.splitAt(index)
      (new Str(leftChars, leftColors), new Str(rightChars, rightColors))
    }


    /**
      * The plain-text length of this [[Ansi.Str]], in UTF-16 characters (same
      * as `.length` on a `java.lang.String`). If you want fancy UTF-8 lengths,
      * use `.plainText`
      */
    def length = chars.length


    override def toString = render

    /**
      * The plain-text `java.lang.String` represented by this [[Ansi.Str]],
      * without all the Ansi colors or other decorations
      */
    lazy val plainText = new String(chars)
    /**
      * Converts this [[Ansi.Str]] into a `java.lang.String`, including all
      * the fancy Ansi colors or decorations as Ansi escapes embedded within
      * the string. "Terminates" colors at the right-most end of the resultant
      * `java.lang.String`, making it safe to concat-with or embed-inside other
      * `java.lang.String` without worrying about Ansi colors leaking out of it.
      */
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
        val hardOffMask = Bold.mask
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

  /**
    * A regex that can be used to easily find the position of Ansi escapes
    * with a `java.lang.String` or other `CharSequence`. Used to [[parse]]
    * an [[Ansi.Str]], but also usable independently in your own code.
    */
//  lazy val ansiRegex = "\u001B\\[[;\\d]*m".r
  lazy val ansiRegex = "[\u001b\u009b][\\[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]".r

  /**
    * Creates an [[Ansi.Str]] from a non-Ansi `java.lang.String` or other
    * `CharSequence`.
    *
    * Note that this method is implicit, meaning you can pass in a
    * `java.lang.String` anywhere an `Ansi.Str` is required and it will be
    * automatically parsed and converted for you.
    */
  implicit def parse(raw: CharSequence): Ansi.Str = {

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
      if (frag.charAt(0) == '\u001b' || frag.charAt(0) == '\u009b') {
        if (ParseMap.contains(frag)) currentColor = ParseMap(frag).transform(currentColor)
        else {
          // If our regex found something that looks suspicious,
          // bail out and report it
          throw new IllegalArgumentException(
            s"Unknown Ansi-escape ${frag.tail} inside string cannot be " +
            "parsed into an Ansi.Str"
          )
        }
      } else {
        var i = 0
        while(i < frag.length){
          chars(currentIndex) = frag(i)
          // If we found the start of an escape code that was missed by our
          // regex, also bail out and just report the index since that's all we
          // know about it
          if (frag(i) == '\u001b' || frag(i) == '\u009b') {
            throw new IllegalArgumentException(
              s"Unknown Ansi-escape at index $i inside string cannot be " +
              "parsed into an Ansi.Str"
            )
          }
          colors(currentIndex) = currentColor
          i += 1
          currentIndex += 1
        }
      }
    }

    Str(chars.take(currentIndex), colors.take(currentIndex))
  }


  /**
    * Represents a single, atomic ANSI escape sequence that results in a
    * color, background or decoration being added to the output. May or may not
    * have an escape sequence (`escapeOpt`), as some attributes (e.g. [[Bold.Off]])
    * are not widely/directly supported by terminals and so Ansi.Str supports them
    * by rendering a hard [[Reset]] and then re-rendering other [[Attr]]s that are
    * active.
    *
    * Many of the codes were stolen shamelessly from
    *
    * http://misc.flogisoft.com/bash/tip_colors_and_formatting
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

  /**
    * An [[Attr]] represented by an Ansi escape sequence
    */
  case class EscapeAttr private[Ansi](escape: String, resetMask: Int, applyMask: Int)
                                     (implicit sourceName: sourcecode.Name) extends Attr{
    def escapeOpt = Some(escape)
    val name = sourceName.value
    override def toString = escape + name + Console.RESET
  }

  /**
    * An [[Attr]] for which no Ansi escape sequence exists
    */
  case class ResetAttr private[Ansi](resetMask: Int, applyMask: Int)
                                    (implicit sourceName: sourcecode.Name) extends Attr{
    def escapeOpt = None
    val name = sourceName.value
    override def toString = name
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
    * Represents the removal of all ansi text decoration. Doesn't fit into any
    * convenient category, since it applies to them all.
    */
  val Reset = new EscapeAttr(Console.RESET, Int.MaxValue, 0)


  /**
    * Represents a set of [[Ansi.Attr]]s all occupying the same bit-space
    * in the state `Int`
    */
  sealed abstract class Category(offset: Int, width: Int)(implicit catName: sourcecode.Name){
    def mask = ((1 << width) - 1) << offset
    val all: Seq[Attr]
    lazy val bitsMap = all.map{ m => m.applyMask -> m}.toMap
    def makeAttr(s: String, applyValue: Int)(implicit name: sourcecode.Name) = {
      new EscapeAttr(s, mask, applyValue << offset)(catName.value + "." + name.value)
    }
    def makeNoneAttr(applyValue: Int)(implicit name: sourcecode.Name) = {
      new ResetAttr(mask, applyValue << offset)(catName.value + "." + name.value)
    }
  }

  /**
    * [[Attr]]s to turn text bold/bright or disable it
    */
  object Bold extends Category(offset = 0, width = 1){
    val On  = makeAttr(Console.BOLD, 1)
    val Off = makeNoneAttr(          0)
    val all = Seq(On, Off)
  }

  /**
    * [[Attr]]s to reverse the background/foreground colors of your text,
    * or un-reverse them
    */
  object Reversed extends Category(offset = 1, width = 1){
    val On  = makeAttr(Console.REVERSED,   1)
    val Off = makeAttr("\u001b[27m",       0)
    val all = Seq(On, Off)
  }
  /**
    * [[Attr]]s to enable or disable underlined text
    */
  object Underlined extends Category(offset = 2, width = 1){
    val On  = makeAttr(Console.UNDERLINED, 1)
    val Off = makeAttr("\u001b[24m",       0)
    val all = Seq(On, Off)
  }

  /**
    * [[Attr]]s to set or reset the color of your foreground text
    */
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
    /**
      * Foreground 256 color [[Attr]]s, for those terminals that support it
      */
    val Full =
      for(x <- 0 to 256)
      yield makeAttr(s"\u001b[38;5;${x}m", 17 + x)(s"Color.Full($x)")

    val all = Vector(
      Reset, Black, Red, Green, Yellow, Blue, Magenta, Cyan, LightGray, DarkGray,
      LightRed, LightGreen, LightYellow, LightBlue, LightMagenta, LightCyan, White
    ) ++ Full
  }

  /**
    * [[Attr]]s to set or reset the color of your background
    */
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
    /**
      * Background 256 color [[Attr]]s, for those terminals that support it
      */
    val Full =
      for(x <- 0 to 256)
      yield makeAttr(s"\u001b[48;5;${x}m", 17 + x)(s"Back.Full($x)")

    val all = Vector(
      Reset, Black, Red, Green, Yellow, Blue, Magenta, Cyan, LightGray, DarkGray,
      LightRed, LightGreen, LightYellow, LightBlue, LightMagenta, LightCyan, White
    ) ++ Full
  }

  /**
    * A list of possible categories
    */
  val categories = Vector(
    Color,
    Back,
    Bold,
    Underlined,
    Reversed
  )

  /**
    * A map to look-up and quickly convert string-colors into [[Ansi.Attr]]s,
    * for use in parsing or elsewhere
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