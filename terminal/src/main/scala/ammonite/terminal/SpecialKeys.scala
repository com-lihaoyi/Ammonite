package ammonite.terminal


/**
 * One place to assign all the esotic control key input snippets to
 * easy-to-remember names
 */
object SpecialKeys {


  /**
   * Lets you easily pattern match on characters modified by ctrl,
   * or convert a character into its ctrl-ed version
   */
  object Ctrl{
    def apply(c: Char) = c.toInt match{
      // For some reason,  Ctrl -, Ctrl /, Ctrl [, Ctrl ], all don't seem to
      // follow the same offset as the bulk of the other characters, at least
      // not on OSX. I haven't managed to find a canonical source of truth for
      // what generates what, but for now just hardcode it to fix the problem on
      // OSX until we find a more modular/flexible solution
      case 45 => 31.toChar.toString
      case 47 => 31.toChar.toString
      case 91 => 27.toChar.toString
      case 93 => 29.toChar.toString
      case n => (n - 96).toChar.toString
    }
    def unapply(i: Int): Option[Int] = Some(i + 96)
  }

  /**
   * The string value you get when you hit the alt key
   */
  def Alt = "\u001b"

  val Backspace = 127.toChar.toString
  val FnDelete = Alt + "[3~"

  val Tab = 9.toChar.toString
  val NewLine = Seq("\n", "\r")

  val DefaultUp = Alt+"[A"
  val DefaultDown = Alt+"[B"
  val DefaultRight = Alt+"[C"
  val DefaultLeft = Alt+"[D"

  val WeirdUp = Alt+"OA"
  val WeirdDown = Alt+"OB"
  val WeirdRight = Alt+"OC"
  val WeirdLeft = Alt+"OD"

  val Up = Seq(DefaultUp, WeirdUp)
  val Down = Seq(DefaultDown, WeirdDown)
  val Right = Seq(DefaultRight, WeirdRight)
  val Left = Seq(DefaultLeft, WeirdLeft)

  val Home = Alt+"OH"
  val End = Alt+"OF"

  // For some reason Screen makes these print different incantations
  // from a normal snippet, so this causes issues like
  // https://github.com/lihaoyi/Ammonite/issues/152 unless we special
  // case them
  val HomeScreen = Alt+"[1~"
  val EndScreen = Alt+"[4~"
  val HomeLinuxXterm = Alt+"[7~"
  val EndRxvt = Alt+"[8~"

  val ShiftUp = Alt+"[1;2A"
  val ShiftDown = Alt+"[1;2B"
  val ShiftRight = Alt+"[1;2C"
  val ShiftLeft = Alt+"[1;2D"

  val FnUp = Alt+"[5~"
  val FnDown = Alt+"[6~"
  val FnRight = Alt+"[F"
  val FnLeft = Alt+"[H"

  val AltUp = Alt*2+"[A"
  val AltDown = Alt*2+"[B"
  val AltRight = Alt*2+"[C"
  val AltLeft = Alt*2+"[D"

  val LinuxCtrlRight = Alt+"[1;5C"
  val LinuxCtrlLeft = Alt+"[1;5D"

  val FnAltUp = Alt*2+"[5~"
  val FnAltDown = Alt*2+"[6~"
  val FnAltRight = Alt+"[1;9F"
  val FnAltLeft = Alt+"[1;9H"

  // Same as fn-alt-{up, down}
//  val FnShiftUp = Alt*2+"[5~"
//  val FnShiftDown = Alt*2+"[6~"
  val FnShiftRight = Alt+"[1;2F"
  val FnShiftLeft = Alt+"[1;2H"

  val AltShiftUp = Alt+"[1;10A"
  val AltShiftDown = Alt+"[1;10B"
  val AltShiftRight = Alt+"[1;10C"
  val AltShiftLeft = Alt+"[1;10D"

  // Same as fn-alt-{up, down}
//  val FnAltShiftUp = Alt*2+"[5~"
//  val FnAltShiftDown = Alt*2+"[6~"
  val FnAltShiftRight = Alt+"[1;10F"
  val FnAltShiftLeft = Alt+"[1;10H"
}
