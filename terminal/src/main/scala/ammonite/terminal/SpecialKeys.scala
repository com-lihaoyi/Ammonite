package ammonite.terminal


/**
 * Created by haoyi on 6/11/15.
 */
object SpecialKeys {


  /**
   * Lets you easily pattern match on characters modified by ctrl,
   * or convert a character into its ctrl-ed version
   */
  object Ctrl{
    def apply(c: Char) = (c - 96).toChar.toString
    def unapply(i: Int): Option[Int] = Some(i + 96)
  }

  /**
   * The string value you get when you hit the alt key
   */
  def Alt = "\u001b"


  val Up = Alt+"[A"
  val Down = Alt+"[B"
  val Right = Alt+"[C"
  val Left = Alt+"[D"

  val Home = Alt+"OH"
  val End = Alt+"OF"

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
