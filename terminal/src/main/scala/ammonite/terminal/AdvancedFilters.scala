package ammonite.terminal

import ammonite.terminal.FilterTools._

/**
 * Created by haoyi on 6/11/15.
 */
object AdvancedFilters {
  lazy val advancedNavFilter = orElseAll(
    Case(Alt*2+"[A"){(b, c, m) => Debug("alt-up"); (b, c)},
    Case(Alt*2+"[B"){(b, c, m) => Debug("alt-down"); (b, c)},
    Case(Alt*2+"[C"){(b, c, m) => wordRight(b, c)},
    Case(Alt*2+"[D"){(b, c, m) => wordLeft(b, c)},

    Case(Alt+"[1;2A"){(b, c, m) => Debug("shift-up"); (b, c)},
    Case(Alt+"[1;2B"){(b, c, m) => Debug("shift-down"); (b, c)},
    Case(Alt+"[1;2C"){(b, c, m) => Debug("shift-right"); (b, c)},
    Case(Alt+"[1;2D"){(b, c, m) => Debug("shift-left"); (b, c)},

    Case(Alt*2+"[5~"){(b, c, m) => Debug("fn-alt-up"); (b, c)},
    Case(Alt*2+"[6~"){(b, c, m) => Debug("fn-alt-down"); (b, c)},
    Case(Alt+"[1;9F"){(b, c, m) => Debug("fn-alt-right"); (b, c)},
    Case(Alt+"[1;9H"){(b, c, m) => Debug("fn-alt-left"); (b, c)},

    Case(Alt*2+"[5~"){(b, c, m) => Debug("fn-alt-up"); (b, c)},
    Case(Alt*2+"[6~"){(b, c, m) => Debug("fn-alt-down"); (b, c)},
    Case(Alt+"[1;2F"){(b, c, m) => Debug("fn-shift-right"); (b, c)},
    Case(Alt+"[1;2H"){(b, c, m) => Debug("fn-shift-left"); (b, c)},

    Case(Alt+"[1;10A"){(b, c, m) => Debug("alt-shift-up"); (b, c)},
    Case(Alt+"[1;10B"){(b, c, m) => Debug("alt-shift-down"); (b, c)},
    Case(Alt+"[1;10C"){(b, c, m) => Debug("alt-shift-right"); (b, c)},
    Case(Alt+"[1;10D"){(b, c, m) => Debug("alt-shift-left"); (b, c)},

    // Same as the case fn-alt-{up,down} without the shift
    Case(Alt*2+"[5~"){(b, c, m) => Debug("fn-alt-shift-up"); (b, c)},
    Case(Alt*2+"[6~"){(b, c, m) => Debug("fn-alt-shift-down"); (b, c)},
    Case(Alt+"[1;10F"){(b, c, m) => Debug("fn-alt-shift-right"); (b, c)},
    Case(Alt+"[1;10H"){(b, c, m) => Debug("fn-alt-shift-left"); (b, c)}
  )


  def consumeWord(b: Vector[Char], c: Int, delta: Int, offset: Int) = {
    var current = c
    // Move at least one character! Otherwise
    // you get stuck at the end of a word.
    current += delta
    while(b.isDefinedAt(current) && !b(current).isLetterOrDigit) current += delta
    while(b.isDefinedAt(current) && b(current).isLetterOrDigit) current += delta
    current + offset
  }

  def wordLeft(b: Vector[Char], c: Int) = b -> consumeWord(b, c, -1, 1)
  def wordRight(b: Vector[Char], c: Int) = b -> consumeWord(b, c, 1, 0)

}
