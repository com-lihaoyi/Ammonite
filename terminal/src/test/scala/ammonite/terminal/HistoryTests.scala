package ammonite.terminal

import ammonite.terminal.ReadlineFilters.HistoryFilter
import utest._


object HistoryTests extends TestSuite{

  val history = new HistoryFilter(() => Vector(
    "abcde",
    "abcdefg",
    "abcdefg"
  ))
  val tests = TestSuite{
    val check = Checker(
      width = 50,
      grid = """
        Hell_o
      """
    )
    // Without history, the    
    'noHistory - check(
      """
      _ello
      """,
      check.wordLeft
    )

  }
}
