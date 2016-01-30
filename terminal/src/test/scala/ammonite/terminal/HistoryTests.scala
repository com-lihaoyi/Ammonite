package ammonite.terminal

import ammonite.terminal.ReadlineFilters.HistoryFilter
import utest._


object HistoryTests extends TestSuite{


  val tests = TestSuite{
    val history = new HistoryFilter(() => Vector(
      "abcde",
      "abcdefg",
      "abcdefg",
      "abcdefghi"
    ))
    def checker(start: String)= Checker(
      width = 50,
      grid = start
    )

    // When you page up with something that doesn't exist in history, it
    // should preserve the current line and move your cursor to the end
    'noHistory{
      checker("Hell_o")
        .run(history.startHistory)
        .check("Hello_")
    }


    'ctrlR {
      // If you hit Ctrl-R on an empty line, it shows a nice help message
      'empty{
        checker("_")
          .run(history.ctrlR)
          .check(s"_${Console.BLUE} ...press any key to search, `up` for more results${Console.RESET}")
          .run(history.printableChar('c'))
          .check("abc_e")
          .run(history.backspace)
          .check(s"_${Console.BLUE} ...press any key to search, `up` for more results${Console.RESET}")
      }
    }

    // When you page up with something that doesn't exist in history, it
    // should preserve the current line and move your cursor to the end
    'ups{
        checker("abc")
          .run(history.startHistory)
          .check("abc_e")
          .run(history.upSkip)
          .check("abc_efg")
          // Pressing up should treat the duplicates entries in history as
          // one entry when navigating through them
          .run(history.upSkip)
          .check("abc_efghi")
          // You should be able to cycle through the end of the history and
          // back to the original command
          .run(history.upSkip)
          .check("abc_")
          .run(history.upSkip)
          .check("abc_e")
    }

    'down{
      checker("abc_")
        .run(history.startHistory)
        .check("abc_e")
        .run(history.down)
        .check("abc_")
        // `down` doesn't wrap around like `up` does
        .run(history.down)
        .check("abc_")

    }


  }
}
