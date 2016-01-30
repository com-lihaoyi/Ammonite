package ammonite.terminal

import ammonite.terminal.ReadlineFilters.HistoryFilter
import utest._


object HistoryTests extends TestSuite{

  val emptySearchMessage = s"_${Console.BLUE} ...press any key to search, `up` for more results${Console.RESET}"
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
          .check(emptySearchMessage)
          .run(history.printableChar('c'))
          .check("abc_e")
          .run(history.backspace)
          .check(s"_${Console.BLUE} ...press any key to search, `up` for more results${Console.RESET}")
      }
      'nonEmpty{
        checker("cd_")
          .run(history.ctrlR)
          .check("abcd_")
          .run(history.ctrlR) // `ctrlR` should behave like `up` if called multiple times
          .check("abcd_fg")
          .run(history.printableChar('e'))
          .run(history.printableChar('f'))
          .run(history.printableChar('g'))
          .run(history.printableChar('h'))
          .check("abcdefgh_")
      }
    }
    'historyNoSearch{
      checker("_")
        .run(history.startHistory)
        .check("abcde_")
        .run(history.up)
        .check("abcdefg_")
        .run(history.up)
        .check("abcdefghi_")
    }

    // When you page up with something that doesn't exist in history, it
    // should preserve the current line and move your cursor to the end
    'ups{
      checker("abc")
        .run(history.startHistory)
        .check("abc_e")
        .run(history.up)
        .check("abc_efg")
        // Pressing up should treat the duplicates entries in history as
        // one entry when navigating through them
        .run(history.up)
        .check("abc_efghi")
        // You should be able to cycle through the end of the history and
        // back to the original command
        .run(history.up)
        .check("abc_")
        .run(history.up)
        .check("abc_e")
    }
    'upsTyping{
      checker("de")
        .run(history.startHistory)
        .check("abcde_")
        // Entering more characters should refine the search
        .run(history.printableChar('f'))
        .check("abcdef_")
        // Deleting characters and typing them back in should do nothing
        // except move the cursor
        .run(history.backspace)
        .check("abcde_g")
        .run(history.backspace)
        .check("abcd_fg")
        .run(history.printableChar('e'))
        .check("abcde_g")
        .run(history.printableChar('f'))
        .check("abcdef_")
        // Deleting all characters from search should show the empty search message
        .run(history.backspace)
        .run(history.backspace)
        .run(history.backspace)
        .run(history.backspace)
        .check(emptySearchMessage)
        // But typing something in should make it go away and
        // find something relevant to show you
        .run(history.printableChar('i'))
        .check("abcdefghi_")

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
