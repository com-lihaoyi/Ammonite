package ammonite.terminal

import utest._


object HistoryTests extends TestSuite{


  val tests = TestSuite{
    val history = new HistoryFilter(
      () => Vector(
        "abcde",
        "abcdefg",
        "abcdefg",
        "abcdefghi"
      ),
      "",
      ""
    )
    def checker(start: String)= Checker(
      width = 50,
      grid = start
    )

    // When you page up with something that doesn't exist in history, it
    // should preserve the current line and move your cursor to the end
    'noHistory{
      checker("Hell_o")
        .runMsg(history.startHistory)
        .check("Hello_")
        .checkMsg(HistoryFilter.cannotFindSearchMessage)
        .runMsg(history.up)
        .check("Hello_")
        .checkMsg(HistoryFilter.cannotFindSearchMessage)
    }


    'ctrlR {
      // If you hit Ctrl-R on an empty line, it shows a nice help message
      'empty{
        checker("_")
          .runMsg(history.ctrlR)
          .check("_")
          .checkMsg(HistoryFilter.emptySearchMessage)
          .runMsg(history.printableChar('c'))
          .check("abc_de")
          .runMsg(history.backspace)
          .check("_")
          .checkMsg(HistoryFilter.emptySearchMessage)
      }
      'nonEmpty{
        checker("cd_")
          .runMsg(history.ctrlR)
          .check("abcd_e")
          .runMsg(history.ctrlR) // `ctrlR` should behave like `up` if called multiple times
          .check("abcd_efg")
          .runMsg(history.printableChar('e'))
          .runMsg(history.printableChar('f'))
          .runMsg(history.printableChar('g'))
          .runMsg(history.printableChar('h'))
          .check("abcdefgh_i")
      }
    }
    'historyNoSearch{
      checker("_")
        .runMsg(history.startHistory)
        .check("abcde_")
        .runMsg(history.up)
        .check("abcdefg_")
        .runMsg(history.up)
        .check("abcdefghi_")
    }

    // When you page up with something that doesn't exist in history, it
    // should preserve the current line and move your cursor to the end
    'ups{
      checker("abc_")
        .runMsg(history.startHistory)
        .check("abc_de")
        .runMsg(history.up)
        .check("abc_defg")
        // Pressing up should treat the duplicates entries in history as
        // one entry when navigating through them
        .runMsg(history.up)
        .check("abc_defghi")
        // You should be able to cycle through the end of the history and
        // back to the original command
        .runMsg(history.up)
        .check("abc_")
        .checkMsg(HistoryFilter.cannotFindSearchMessage)
        .runMsg(history.up)
        .check("abc_de")
    }
    'upsTyping{
      checker("de_")
        .runMsg(history.startHistory)
        .check("abcde_")
        // Entering more characters should refine the search
        .runMsg(history.printableChar('f'))
        .check("abcdef_g")
        // Deleting characters and typing them back in should do nothing
        // except move the cursor
        .runMsg(history.backspace)
        .check("abcde_fg")
        .runMsg(history.backspace)
        .check("abcd_efg")
        .runMsg(history.printableChar('e'))
        .check("abcde_fg")
        .runMsg(history.printableChar('f'))
        .check("abcdef_g")
        // Deleting all characters from search should show the empty search message
        .runMsg(history.backspace)
        .runMsg(history.backspace)
        .runMsg(history.backspace)
        .runMsg(history.backspace)
        .check("_")
        .checkMsg(HistoryFilter.emptySearchMessage)
        // But typing something in should make it go away and
        // find something relevant to show you
        .runMsg(history.printableChar('i'))
        .check("abcdefghi_")

    }
    'cannotFindSearch{
      checker("abcde_")
        .runMsg(history.startHistory)
        .check("abcde_")
        .runMsg(history.printableChar('Z'))
        .check("abcdeZ_")
        .checkMsg(HistoryFilter.cannotFindSearchMessage)
    }
    'down{
      checker("abc_")
        .runMsg(history.startHistory)
        .check("abc_de")
        .runMsg(history.down)
        .check("abc_")
        // `down` doesn't wrap around like `up` does
        .runMsg(history.down)
        .check("abc_")

    }


  }
}
