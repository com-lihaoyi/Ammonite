package ammonite.terminal

import ammonite.terminal.filters.ReadlineFilters
import utest._

object EditTests extends TestSuite {

  val tests = TestSuite {
    val check = Checker(
      width = 5,
      grid = """
        abcd
        e_fgh
        ijkl
      """
    )

    import check._
    'cutting {

      * - check(
        """
        abcd
        _fgh
        ijkl
        """,
        edit.cutWordLeft
      )
      * - check(
        """
        abcd
        e_
        ijkl
        """,
        edit.cutWordRight
      )
      * - check(
        """
        abcd
        _e
        ijkl
        """,
        edit.cutWordRight,
        wordLeft
      )
      * - check(
        """
        _fgh
        ijkl
        """,
        edit.cutAllLeft
      )
      * - check(
        """
        abcd
        e_
        """,
        edit.cutAllRight
      )

      * - check(
        """
        abcd
        _e
        """,
        edit.cutAllRight,
        wordLeft
      )

      * - check(
        """
        abcd
        _fgh
        ijkl
        """,
        edit.cutCharLeft
      )

      * - check(
        """
        abc_fgh
        ijkl
        """,
        edit.cutCharLeft,
        edit.cutCharLeft,
        edit.cutCharLeft
      )
    }
    'pasting {

      * - check(
        """
        abcd
        e_fgh
        ijkl
        """,
        edit.cutWordLeft,
        edit.paste
      )
      * - check(
        """
        abcd
        ee_fgh
        ijkl
        """,
        edit.cutWordLeft,
        edit.paste,
        edit.paste
      )
      * - check(
        """
        abcd
        efghfgh_
        ijkl
        """,
        edit.cutWordRight,
        edit.paste,
        edit.paste
      )
      * - check(
        """
        abcd
        efghefgh_
        ijkl
        """,
        edit.cutWordLeft,
        edit.cutWordRight,
        edit.paste,
        edit.paste
      )

      * - check(
        """
        abcd
        fgh_
        ijkl
        """,
        edit.cutWordRight,
        edit.cutCharLeft,
        edit.paste
      )
    }
    'transpose {
      val check = Checker(
        width = 7,
        """
          abcdefg\
          hijk
          l_mnopqr\
          s
          tuvwxyz
        """
      )
      'words {
        // Trying to transpose something at the start of the input does nothing
        'start - Checker(width = 20, "_abc  defg    hijkl")
          .run(ReadlineFilters.transposeWord)
          .check("_abc  defg    hijkl")
          .run((b, c) => (b, c + 2))
          // even if not at the start, but in the first word, still do nothing
          .check("ab_c  defg    hijkl")
          .run(ReadlineFilters.transposeWord)
          .check("ab_c  defg    hijkl")
          .run((b, c) => (b, c + 1))
          .check("abc_  defg    hijkl")
          // only at end of first word do we start transposing
          .run(ReadlineFilters.transposeWord)
          .check("defg  abc_    hijkl")

        // Reverse works anywhere inside the last word
        'end - Checker(width = 20, "abc  defg    h_ijkl")
          .run(ReadlineFilters.transposeWord)
          .check("abc  hijkl    defg_")
          .run(ReadlineFilters.transposeWord)
          .check("abc  defg    hijkl_")
          .run(ReadlineFilters.transposeWord)
          .check("abc  hijkl    defg_")

        'middle - Checker(width = 20, "abc _ defg    hijkl")
          .run(ReadlineFilters.transposeWord)
          .check("defg  abc_    hijkl")
          .run(ReadlineFilters.transposeWord)
          .check("defg  hijkl    abc_")
      }
      'letter {
        // Trying to transpose something at the start of the input does nothing
        'start - Checker(width = 20, "_abcd").run(ReadlineFilters.transposeLetter).check("_abcd")

        // Trying to transpose at the end of the input transposes the *two*
        // previous characters, rather than the 1 char before and 1 char after
        'end - Checker(width = 20, "a_bcd")
          .run(ReadlineFilters.transposeLetter)
          .check("ba_cd")
          .run(ReadlineFilters.transposeLetter)
          .check("bca_d")
          .run(ReadlineFilters.transposeLetter)
          .check("bcda_")
          .run(ReadlineFilters.transposeLetter)
          .check("bcad_")
          .run(ReadlineFilters.transposeLetter)
          .check("bcda_")

        'middling - check
        // single movement
          .run(ReadlineFilters.transposeLetter)
          .check("""
            abcdefg\
            hijk
            ml_nopqr\
            s
            tuvwxyz
          """)
          .run( // move more
               ReadlineFilters.transposeLetter,
               ReadlineFilters.transposeLetter,
               ReadlineFilters.transposeLetter,
               ReadlineFilters.transposeLetter)
          .check("""
            abcdefg\
            hijk
            mnopql_r\
            s
            tuvwxyz
          """)
          .run(ReadlineFilters.transposeLetter)
          .check("""
            abcdefg\
            hijk
            mnopqrl_\
            s
            tuvwxyz
          """)
          .run(ReadlineFilters.transposeLetter)
          .check("""
            abcdefg\
            hijk
            mnopqrsl_
            tuvwxyz
           """)
          // Make sure it crosses newlines properly
          .run(ReadlineFilters.transposeLetter)
          .check("""
            abcdefg\
            hijk
            mnopqrs
            l_tuvwxyz
           """)
          .run(ReadlineFilters.transposeLetter)
          .check("""
            abcdefg\
            hijk
            mnopqrs
            tl_uvwxyz
           """)
      }
    }
  }
}
