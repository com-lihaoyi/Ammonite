package ammonite.terminal

import ammonite.terminal.filters.ReadlineFilters
import utest._

object EditTests extends TestSuite {

  val tests = Tests {
    val check = Checker(
      width = 5,
      grid = """
        abcd
        e_fgh
        ijkl
      """
    )

    import check._
    test("cutting") {

      test - check(
        """
        abcd
        _fgh
        ijkl
        """,
        edit.cutWordLeft
      )
      test - check(
        """
        abcd
        e_
        ijkl
        """,
        edit.cutWordRight
      )
      test - check(
        """
        abcd
        _e
        ijkl
        """,
        edit.cutWordRight,
        wordLeft
      )
      test - check(
        """
        abcd
        _fgh
        ijkl
        """,
        edit.cutLineLeft
      )
      test - check(
        """
        abcd_fgh
        ijkl
        """,
        edit.cutLineLeft,
        edit.cutLineLeft
      )
      test - check(
        """
        _fgh
        ijkl
        """,
        edit.cutLineLeft,
        edit.cutLineLeft,
        edit.cutLineLeft
      )
      test - check(
        """
        abcd
        e_
        ijkl
        """,
        edit.cutLineRight
      )
      test - check(
        """
        abcd
        e_ijkl
        """,
        edit.cutLineRight,
        edit.cutLineRight
      )
      test - check(
        """
        abcd
        e_
        """,
        edit.cutLineRight,
        edit.cutLineRight,
        edit.cutLineRight
      )

      test - check(
        """
        abcd
        _fgh
        ijkl
        """,
        edit.cutCharLeft
      )

      test - check(
        """
        abc_fgh
        ijkl
        """,
        edit.cutCharLeft,
        edit.cutCharLeft,
        edit.cutCharLeft
      )
    }
    test("pasting") {

      test - check(
        """
        abcd
        e_fgh
        ijkl
        """,
        edit.cutWordLeft,
        edit.paste
      )
      test - check(
        """
        abcd
        ee_fgh
        ijkl
        """,
        edit.cutWordLeft,
        edit.paste,
        edit.paste
      )
      test - check(
        """
        abcd
        efghfgh_
        ijkl
        """,
        edit.cutWordRight,
        edit.paste,
        edit.paste
      )
      test - check(
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

      test - check(
        """
        abcd
        fgh_
        ijkl
        """,
        edit.cutWordRight,
        edit.cutCharLeft,
        edit.paste
      )
      test - check(
        """
        abcd
        e_fgh
        ijkl
        """,
        edit.cutLineLeft,
        edit.paste
      )
      test - check(
        """
        abcd
        e_fgh
        ijkl
        """,
        edit.cutLineLeft,
        edit.cutLineLeft,
        edit.paste
      )
      test - check(
        """
        abcd
        e_fgh
        ijkl
        """,
        edit.cutLineLeft,
        edit.cutLineLeft,
        edit.cutLineLeft,
        edit.paste
      )
      test - check(
        """
        abcd
        efgh_
        ijkl
        """,
        edit.cutLineRight,
        edit.paste
      )
      test - check(
        """
        abcd
        efgh
        _ijkl
        """,
        edit.cutLineRight,
        edit.cutLineRight,
        edit.paste
      )
      test - check(
        """
        abcd
        efgh
        ijkl_
        """,
        edit.cutLineRight,
        edit.cutLineRight,
        edit.cutLineRight,
        edit.paste
      )
    }
    test("transpose") {
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
      test("words") {
        // Trying to transpose something at the start of the input does nothing
        test("start") - Checker(width = 20, "_abc  defg    hijkl")
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
        test("end") - Checker(width = 20, "abc  defg    h_ijkl")
          .run(ReadlineFilters.transposeWord)
          .check("abc  hijkl    defg_")
          .run(ReadlineFilters.transposeWord)
          .check("abc  defg    hijkl_")
          .run(ReadlineFilters.transposeWord)
          .check("abc  hijkl    defg_")

        test("middle") - Checker(width = 20, "abc _ defg    hijkl")
          .run(ReadlineFilters.transposeWord)
          .check("defg  abc_    hijkl")
          .run(ReadlineFilters.transposeWord)
          .check("defg  hijkl    abc_")
      }
      test("letter") {
        // Trying to transpose something at the start of the input does nothing
        test("start") - Checker(width = 20, "_abcd")
          .run(ReadlineFilters.transposeLetter)
          .check("_abcd")

        // Trying to transpose at the end of the input transposes the *two*
        // previous characters, rather than the 1 char before and 1 char after
        test("end") - Checker(width = 20, "a_bcd")
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

        test("middling") - check
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
            ReadlineFilters.transposeLetter
          )
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
