package ammonite.terminal

import utest._


object EditTests extends TestSuite{

  val tests = TestSuite{
    val check = Checker(
      width = 5,
      grid = """
        abcd
        e_fgh
        ijkl
      """
    )

    import check._
    'cutting{

      * - check(
        """
        abcd
        _gh
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
        _
        ijkl
        """,
        edit.cutWordRight,
        wordLeft
      )
      * - check(
        """
        _gh
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
        _
        """,
        edit.cutAllRight,
        wordLeft
      )

      * - check (
        """
        abcd
        _gh
        ijkl
        """,
        edit.cutCharLeft
      )
      
      * - check (
        """
        abc_gh
        ijkl
        """,
        edit.cutCharLeft,
        edit.cutCharLeft,
        edit.cutCharLeft
      )
  }
    'pasting{

      * - check(
        """
        abcd
        e_gh
        ijkl
        """,
        edit.cutWordLeft,
        edit.paste
      )
      * - check(
        """
        abcd
        ee_gh
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
        fghfgh_
        ijkl
        """,
        edit.cutWordLeft,
        edit.cutWordRight,
        edit.paste,
        edit.paste
      )
      
      * - check (
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
  }
}
