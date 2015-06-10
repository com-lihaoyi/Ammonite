package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object NavigationTests extends TestSuite{
  def normalize(s: String) = {
    val lines = s.lines.toVector
    val min = lines.map(_.indexWhere(_ != ' '))
      .filter(_ != -1)
      .min
    lines.drop(1).dropRight(1).map(_.drop(min)).mkString("\n")

  }
  def check(grid0: String,
            start0: String,
            end0: String,
            actions: (Int => Int)*) = {


    val grid = normalize(grid0)
    val start = normalize(start0)
    val end = normalize(end0)

    val startCursor = start.indexOf('_')
    val endCursor = actions.foldLeft(startCursor)(
      (a, f) => math.min(grid.length, math.max(0, f(a)))
    )

    val endState =
      if (endCursor == grid.length) grid + '_'
      else if (grid(endCursor) != '\n') grid.updated(endCursor, '_')
      else{
        val (a, b) = grid.splitAt(endCursor)
        a + '_' + b
      }

    println(startCursor + " -> " + endCursor)
    assert(end == endState)
  }
  val tests = TestSuite{
    'simple{
      // Tests for a simple, not-wrap-around
      // grid of characters
      val width = 5
      val grid =
        """
        abcd
        efgh
        ijkl
        """
      val gridv = normalize(grid).toVector

      val start =
        """
        abcd
        e_gh
        ijkl
        """

      def check(end: String, actions: (Int => Int)*) = {
        NavigationTests.check(grid, start, end, actions:_*)
      }
      'noop - check(
        """
        abcd
        e_gh
        ijkl
        """,
        x => x
      )
      val down = Term.moveDown(gridv, _: Int, width)
      val up = Term.moveUp(gridv, _: Int, width)
      'upsAndDowns{

        'down - check(
          """
          abcd
          efgh
          i_kl
          """,
          down
        )
        'up - check(
          """
          a_cd
          efgh
          ijkl
          """,
          up
        )
        'updown - check(
          """
          abcd
          e_gh
          ijkl
          """,
          up, down
        )
        'upup - check(
          """
          _bcd
          efgh
          ijkl
          """,
          up, up
        )
        'downdown- check(
          """
          abcd
          efgh
          ijkl_
          """,
          down, down
        )
        'upupdown - check(
          """
          abcd
          _fgh
          ijkl
          """,
          up, up, down
        )
        'downdownup - check(
          """
          abcd
          efgh_
          ijkl
          """,
          down, down, up
        )
      }
      'startEnd{
        'end - check(
          """
          abcd
          efgh_
          ijkl
          """,
          Term.moveStartEnd(gridv, _, 1)
        )
        'start - check(
          """
          abcd
          _fgh
          ijkl
          """,
          Term.moveStartEnd(gridv, _, 0)
        )
      }
    }
    'jagged{
      // tests where the lines of characters
      // are of uneven lengths
      val width = 10
      val grid =
        """
        abcdefg
        hijk
        lmnopqr
        s
        tuvwxyz
        """
      val gridv = normalize(grid).toVector

      val start =
        """
        abcdefg
        hijk
        lm_opqr
        s
        tuvwxyz
        """

      val down = Term.moveDown(gridv, _: Int, width)
      val up = Term.moveUp(gridv, _: Int, width)

      def check(end: String, actions: (Int => Int)*) = {
        NavigationTests.check(grid, start, end, actions:_*)
      }
      'truncate - check(
        """
        abcdefg
        hijk
        lmnopqr
        s
        t_vwxyz
        """,
        down, down
      )
      'truncateBackUp - check(
        """
        abcdefg
        hijk
        l_nopqr
        s
        tuvwxyz
        """,
        down, down, up, up
      )
      'upup- check(
        """
        ab_defg
        hijk
        lmnopqr
        s
        tuvwxyz
        """,
        up, up
      )
      'endup- check(
        """
        abcdefg
        hijk_
        lmnopqr
        s
        tuvwxyz
        """,
        Term.moveStartEnd(gridv, _, 1), up
      )
    }
  }
}
