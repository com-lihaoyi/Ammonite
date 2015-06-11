package ammonite.terminal

import utest._


object NavigationTests extends TestSuite{
  def normalize(s: String) = {
    val lines = s.lines.toVector
    val min = lines.map(_.indexWhere(_ != ' '))
      .filter(_ != -1)
      .min
    lines.drop(1).dropRight(1).map(_.drop(min)).mkString("\n").replace("\\\n", "")

  }
  def check0(grid0: String,
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
  class Checker(width: Int, grid: String, start: String){
    val gridv = normalize(grid).toVector
    def apply(end: String, actions: (Int => Int)*) = {
      NavigationTests.check0(grid, start, end, actions:_*)
    }
    val down = Term.moveDown(gridv, _: Int, width)
    val up = Term.moveUp(gridv, _: Int, width)
    val home = Term.moveStart(gridv, _: Int, width)
    val end = Term.moveEnd(gridv, _: Int, width)
    val wordLeft = Term.consumeWord(gridv, _: Int, -1, 1)
    val wordRight = Term.consumeWord(gridv, _: Int, 1, 0)
  }
  val tests = TestSuite{
    'simple{
      // Tests for a simple, not-wrap-around
      // grid of characters
      val check = new Checker(
        width = 5,
        grid = """
          abcd
          efgh
          ijkl
        """ ,
        start = """
          abcd
          e_gh
          ijkl
        """

      )

      import check._

      'noop - check(
        """
        abcd
        e_gh
        ijkl
        """,
        x => x
      )

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
          end
        )
        'start - check(
          """
          abcd
          _fgh
          ijkl
          """,
          home
        )
      }
    }
    'jagged{
      // tests where the lines of characters
      // are of uneven lengths
      val check = new Checker(
        width = 10,
        grid = """
          abcdefg
          hijk
          lmnopqr
          s
          tuvwxyz
        """,
        start = """
          abcdefg
          hijk
          lm_opqr
          s
          tuvwxyz
        """
      )

      import check._

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
        end, up
      )
    }
    'wrapping{
      // tests where some lines are so long that they start
      // wrapping onto the next ones. Navigating around they
      // should behave like separate lines
      val check = new Checker(
        width = 7,
        grid = """
          abcdefg\
          hijk
          lmnopqr\
          s
          tuvwxyz
        """,
        start = """
          abcdefg\
          hijk
          l_nopqr\
          s
          tuvwxyz
        """
      )
      import check._
      'updown{
        check(
          """
          abcdefg\
          h_jk
          lmnopqr\
          s
          tuvwxyz
          """,
          up
        )
        check(
          """
          a_cdefg\
          hijk
          lmnopqr\
          s
          tuvwxyz
          """,
          up, up
        )
        check(
          """
          abcdefg\
          hijk
          lmnopqr\
          s_
          tuvwxyz
          """,
          down
        )
        check(
          """
          abcdefg\
          hijk
          lmnopqr\
          s
          t_vwxyz
          """,
          down, down
        )
      }
      'startend{

        * - check(
          """
          abcdefg\
          hijk
          lmnopqr\
          _
          tuvwxyz
          """,
          end
        )
        * - check(
          """
          abcdefg\
          hijk
          _mnopqr\
          s
          tuvwxyz
          """,
          home
        )
        * - check(
          """
          abcdefg\
          _ijk
          lmnopqr\
          s
          tuvwxyz
          """,
          up, home
        )
        * - check(
          """
          abcdefg\
          _ijk
          lmnopqr\
          s
          tuvwxyz
          """,
          up, home, home, home
        )
        * - check(
          """
          abcdefg\
          _ijk
          lmnopqr\
          s
          tuvwxyz
          """,
          up, up, end
        )
      }
    }
    'wordnav{
      // Tests of word-by-word navigation
      val check = new Checker(
        width = 10,
        grid = """
          s.dropPref\
          ix(
            base.map\
          (x.toInt)
          )
        """,
        start = """
          s.dropPref\
          ix(
            b_se.map\
          (x.toInt)
          )
        """
      )
      import check._

      * - check(
        """
          s.dropPref\
          ix(
            _ase.map\
          (x.toInt)
          )
        """,
        wordLeft
      )
      * - check(
        """
          s.dropPref\
          ix(
            base_map\
          (x.toInt)
          )
        """,
        wordLeft, wordRight
      )
      * - check(
        """
          s._ropPref\
          ix(
            base.map\
          (x.toInt)
          )
        """,
        wordLeft, wordLeft
      )
      * - check(
        """
          s.dropPref\
          ix(
            base_map\
          (x.toInt)
          )
        """,
        wordRight
      )
      * - check(
        """
          s.dropPref\
          ix(
            base.map\
          _x.toInt)
          )
        """,
        wordRight, wordRight
      )

    }

  }
}
