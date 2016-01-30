package ammonite.terminal

import utest._


object NavigationTests extends TestSuite{

  val tests = TestSuite{
    'simple{
      // Tests for a simple, not-wrap-around
      // grid of characters
      val check = Checker(
        width = 5,
        """
          abcd
          e_fgh
          ijkl
        """

      )

      import check._

      'noop - check(
        """
        abcd
        e_fgh
        ijkl
        """,
        (g, v) => (g, v)
      )

      'upsAndDowns{

        'down - check(
          """
          abcd
          efgh
          i_jkl
          """,
          down
        )
        'up - check(
          """
          a_bcd
          efgh
          ijkl
          """,
          up
        )
        'updown - check(
          """
          abcd
          e_fgh
          ijkl
          """,
          up, down
        )
        'upup - check(
          """
          _abcd
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
          _efgh
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
          _efgh
          ijkl
          """,
          home
        )
      }
    }
    'jagged{
      // tests where the lines of characters
      // are of uneven lengths
      val check = Checker(
        width = 10,
        """
          abcdefg
          hijk
          lm_nopqr
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
        t_uvwxyz
        """,
        down, down
      )
      'truncateBackUp - check(
        """
        abcdefg
        hijk
        l_mnopqr
        s
        tuvwxyz
        """,
        down, down, up, up
      )
      'upup- check(
        """
        ab_cdefg
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
      import check._
      'updown{
        * - {
          check
            .run(up)
            .check(
              """
              abcdefg\
              h_ijk
              lmnopqr\
              s
              tuvwxyz
              """
            )
            .run(up)
            .check(
              """
              a_bcdefg\
              hijk
              lmnopqr\
              s
              tuvwxyz
              """
            )
        }
        * - {
          check
          .run(down)
          .check(
            """
            abcdefg\
            hijk
            lmnopqr\
            s_
            tuvwxyz
            """
          )
          .run(down)
          .check(
            """
            abcdefg\
            hijk
            lmnopqr\
            s
            t_uvwxyz
            """
          )
        }

      }
      'startend{

        * - check(
          """
          abcdefg\
          hijk
          lmnopqr\
          _s
          tuvwxyz
          """,
          end
        )
        * - check(
          """
          abcdefg\
          hijk
          _lmnopqr\
          s
          tuvwxyz
          """,
          home
        )
        * - check(
          """
          abcdefg\
          _hijk
          lmnopqr\
          s
          tuvwxyz
          """,
          up, home
        )
        * - check(
          """
          abcdefg\
          _hijk
          lmnopqr\
          s
          tuvwxyz
          """,
          up, home, home, home
        )
        * - check(
          """
          abcdefg\
          _hijk
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
      val check = Checker(
        width = 10,
        """
          s.dropPref\
          ix(
            b_ase.map\
          (x.toInt)
          )
        """
      )
      import check._

      * - {
        check
          .run(wordLeft)
          .check(
            """
            s.dropPref\
            ix(
              _base.map\
            (x.toInt)
            )
            """
          )
          .run(wordRight)
          .check(
            """
            s.dropPref\
            ix(
              base_.map\
            (x.toInt)
            )
            """
          )
      }
      * - {
        check
          .run(wordLeft, wordLeft)
          .check(
            """
            s._dropPref\
            ix(
              base.map\
            (x.toInt)
            )
            """
          )
          .run(wordLeft)
          .check(
            """
            _s.dropPref\
            ix(
              base.map\
            (x.toInt)
            )
            """
          )
          .run(wordLeft)
          .check(
            """
            _s.dropPref\
            ix(
              base.map\
            (x.toInt)
            )
            """
          )
      }

      * - {
        check
          .run(wordRight)
          .check(
            """
            s.dropPref\
            ix(
              base_.map\
            (x.toInt)
            )
            """
          )
          .run(wordRight)
          .check(
            """
            s.dropPref\
            ix(
              base.map\
            _(x.toInt)
            )
            """
          )
          .run(wordRight)
          .check(
            """
            s.dropPref\
            ix(
              base.map\
            (x_.toInt)
            )
            """
          )
          .run(wordRight)
          .check(
            """
            s.dropPref\
            ix(
              base.map\
            (x.toInt_)
            )
            """
          )
          .run(wordRight)
          .check(
            """
            s.dropPref\
            ix(
              base.map\
            (x.toInt)
            )_
            """
          )
      }
    }

  }
}
