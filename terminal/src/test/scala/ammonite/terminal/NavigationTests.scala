package ammonite.terminal

import utest._


object NavigationTests extends TestSuite{

  val tests = Tests{
    test("simple"){
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

      test("noop") - check(
        """
        abcd
        e_fgh
        ijkl
        """,
        (g, v) => (g, v)
      )

      test("upsAndDowns"){

        test("down") - check(
          """
          abcd
          efgh
          i_jkl
          """,
          down
        )
        test("up") - check(
          """
          a_bcd
          efgh
          ijkl
          """,
          up
        )
        test("updown") - check(
          """
          abcd
          e_fgh
          ijkl
          """,
          up, down
        )
        test("upup") - check(
          """
          _abcd
          efgh
          ijkl
          """,
          up, up
        )
        test("downdown") - check(
          """
          abcd
          efgh
          ijkl_
          """,
          down, down
        )
        test("upupdown") - check(
          """
          abcd
          _efgh
          ijkl
          """,
          up, up, down
        )
        test("downdownup") - check(
          """
          abcd
          efgh_
          ijkl
          """,
          down, down, up
        )
      }
      test("startEnd"){
        test("end") - check(
          """
          abcd
          efgh_
          ijkl
          """,
          end
        )
        test("start") - check(
          """
          abcd
          _efgh
          ijkl
          """,
          home
        )
      }
    }
    test("jagged"){
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

      test("truncate") - check(
        """
        abcdefg
        hijk
        lmnopqr
        s
        t_uvwxyz
        """,
        down, down
      )
      test("truncateBackUp") - check(
        """
        abcdefg
        hijk
        l_mnopqr
        s
        tuvwxyz
        """,
        down, down, up, up
      )
      test("upup") - check(
        """
        ab_cdefg
        hijk
        lmnopqr
        s
        tuvwxyz
        """,
        up, up
      )
      test("endup") - check(
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
    test("wrapping"){
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
      test("updown"){
        test{
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
        test{
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
      test("startend"){

        test - check(
          """
          abcdefg\
          hijk
          lmnopqr\
          _s
          tuvwxyz
          """,
          end
        )
        test - check(
          """
          abcdefg\
          hijk
          _lmnopqr\
          s
          tuvwxyz
          """,
          home
        )
        test - check(
          """
          abcdefg\
          _hijk
          lmnopqr\
          s
          tuvwxyz
          """,
          up, home
        )
        test - check(
          """
          abcdefg\
          _hijk
          lmnopqr\
          s
          tuvwxyz
          """,
          up, home, home, home
        )
        test - check(
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
    test("wordnav"){
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


      test("leftRight") - check
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

      test("manyLeft") - check
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


      test("manyRight") - check
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

      test("oneOffs") - check
        .run((b, c) => (b, c+2))
        .check(
          """
          s.dropPref\
          ix(
            bas_e.map\
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
        .run(wordRight)
        .check(
          """
          s.dropPref\
          ix(
            base.map_\
          (x.toInt)
          )
          """
        )
        .run((b, c) => (b, c-2))
        .check(
          """
          s.dropPref\
          ix(
            base.m_ap\
          (x.toInt)
          )
          """
        )
        .run(wordLeft)
        .check(
          """
          s.dropPref\
          ix(
            base._map\
          (x.toInt)
          )
          """
        )
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
    }

  }
}
