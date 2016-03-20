package ammonite.terminal

import utest._

import scala.Console.{
  RED => R, 
  GREEN => G, 
  BLUE => B,
  YELLOW => Y,
  UNDERLINED => UND, 
  REVERSED => REV, 
  RESET => RES
}

object AnsiStrTests extends TestSuite{
  import Ansi.Str.parse

  /**
    * ANSI escape sequence to reset text color
    */
  val RTC = "\u001b[39m"
  val tests = TestSuite{
    val rgbOps = s"+++$R---$G***$B///"
    val rgb = s"$R$G$B"
    'parsing{
      assert(
        parse(rgbOps).plainText == "+++---***///",
        parse(rgb).plainText == "",
        parse(rgbOps).render == rgbOps + RTC,
        parse(rgb).render == ""
      )
    }

    'concat{
      val concated = (parse(rgbOps) ++ parse(rgbOps)).render
      val expected = rgbOps ++ RTC ++ rgbOps ++ RTC

      assert(concated == expected)
    }

    'split{
      val splits = Seq(
        // Under-shot indexes just get clamped
        (-99,  s"", s"+++$R---$G***$B///$RTC"),
        (-1,  s"", s"+++$R---$G***$B///$RTC"),

        // These are the standard series
        (0,  s"", s"+++$R---$G***$B///$RTC"),
        (1,  s"+", s"++$R---$G***$B///$RTC"),
        (2,  s"++", s"+$R---$G***$B///$RTC"),
        (3,  s"+++", s"$R---$G***$B///$RTC"),
        (4,  s"+++$R-$RTC", s"$R--$G***$B///$RTC"),
        (5,  s"+++$R--$RTC", s"$R-$G***$B///$RTC"),
        (6,  s"+++$R---$RTC", s"$G***$B///$RTC"),
        (7,  s"+++$R---$G*$RTC", s"$G**$B///$RTC"),
        (8,  s"+++$R---$G**$RTC", s"$G*$B///$RTC"),
        (9,  s"+++$R---$G***$RTC", s"$B///$RTC"),
        (10, s"+++$R---$G***$B/$RTC", s"$B//$RTC"),
        (11, s"+++$R---$G***$B//$RTC", s"$B/$RTC"),
        (12, s"+++$R---$G***$B///$RTC", s""),

        // Overshoots just get clamped
        (13, s"+++$R---$G***$B///$RTC", s""),
        (99, s"+++$R---$G***$B///$RTC", s"")
      )
      for((index, expectedLeft0, expectedRight0) <- splits){
        val (splitLeft, splitRight) = parse(rgbOps).splitAt(index)
        val (expectedLeft, expectedRight) = (expectedLeft0, expectedRight0)
        val left = splitLeft.render
        val right = splitRight.render
        assert((left, right) == (expectedLeft, expectedRight))
      }
    }

    'overlay{
      'simple{
        val overlayed = rgbOps.overlay(Ansi.Color.Yellow, 4, 7)
        val expected = s"+++$R-$Y--*$G**$B///$RTC"
        assert(overlayed.render == expected)
      }
      'resetty{
        val resetty = s"+$RES++$R--$RES-$RES$G***$B///"
        val overlayed = resetty.overlay(Ansi.Color.Yellow, 4, 7).render
        val expected = s"+++$R-$Y--*$G**$B///$RTC"
        assert(overlayed == expected)
      }
      'mixedResetUnderline{
        val resetty = s"+$RES++$R--$RES-$UND$G***$B///"
        val overlayed = resetty.overlay(Ansi.Color.Yellow, 4, 7).render toVector
        val expected = s"+++$R-$Y--$UND*$G**$B///$RES" toVector

        assert(overlayed == expected)
      }
      'underlines{
        val resetty = s"$UND#$RES    $UND#$RES"
        'underlineBug{
          val overlayed = resetty.overlay(Ansi.Reversed.On, 0, 2).render
          val expected = s"$UND$REV#$RES$REV $RES   $UND#$RES"
          assert(overlayed == expected)
        }
        'barelyOverlapping{
          val overlayed = resetty.overlay(Ansi.Reversed.On, 0, 1).render
          val expected = s"$UND$REV#$RES    $UND#$RES"
          assert(overlayed == expected)
        }
        'endOfLine{
          val overlayed = resetty.overlay(Ansi.Reversed.On, 5, 6).render
          val expected = s"$UND#$RES    $UND$REV#$RES"
          assert(overlayed == expected)
        }
        'overshoot{
          val overlayed = resetty.overlay(Ansi.Reversed.On, 5, 10).render.toVector
          val expected = s"$UND#$RES    $UND$REV#$RES".toVector
          assert(overlayed == expected)
        }
        'empty{
          val overlayed = resetty.overlay(Ansi.Reversed.On, 0, 0).render
          val expected = s"$UND#$RES    $UND#$RES"
          assert(overlayed == expected)
        }
        'singleContent{
          val overlayed = resetty.overlay(Ansi.Reversed.On, 2, 4).render
          val expected = s"$UND#$RES $REV  $RES $UND#$RES"
          assert(overlayed == expected)

        }
      }
    }
    'colors{
      'reset - Ansi.Color.Reset
      'black - Ansi.Color.Black
      'red - Ansi.Color.Red
      'green - Ansi.Color.Green
      'yellow - Ansi.Color.Yellow
      'blue - Ansi.Color.Blue
      'magenta - Ansi.Color.Magenta
      'cyan - Ansi.Color.Cyan
      'white - Ansi.Color.White
    }
  }
}
