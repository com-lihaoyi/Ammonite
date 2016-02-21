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
  val tests = TestSuite{
    val rgbOps = s"+++$R---$G***$B///"
    val rgb = s"$R$G$B"
    'parsing{
      assert(
        parse(rgbOps).plainText.mkString == "+++---***///",
        parse(rgb).plainText.mkString == "",
        parse(rgbOps).render.mkString == rgbOps,
        parse(rgb).render.mkString == rgb
      )
    }

    'autoCollapse{
      val frags = parse(s"$R$R Hello$B $B World").fragments
      val expected = Vector(
        Ansi.Red, Ansi.Content(" Hello"),
        Ansi.Blue, Ansi.Content("  World"))
      assert(frags == expected)
    }
    'concat{
      assert((parse(rgbOps) ++ parse(rgbOps)).render.mkString == rgbOps ++ rgbOps)
    }

    'split{
      val splits = Seq(
        // Under-shot indexes just get clamped
        (-99,  s"", s"+++$R---$G***$B///"),
        (-1,  s"", s"+++$R---$G***$B///"),

        // These are the standard series
        (0,  s"", s"+++$R---$G***$B///"),
        (1,  s"+", s"++$R---$G***$B///"),
        (2,  s"++", s"+$R---$G***$B///"),
        (3,  s"+++$R", s"$R---$G***$B///"),
        (4,  s"+++$R-", s"$R--$G***$B///"),
        (5,  s"+++$R--", s"$R-$G***$B///"),
        (6,  s"+++$R---$G", s"$G***$B///"),
        (7,  s"+++$R---$G*", s"$G**$B///"),
        (8,  s"+++$R---$G**", s"$G*$B///"),
        (9,  s"+++$R---$G***$B", s"$B///"),
        (10, s"+++$R---$G***$B/", s"$B//"),
        (11, s"+++$R---$G***$B//", s"$B/"),
        (12, s"+++$R---$G***$B///", s"$B"),

        // Overshoots just get clamped
        (13, s"+++$R---$G***$B///", s"$B"),
        (99, s"+++$R---$G***$B///", s"$B")
      )
      for((index, expectedLeft0, expectedRight0) <- splits){
        val (splitLeft, splitRight) = parse(rgbOps).splitAt(index)
        val (expectedLeft, expectedRight) = (expectedLeft0.toVector, expectedRight0.toVector)
        val left = splitLeft.render
        val right = splitRight.render
        assert((left, right) == (expectedLeft, expectedRight))
      }
    }

    'overlay{
      'simple{
        val overlayed = rgbOps.overlay(Ansi.Yellow, 4, 7)
        val expected = s"+++$R-$Y--*$G**$B///"
        assert(overlayed.toString == expected)
      }
      'resetty{
        val resetty = s"+$RES++$R--$RES-$RES$G***$B///"
        val overlayed = resetty.overlay(Ansi.Yellow, 4, 7).toString
        val expected = s"+++$R-$Y--*$G**$B///"
        assert(overlayed == expected)
      }
      'mixedResetUnderline{
        val resetty = s"+$RES++$R--$RES-$UND$G***$B///"
        val overlayed = resetty.overlay(Ansi.Yellow, 4, 7).toString
        val expected = s"+++$R-$Y--$UND*$G**$B///"
        assert(overlayed == expected)
      }
      'underlines{
        val resetty = s"$UND#$RES    $UND#$RES"
        'underlineBug{
          val overlayed = resetty.overlay(Ansi.Reversed, 0, 2).toString
          val expected = s"$UND$REV#$RES$REV $RES   $UND#$RES"
          assert(overlayed == expected)
        }
        'barelyOverlapping{
          val overlayed = resetty.overlay(Ansi.Reversed, 0, 1).toString
          val expected = s"$UND$REV#$RES$UND$RES    $UND#$RES"
          assert(overlayed == expected)
        }
        'endOfLine{
          val overlayed = resetty.overlay(Ansi.Reversed, 5, 6).toString
          val expected = s"$UND#$RES    $REV$UND#$RES$UND$RES"
          assert(overlayed == expected)
        }
        'overshoot{
          val overlayed = resetty.overlay(Ansi.Reversed, 5, 10).toString
          val expected = s"$UND#$RES    $REV$UND#$RES$REV"
          assert(overlayed == expected)
        }
        'empty{
          val overlayed = resetty.overlay(Ansi.Reversed, 0, 0).toString
          assert(overlayed == resetty)
        }
        'singleContent{
          val overlayed = resetty.overlay(Ansi.Reversed, 2, 4).toString
          val expected = s"$UND#$RES $REV  $RES $UND#$RES"
          assert(overlayed == expected)
        }
      }
    }
  }
}
