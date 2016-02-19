package ammonite.terminal

import utest._

import scala.Console._

object AnsiStrTests extends TestSuite{
  import Ansi.Str.parse
  val tests = TestSuite{
    val rgbOps = s"+++$RED---$GREEN***$BLUE///"
    val rgb = s"$RED$GREEN$BLUE"
    'parsing{
      assert(
        parse(rgbOps).plainText.mkString == "+++---***///",
        parse(rgb).plainText.mkString == "",
        parse(rgbOps).render.mkString == rgbOps,
        parse(rgb).render.mkString == rgb
      )
    }

    'concat{
      assert((parse(rgbOps) ++ parse(rgbOps)).render.mkString == rgbOps ++ rgbOps)
    }

    'split{
      val splits = Seq(
        // Under-shot indexes just get clamped
        (-99,  s"", s"+++$RED---$GREEN***$BLUE///"),
        (-1,  s"", s"+++$RED---$GREEN***$BLUE///"),

        // These are the standard series
        (0,  s"", s"+++$RED---$GREEN***$BLUE///"),
        (1,  s"+", s"++$RED---$GREEN***$BLUE///"),
        (2,  s"++", s"+$RED---$GREEN***$BLUE///"),
        (3,  s"+++$RED", s"$RED---$GREEN***$BLUE///"),
        (4,  s"+++$RED-", s"$RED--$GREEN***$BLUE///"),
        (5,  s"+++$RED--", s"$RED-$GREEN***$BLUE///"),
        (6,  s"+++$RED---$GREEN", s"$GREEN***$BLUE///"),
        (7,  s"+++$RED---$GREEN*", s"$GREEN**$BLUE///"),
        (8,  s"+++$RED---$GREEN**", s"$GREEN*$BLUE///"),
        (9,  s"+++$RED---$GREEN***$BLUE", s"$BLUE///"),
        (10, s"+++$RED---$GREEN***$BLUE/", s"$BLUE//"),
        (11, s"+++$RED---$GREEN***$BLUE//", s"$BLUE/"),
        (12, s"+++$RED---$GREEN***$BLUE///", s"$BLUE"),

        // Overshoots just get clamped
        (13, s"+++$RED---$GREEN***$BLUE///", s"$BLUE"),
        (99, s"+++$RED---$GREEN***$BLUE///", s"$BLUE")
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
        val expected = s"+++$RED-$YELLOW--*$GREEN**$BLUE///"
        assert(overlayed.toString == expected)
      }
      'resetty{
        val resetty = s"+$RESET++$RED--$RESET-$RESET$GREEN***$BLUE///"
        val overlayed = resetty.overlay(Ansi.Yellow, 4, 7).toString
        val expected = s"+++$RED-$YELLOW--*$GREEN**$BLUE///"
        assert(overlayed == expected)
      }
      'mixedResetUnderline{
        val resetty = s"+$RESET++$RED--$RESET-$UNDERLINED$GREEN***$BLUE///"
        val overlayed = resetty.overlay(Ansi.Yellow, 4, 7).toString
        val expected = s"+++$RED-$YELLOW--$UNDERLINED*$GREEN**$BLUE///"
        assert(overlayed == expected)
      }
    }
  }
}
