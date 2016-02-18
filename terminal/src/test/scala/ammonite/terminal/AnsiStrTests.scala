package ammonite.terminal

import utest._

import scala.Console._

object AnsiStrTests extends TestSuite{
  import AnsiStr.parse
  val tests = TestSuite{
    val rgbOps = s"+1+$RED-2-$GREEN*3*$BLUE/4/"
    val rgb = s"$RED$GREEN$BLUE"
    'parsing{
      assert(
        parse(rgbOps).plainText.mkString == "+1+-2-*3*/4/",
        parse(rgb).plainText.mkString == "",
        parse(rgbOps).render.mkString == rgbOps,
        parse(rgb).render.mkString == rgb
      )

    }
    'concat{
      assert((parse(rgbOps) ++ parse(rgbOps)).render.mkString == rgbOps ++ rgbOps)
    }
    'query{

    }
    'split{
      val splits = Seq(
        // Under-shot indexes just get clamped
        (-99,  s"", s"+1+$RED-2-$GREEN*3*$BLUE/4/"),
        (-1,  s"", s"+1+$RED-2-$GREEN*3*$BLUE/4/"),

        // These are the standard series
        (0,  s"", s"+1+$RED-2-$GREEN*3*$BLUE/4/"),
        (1,  s"+", s"1+$RED-2-$GREEN*3*$BLUE/4/"),
        (2,  s"+1", s"+$RED-2-$GREEN*3*$BLUE/4/"),
        (3,  s"+1+$RED", s"$RED-2-$GREEN*3*$BLUE/4/"),
        (4,  s"+1+$RED-", s"${RED}2-$GREEN*3*$BLUE/4/"),
        (5,  s"+1+$RED-2", s"$RED-$GREEN*3*$BLUE/4/"),
        (6,  s"+1+$RED-2-$GREEN", s"$GREEN*3*$BLUE/4/"),
        (7,  s"+1+$RED-2-$GREEN*", s"${GREEN}3*$BLUE/4/"),
        (8,  s"+1+$RED-2-$GREEN*3", s"$GREEN*$BLUE/4/"),
        (9,  s"+1+$RED-2-$GREEN*3*$BLUE", s"$BLUE/4/"),
        (10, s"+1+$RED-2-$GREEN*3*$BLUE/", s"${BLUE}4/"),
        (11, s"+1+$RED-2-$GREEN*3*$BLUE/4", s"$BLUE/"),
        (12, s"+1+$RED-2-$GREEN*3*$BLUE/4/", s"$BLUE"),

        // Overshoots just get clamped
        (13, s"+1+$RED-2-$GREEN*3*$BLUE/4/", s"$BLUE"),
        (99, s"+1+$RED-2-$GREEN*3*$BLUE/4/", s"$BLUE")
      )
      for((index, expectedLeft0, expectedRight0) <- splits){
        val (splitLeft, splitRight) = parse(rgbOps).split(index)
        val (expectedLeft, expectedRight) = (expectedLeft0.toVector, expectedRight0.toVector)
        val left = splitLeft.render
        val right = splitRight.render
        assert((left, right) == (expectedLeft, expectedRight))
      }
    }

  }
}
