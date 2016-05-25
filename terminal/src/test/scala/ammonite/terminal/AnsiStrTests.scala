package ammonite.terminal

import utest._


object AnsiStrTests extends TestSuite{

  // Alias a bunch of rendered attributes to short names
  // to use in all our test cases
  val R = Ansi.Color.Red.escape
  val G = Ansi.Color.Green.escape
  val B = Ansi.Color.Blue.escape
  val Y = Ansi.Color.Yellow.escape
  val UND = Ansi.Underlined.On.escape
  val DUND = Ansi.Underlined.Off.escape
  val REV = Ansi.Reversed.On.escape
  val DREV = Ansi.Reversed.Off.escape
  val DCOL = Ansi.Color.Reset.escape
  val RES = Ansi.Reset.escape
  /**
    * ANSI escape sequence to reset text color
    */
  val RTC = Ansi.Color.Reset.escape
  val tests = TestSuite{
    val rgbOps = s"+++$R---$G***$B///"
    val rgb = s"$R$G$B"
    'parsing{
      assert(
        Ansi.Str(rgbOps).plainText == "+++---***///",
        Ansi.Str(rgb).plainText == "",
        Ansi.Str(rgbOps).render == rgbOps + RTC,
        Ansi.Str(rgb).render == ""
      )
    }

    'concat{
      val concated = (Ansi.Str(rgbOps) ++ Ansi.Str(rgbOps)).render
      val expected = rgbOps ++ RTC ++ rgbOps ++ RTC

      assert(concated == expected)
    }

    'split{
      val splits = Seq(
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
        (12, s"+++$R---$G***$B///$RTC", s"")
      )
      for((index, expectedLeft0, expectedRight0) <- splits){
        val (splitLeft, splitRight) = Ansi.Str(rgbOps).splitAt(index)
        val (expectedLeft, expectedRight) = (expectedLeft0, expectedRight0)
        val left = splitLeft.render
        val right = splitRight.render
        assert((left, right) == (expectedLeft, expectedRight))
      }
    }
    'substring{
      val substringed = Ansi.Str(rgbOps).substring(4, 9).render
      assert(substringed == s"$R--$G***$RTC")
    }

    'overlay{
      'simple{
        val overlayed = Ansi.Str(rgbOps).overlay(Ansi.Color.Yellow, 4, 7)
        val expected = s"+++$R-$Y--*$G**$B///$RTC"
        assert(overlayed.render == expected)
      }
      'resetty{
        val resetty = s"+$RES++$R--$RES-$RES$G***$B///"
        val overlayed = Ansi.Str(resetty).overlay(Ansi.Color.Yellow, 4, 7).render
        val expected = s"+++$R-$Y--*$G**$B///$RTC"
        assert(overlayed == expected)
      }
      'mixedResetUnderline{
        val resetty = s"+$RES++$R--$RES-$UND$G***$B///"
        val overlayed = Ansi.Str(resetty).overlay(Ansi.Color.Yellow, 4, 7).render toVector
        val expected = s"+++$R-$Y--$UND*$G**$B///$DCOL$DUND" toVector

        assert(overlayed == expected)
      }
      'underlines{
        val resetty = s"$UND#$RES    $UND#$RES"
        'underlineBug{
          val overlayed = Ansi.Str(resetty).overlay(Ansi.Reversed.On, 0, 2).render
          val expected = s"$UND$REV#$DUND $DREV   $UND#$DUND"
          assert(overlayed == expected)
        }
        'barelyOverlapping{
          val overlayed = Ansi.Str(resetty).overlay(Ansi.Reversed.On, 0, 1).render
          val expected = s"$UND$REV#$DUND$DREV    $UND#$DUND"
          assert(overlayed == expected)
        }
        'endOfLine{
          val overlayed = Ansi.Str(resetty).overlay(Ansi.Reversed.On, 5, 6).render
          val expected = s"$UND#$DUND    $UND$REV#$DUND$DREV"
          assert(overlayed == expected)
        }
        'overshoot{
          val overlayed = Ansi.Str(resetty).overlay(Ansi.Reversed.On, 5, 10).render.toVector
          val expected = s"$UND#$DUND    $UND$REV#$DUND$DREV".toVector
          assert(overlayed == expected)
        }
        'empty{
          val overlayed = Ansi.Str(resetty).overlay(Ansi.Reversed.On, 0, 0).render
          val expected = s"$UND#$DUND    $UND#$DUND"
          assert(overlayed == expected)
        }
        'singleContent{
          val overlayed = Ansi.Str(resetty).overlay(Ansi.Reversed.On, 2, 4).render
          val expected = s"$UND#$DUND $REV  $DREV $UND#$DUND"
          assert(overlayed == expected)

        }
      }
    }
    'attributes{
      * - {
        Console.RESET + Ansi.Underlined.On
      }
      * - {
        Console.RESET + (Ansi.Underlined.On("Reset ") ++ Ansi.Underlined.Off("Underlined"))
      }
      * - {
        Console.RESET + Ansi.Bold.On
      }
      * - {
        Console.RESET + (Ansi.Bold.On("Reset ") ++ Ansi.Bold.Off("Bold"))
      }
      * - {
        Console.RESET + Ansi.Reversed.On
      }
      * - {
        Console.RESET + (Ansi.Reversed.On("Reset ") ++ Ansi.Reversed.Off("Reversed"))
      }
    }
    def tabulate(all: Seq[Ansi.Attr]) = {
      all.map(attr => attr.toString + " " * (25 - attr.name.length))
         .grouped(3)
         .map(_.mkString)
         .mkString("\n")
    }

    'colors - tabulate(Ansi.Color.all)
    'backgrounds - tabulate(Ansi.Back.all)
    'negative{
      'parse{
        // Make sure that Ansi.Str throws on most common non-color
        // Ansi terminal commands
        //
        // List of common non-color Ansi terminal commands taken from
        // https://en.wikipedia.org/wiki/ANSI_escape_code#Non-CSI_codes

        def check(s: String, msg: String) ={
          intercept[IllegalArgumentException]{ Ansi.Str(s, strict = true) }
  //        assert(ex.getMessage.contains(msg))
        }

        'cursorUp - check("Hello\u001b[2AWorld", "[2A")
        'cursorDown- check("Hello\u001b[2BWorld", "[2B")
        'cursorForward - check("Hello\u001b[2CWorld", "[2C")
        'cursorBack - check("Hello\u001b[2DWorld", "[2D")
        'cursorNextLine - check("Hello\u001b[2EWorld", "[2E")
        'cursorPrevLine - check("Hello\u001b[2FWorld", "[2F")
        'cursorHorizontalAbs - check("Hello\u001b[2GmWorld", "[2G")
        'cursorPosition- check("Hello\u001b[2;2HmWorld", "[2;2H")
        'eraseDisplay - check("Hello\u001b[2JWorld", "[2J")
        'eraseLine - check("Hello\u001b[2KWorld", "[2K")
        'scrollUp - check("Hello\u001b[2SWorld", "[2S")
        'scrollDown - check("Hello\u001b[2TWorld", "[2T")
        'horizontalVerticalPos - check("Hello\u001b[2;2fWorld", "[2;2f")
        'selectGraphicRendition - check("Hello\u001b[2mWorld", "[2m")
        'auxPortOn - check("Hello\u001b[5iWorld", "[5i")
        'auxPortOff - check("Hello\u001b[4iWorld", "[4i")
        'deviceStatusReport - check("Hello\u001b[6n", "[6n")
        'saveCursor - check("Hello\u001b[s", "[s")
        'restoreCursor - check("Hello\u001b[u", "[u")

      }
      'outOfBounds{
        intercept[IllegalArgumentException]{ Ansi.Str("foo").splitAt(10) }
        intercept[IllegalArgumentException]{ Ansi.Str("foo").splitAt(4) }
        intercept[IllegalArgumentException]{ Ansi.Str("foo").splitAt(-1) }
        intercept[IllegalArgumentException]{ Ansi.Str("foo").substring(0, 4)}
        intercept[IllegalArgumentException]{ Ansi.Str("foo").substring(-1, 2)}
        intercept[IllegalArgumentException]{ Ansi.Str("foo").substring(2, 1)}
      }
    }
  }
}
