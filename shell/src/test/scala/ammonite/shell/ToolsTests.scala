package ammonite.shell

import ammonite.ops._

import utest._


object ToolsTests extends TestSuite{

  val tests = TestSuite {
    var wd = cwd
    /**
      * Convert the highlighter colors into angle brackets for easy testing
      */
    implicit val defaultHighlightColor = GrepResult.Color("<", ">")

    'grep{

      val items = Seq(123, 456, 789)
      'filter{
        import pprint.Config.Defaults._
        assert(
          (items |? grep! "45") == Seq(456),
          (items |? grep! "45".r) == Seq(456),
          (items |? grep! "[123456]+".r) == Seq(123, 456),
          (items |? grep! "^[123456]+$".r) == Seq(123, 456),
          (items |? grep! "[123456]".r) == Seq(123, 456),
          (items |? grep! "^[123456]$".r) == Seq()
        )
      }
      'flatMap{
        implicit def pprintConfig = pprint.Config.Defaults.PPrintConfig.copy(width = 25)
        def check[T: Grepper, V: pprint.PPrint](items: Seq[V], regex: T, expected: Seq[String]) = {

          val grepped = items || grep! regex
          implicitly[pprint.Config]
          val displayed = grepped.map(pprint.tokenize(_).mkString)
          assert(displayed == expected)
        }
        'string{
          check(items, "12", Seq("<12>3"))
          check(items, "23", Seq("1<23>"))

        }
        'regex{
          check(items, "^[123456]".r, Seq("<1>23", "<4>56"))
          check(items, "[123456]$".r, Seq("12<3>", "45<6>"))
        }
        'long{
          val longItems = Seq("123456789012345678901234567890")

          // If you grep near the start, peg the context to the start
          'truncateStart - check(
            longItems,
            "\"123".r,
            Seq("<\"123>456789012345678...")
          )
          // If you grep near the end, peg the context to the end
          'truncateEnd- check(
            longItems,
            "890\"".r,
            Seq("...345678901234567<890\">")
          )

          // If your greps are close together, peg around the middle
          'closeTogether  - check(
            longItems,
            "0123",
            Seq("...89<0123>456789<0123>45...")
          )

          // If your greps are far apart, peg each one
          'farApart - check(
            longItems,
            "\"123|890\"".r,
            Seq("<\"123>456789012345678...\n...345678901234567<890\">")
          )

          // Make sure that when the different matches are relatively close
          // together, the snippets of context displayed do not overlap.
          'noOverlap - check(
            longItems,
            "123",
            Seq("\"<123>4567890<123>45678...\n...90<123>4567890\"")
          )
        }
      }
    }
  }
}
