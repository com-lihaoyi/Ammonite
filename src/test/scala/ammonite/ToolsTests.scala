package ammonite

import ammonite.ops._
import ammonite.runtime.tools._
import ammonite.util.Util.newLine
import org.scalatest.FreeSpec

class ToolsTests extends FreeSpec {

  /**
    * Convert the highlighter colors into angle brackets for easy testing
    */
  implicit val defaultHighlightColor =
    ammonite.runtime.tools.GrepResult.Color(
      fansi.Color.Red,
      fansi.Attrs.Empty
    )

  "grep" - {

    val items = Seq(123, 456, 789)
    "filter" in {
      import pprint.Config.Defaults._
      assert(
        (items |? grep ! "45") == Seq(456) &&
          (items |? grep ! "45".r) == Seq(456) &&
          (items |? grep ! "[123456]+".r) == Seq(123, 456) &&
          (items |? grep ! "^[123456]+$".r) == Seq(123, 456) &&
          (items |? grep ! "[123456]".r) == Seq(123, 456) &&
          (items |? grep ! "^[123456]$".r) == Seq()
      )
    }
    "flatMap" - {
      implicit def pprintConfig = pprint.Config.Defaults.PPrintConfig.copy(width = 25)
      def check[T: Grepper, V: pprint.PPrint](items: Seq[V], regex: T, expected: Seq[String]) = {
        val grepped = items || grep ! regex
        implicitly[pprint.Config]
        val displayed = for (g <- grepped) yield {
          pprint.tokenize(g).mkString.replace(fansi.Color.Red.escape, "<").replace(fansi.Color.Reset.escape, ">")
        }
        assert(displayed == expected)
      }
      "string" in {
        check(items, "12", Seq("<12>3"))
        check(items, "23", Seq("1<23>"))
      }
      "regex" in {
        check(items, "^[123456]".r, Seq("<1>23", "<4>56"))
        check(items, "[123456]$".r, Seq("12<3>", "45<6>"))
      }
      "long" - {
        val longItems = Seq("123456789012345678901234567890")
        "truncateStart" in check(
          longItems,
          "\"123".r,
          Seq("<\"123>45678901234567...")
        )
        // If you grep near the end, peg the context to the end
        "truncateEnd" in check(
          longItems,
          "890\"".r,
          Seq("...45678901234567<890\">")
        )

        // If your greps are close together, peg around the middle
        "closeTogether" in check(
          longItems,
          "0123",
          Seq("...89<0123>456789<0123>45...")
        )

        // If your greps are far apart, peg each one
        "farApart" in check(
          longItems,
          "\"123|890\"".r,
          Seq("<\"123>45678901234567..." + newLine + "...45678901234567<890\">")
        )

        // Make sure that when the different matches are relatively close
        // together, the snippets of context displayed do not overlap.
        "noOverlap" in check(
          longItems,
          "123",
          Seq("\"<123>4567890<123>4567..." + newLine + "...890<123>4567890\"")
        )
      }
    }
  }

}
