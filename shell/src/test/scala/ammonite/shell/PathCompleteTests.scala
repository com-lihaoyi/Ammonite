package ammonite.shell

import ammonite.TestUtils.scala2
import utest._

object PathCompleteTests extends TestSuite{
  val mainTests = Tests{
    test("path"){
      test("parse"){
        def check(s: String,
                  expected: (Option[String], Seq[Option[String]], Option[String], Int)) = {
          val cursor = s.indexOf("<caret>")
          val value = PathComplete.findPathLiteral(s.take(cursor), cursor).get
          assert(value == PathComplete.PathLiteralInfo.tupled(expected))
        }
        def checkNeg(s: String) = {
          val cursor = s.indexOf("<caret>")
          val res = PathComplete.findPathLiteral(s.take(cursor), cursor)
          assert(res == None)
        }
        test("pos"){
          check("""'hello/<caret>""", (None, Seq(Some("hello")), None, 0))
          check("""'hello / <caret>""", (None, Seq(Some("hello")), None, 0))
          check("""'hello / 'worl<caret>""", (None, Seq(Some("hello")), Some("'worl"), 5))
          check(
            """'hello / "world" / <caret>""",
            (None, Seq(Some("hello"), Some("world")), None, 0)
          )
          check(
            """'hello / "world" / "foo<caret>""",
            (None, Seq(Some("hello"), Some("world")), Some("\"foo"), 4)
          )
          check(
            """'hello / "\"" / "foo<caret>""",
            (None, Seq(Some("hello"), Some("\"")), Some("\"foo"), 4)
          )
          check(
            """wd/ 'hello / "\"" / "foo<caret>""",
            (Some("wd"), Seq(Some("hello"), Some("\"")), Some("\"foo"), 4)
          )

          check(
            """wd / up / 'hello / up / "\"" / "foo<caret>""",
            (Some("wd"), Seq(None, Some("hello"), None, Some("\"")), Some("\"foo"), 4)
          )

          check("""home/'fi<caret>""", (Some("home"), Nil, Some("'fi"), 3))
          check("""home/'fi<caret>nd""", (Some("home"), Nil, Some("'fi"), 3))
        }
        test("neg"){
          checkNeg(""" "hello".<caret>""")
          checkNeg(""" omg/<caret>""")
          checkNeg(""" omg / <caret>""")
          // We only do dumb "literal" paths; any extraneous syntax should
          // cause it to fail
          checkNeg(""" wd / "" / ("omg") / <caret>""")
        }
      }
    }

  }

  val tests =
    if (scala2) mainTests
    else Tests { test("Disabled in Scala 3") - "Disabled in Scala 3" }
}
