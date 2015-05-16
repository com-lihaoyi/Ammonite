package test.ammonite.pprint

import utest._

import scala.annotation.tailrec
import scala.collection.{immutable => imm}
import scala.util.matching.Regex
import ammonite.pprint._

object Nested{
  object ODef { case class Foo(i: Int, s: String) }

  class CDef { case class Foo(i: Int, s: String) }
  object CDef extends CDef

}
case class Foo(integer: Int, sequence: Seq[String])
case class FooG[T](t: T, sequence: Seq[String])
case class FooNoArgs()
object PPrintTests extends TestSuite{

  def check[T: PPrint](t: T, expected: String) = {
    val pprinted = PPrint(t).mkString
    assert(pprinted == expected.trim)
  }

  val tests = TestSuite{
    'Vertical{

      implicit def default = ammonite.pprint.Config(25)
      'singleNested {
        * - check(
          List("12", "12", "12"),
          """List("12", "12", "12")"""
        )
        * - check(
          List("123", "123", "123"),
          """List("123", "123", "123")"""
        )
        * - check(
          List("1234", "123", "123"),
          """List(
            |  "1234",
            |  "123",
            |  "123"
            |)""".stripMargin
        )
        * - check(
          Map(1 -> 2, 3 -> 4),
          """Map(1 -> 2, 3 -> 4)"""
        )
        * - check(
          Map(List(1, 2) -> List(3, 4), List(5, 6) -> List(7, 8)),
          """Map(
            |  List(1, 2) -> List(3, 4),
            |  List(5, 6) -> List(7, 8)
            |)""".stripMargin
        )

        * - check(
          Map(
            List(123, 456, 789, 123, 456) -> List(3, 4, 3, 4),
            List(5, 6) -> List(7, 8)
          ),
          """Map(
            |  List(
            |    123,
            |    456,
            |    789,
            |    123,
            |    456
            |  ) -> List(3, 4, 3, 4),
            |  List(5, 6) -> List(7, 8)
            |)""".stripMargin
        )

        * - check(
          Map(
            List(5, 6) -> List(7, 8),
            List(123, 456, 789, 123, 456) -> List(123, 456, 789, 123, 456)
          ),
          """Map(
            |  List(5, 6) -> List(7, 8),
            |  List(
            |    123,
            |    456,
            |    789,
            |    123,
            |    456
            |  ) -> List(
            |    123,
            |    456,
            |    789,
            |    123,
            |    456
            |  )
            |)""".stripMargin
        )

        * - check(
          List("12345", "12345", "12345"),
          """List(
            |  "12345",
            |  "12345",
            |  "12345"
            |)""".stripMargin
        )
        * - check(
          Foo(123, Seq("hello world", "moo")),
          """Foo(
            |  123,
            |  List(
            |    "hello world",
            |    "moo"
            |  )
            |)""".stripMargin
        )
        * - check(
          Foo(123, Seq("moo")),
          """Foo(123, List("moo"))""".stripMargin
        )

      }
      'doubleNested{

        * - check(
          List(Seq("omg", "omg"), Seq("mgg", "mgg"), Seq("ggx", "ggx")),
          """List(
            |  List("omg", "omg"),
            |  List("mgg", "mgg"),
            |  List("ggx", "ggx")
            |)""".stripMargin
        )
        * - check(
          List(Seq("omg", "omg", "omg", "omg"), Seq("mgg", "mgg"), Seq("ggx", "ggx")),
          """List(
            |  List(
            |    "omg",
            |    "omg",
            |    "omg",
            |    "omg"
            |  ),
            |  List("mgg", "mgg"),
            |  List("ggx", "ggx")
            |)""".stripMargin
        )
        * - check(
          List(
            Seq(
              Seq("mgg", "mgg", "lols"),
              Seq("mgg", "mgg")
            ),
            Seq(
              Seq("ggx", "ggx"),
              Seq("ggx", "ggx", "wtfx")
            )
          ),
          """List(
            |  List(
            |    List(
            |      "mgg",
            |      "mgg",
            |      "lols"
            |    ),
            |    List("mgg", "mgg")
            |  ),
            |  List(
            |    List("ggx", "ggx"),
            |    List(
            |      "ggx",
            |      "ggx",
            |      "wtfx"
            |    )
            |  )
            |)""".stripMargin
        )
        * - check(
          FooG(Seq(FooG(Seq(Foo(123, Nil)), Nil)), Nil),
          """FooG(
            |  List(
            |    FooG(
            |      List(
            |        Foo(123, List())
            |      ),
            |      List()
            |    )
            |  ),
            |  List()
            |)
          """.stripMargin
        )
        * - check(
          FooG(FooG(Seq(Foo(3, Nil)), Nil), Nil),
          """FooG(
            |  FooG(
            |    List(Foo(3, List())),
            |    List()
            |  ),
            |  List()
            |)""".stripMargin
        )
      }
    }
    'traited {
      import ammonite.pprint.Config.Defaults._
      check(Nested.ODef.Foo(2, "ba"), "Foo(2, \"ba\")")
      check(Nested.CDef.Foo(2, "ba"), "Foo(2, \"ba\")")
    }
    'Color{
      import ammonite.pprint.Config.Colors._
      def count(haystack: Iterator[String], needles: (String, Int)*) = {
        val str = haystack.mkString
        for ((needle, expected) <- needles){
          val count = countSubstring(str, needle)
          assert(count == expected)
        }
      }
      def countSubstring(str1:String, str2:String):Int={
        @tailrec def count(pos:Int, c:Int):Int={
          val idx=str1 indexOf(str2, pos)
          if(idx == -1) c else count(idx+str2.size, c+1)
        }
        count(0,0)
      }

      import Console._
      * - count(PPrint(123), GREEN -> 1, RESET -> 1)
      * - count(PPrint(""), GREEN -> 1, RESET -> 1)
      * - count(PPrint(Seq(1, 2, 3)), GREEN -> 3, YELLOW -> 1, RESET -> 4)
      * - count(
        PPrint(Map(1 -> Nil, 2 -> Seq(" "), 3 -> Seq("   "))),
        GREEN -> 5, YELLOW -> 4, RESET -> 9
      )
    }

    'Truncation{
      'long_no_truncation{
        implicit val cfg = Config.Defaults.PPrintConfig
          * - check("a" * 10000,"\""+"a" * 10000+"\"")
          * - check(
            List.fill(30)(100),
            """List(
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100,
              |  100
              |)""".stripMargin
            )
      }

      'short_non_truncated{
        implicit val cfg = Config.Defaults.PPrintConfig.copy(lines = 15)
        * - check("a"*1000, "\"" + "a"*1000 + "\"")
        * - check(List(1,2,3,4), "List(1, 2, 3, 4)")
        * - check(
          List.fill(13)("asdfghjklqwertz"),
          """List(
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz"
            |)
          """.stripMargin
        )
      }

      'short_lines_truncated{
        implicit val cfg = Config.Defaults.PPrintConfig.copy(lines = 15)
        * - check(
          List.fill(15)("foobarbaz"),
          """List(
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |...""".stripMargin
        )
        * - check(
        List.fill(150)("foobarbaz"),
          """List(
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |...""".stripMargin
        )
      }

      'long_line_truncated{
        implicit val cfg = Config.Defaults.PPrintConfig.copy(
          maxWidth = 100,
          lines = 3
        )
        check(
          "a" * 1000,
          """"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa..."""
        )
      }

      'stream{
        implicit val cfg = Config.Defaults.PPrintConfig.copy(
          lines = 5
        )
        check(
          Stream.continually("foo"),
          """Stream(
            |  "foo",
            |  "foo",
            |  "foo",
            |  "foo",
            |...
          """.stripMargin
        )
      }
    }

    'wrapped_lines{
      implicit val cfg = Config.Defaults.PPrintConfig.copy(
        maxWidth = 8,
        lines = 5
      )
      check(
        "1234567890\n"*10,
        "\"\"\"\n1234567890\n1234567890\n..."
      )
      // The result looks like 10 wide 3 deep, but because of the wrapping
      // (maxWidth = 8) it is actually 8 wide and 5 deep.
    }
  }

  
}
