package test.ammonite.pprint

import utest._

import scala.annotation.tailrec
import scala.collection.{immutable => imm, mutable}
import scala.util.matching.Regex
import ammonite.pprint._

object HorizontalTests extends TestSuite{

  def check[T: PPrint](t: T, expected: String) = {
    val pprinted = PPrint(t).mkString
    assert(pprinted == expected.trim)
  }

  val tests = TestSuite{
    'Horizontal {
      import ammonite.pprint.Config.Defaults._

      'primitives {
        'Unit {
          * - check((), "()")
        }
        'Char {
          * - check('\n', "'\\n'")
          * - check('a', "'a'")
        }
        'Byte {
          * - check(123.toByte, "123")
          * - check(-123.toByte, "-123")
        }
        'Short {
          * - check(123.toShort, "123")
          * - check(-12345.toShort, "-12345")
        }
        'Int {
          * - check(123, "123")
          * - check(-1234567, "-1234567")
        }
        'Long {
          * - check(123456789012345L, "123456789012345L")
          * - check(-123456789012345L, "-123456789012345L")
        }
        'Float {
          * - check(0.75F, "0.75F")
          * - check(-13.5F, "-13.5F")
        }
        'Double {
          * - check(0.125, "0.125")
          * - check(-0.125, "-0.125")
        }
        'String {
          val tq = "\"\"\""
          * - check("i am a cow", """ "i am a cow" """)
          * - check( """ "hello" """.trim, s"""
          |$tq
          |"hello"
          |$tq
          """.stripMargin
          )

          * - check("\n\n\n", s"""
          |$tq
          |
          |
          |
          |$tq
          """.stripMargin)
        }
        'Symbols {
          * - check('hello, """'hello""")
          * - check('I_AM_A_COW, """'I_AM_A_COW""")
        }
      }

      'misc {
        //        'Nothing - intercept[NotImplementedError](check(???, ""))
        'Null {
          check(null, "null")
          check(null: String, "null")
          check(Seq("look!", null: String, "hi"), """List("look!", null, "hi")""")
        }
        'Either {
          check(Left(123): Either[Int, Int], "Left(123)")
          check(Left(123): Left[Int, Int], "Left(123)")

          check(Left(123), "Left(123)")
          check(Right((1, "2", 3)), """Right((1, "2", 3))""")
        }
        'Options {
          check(Some(123), "Some(123)")
          check(None: Option[Int], "None")
          check(None: Option[Nothing], "None")
          check(None, "None")
          check(Some(None), "Some(None)")
        }
        'Default{
          check(() => (), "<function0>")
          check((i: Int) => (), "<function1>")
        }
      }

      'collections {
        // Fallback to toString
        'Iterator - check(Iterator(1, 2, 3), "non-empty iterator")
        'Iterator - check(Iterable('1', '2', '3'), "List('1', '2', '3')")

        'Array - check(Array(1, 2, 3), "Array(1, 2, 3)")
        'Seq - check(Seq(1, 2, 3), "List(1, 2, 3)")
        'List - check(List("1", "2", "3"), """List("1", "2", "3")""")
        'Vector - check(Vector('omg, 'wtf, 'bbq), """Vector('omg, 'wtf, 'bbq)""")

        'Buffer - check(mutable.Buffer('omg, 'wtf, 'bbq), """ArrayBuffer('omg, 'wtf, 'bbq)""")

        // Streams are hard-coded to always display vertically, in order
        // to make streaming pretty-printing sane
        'Stream - check(
          Stream('omg, 'wtf, 'bbq),
          """Stream(
            |  'omg,
            |  'wtf,
            |  'bbq
            |)""".stripMargin
        )
        'Iterable - check(Iterable('omg, 'wtf, 'bbq), """List('omg, 'wtf, 'bbq)""")
        'Traversable - check(Traversable('omg, 'wtf, 'bbq), """List('omg, 'wtf, 'bbq)""")
        'Set - check(Set('omg), """Set('omg)""")
        'mutableSet - check(mutable.Set('omg), """Set('omg)""")
        'collectionSet - check(collection.Set('omg), """Set('omg)""")
        'SortedSet - check(
          imm.SortedSet("1", "2", "3"),
          """TreeSet("1", "2", "3")"""
        )
        'Map {
          check(Map("key" -> "value"), """Map("key" -> "value")""")
        }
        'collectionMap {
          check(Map("key" -> "value"): collection.Map[String, String], """Map("key" -> "value")""")
        }

        'mutableMap {
          check(mutable.Map("key" -> "value"), """Map("key" -> "value")""")
        }

        'SortedMap - check(
          imm.SortedMap("key" -> "v", "key2" -> "v2"),
          """Map("key" -> "v", "key2" -> "v2")"""
        )
      }

      'tuples {

        check(Tuple1("123"), """Tuple1("123")""")
        check((1, 2, "123"), """(1, 2, "123")""")
        check(
          (1, 2, "123", (100L, 200L), 1.5F, 0.1),
          """(1, 2, "123", (100L, 200L), 1.5F, 0.1)"""
        )
      }
      'products {

        check(
          Foo(123, Seq("hello world", "moo")),
          """Foo(123, List("hello world", "moo"))"""
        )

        check(
          Seq(Foo(123, Seq("hello world", "moo"))),
          """List(Foo(123, List("hello world", "moo")))"""
        )

        check(FooNoArgs(), "FooNoArgs()")
      }
    }

  }

  
}
