package ammonite.interp

import ammonite.DualTestRepl
import ammonite.TestUtils._
import utest._
import ammonite.util.Util

object AutocompleteTests extends TestSuite {
  class Completer {
    val check = new DualTestRepl()
    def apply(
        caretCode: String,
        cmp: (Set[String]) => Set[String],
        sigs: (Set[String]) => Set[String] = _ => Set()
    ) = {
      val from = caretCode.indexOf("<from>")
      val caretCode0 =
        if (from < 0) caretCode
        else caretCode.replace("<from>", "")
      val cursor = caretCode0.indexOf("<caret>")
      val buf = caretCode0.replace("<caret>", "")

      for (interp <- check.interps) {
        val (index, completions, signatures) = interp.compilerManager.complete(
          cursor,
          interp.frameImports.toString,
          buf
        )
        val left = cmp(completions.toSet)
        assert(left == Set())
        val sigLeft = sigs(signatures.toSet)
        assert(sigLeft == Set())
        assert(from < 0 || index == from)
      }
    }
  }
  def checking[T](f: Completer => T) = {
    val c = new Completer
    val res = f(c)
    c.check.interps.map(_.compilerManager.shutdownPressy())
    res
  }
  implicit class SetExt[T](s1: Set[T]) {
    def ^(s2: Set[T]): Set[T] = (s1 diff s2) | (s2 diff s1)
  }

  val tests = Tests {
    println("AutocompleteTests")
    test("selection") {

      // Not sure why clone and finalize don't appear in this list
      val anyCompletion = Set(
        "!=",
        "==",
        "toString",
        "equals",
        "hashCode",
        "getClass",
        "asInstanceOf",
        "isInstanceOf"
      )
      test("import") - checking { complete =>
        // these fail on Java 9, need investigation
        if (!Util.java9OrAbove && complete.check.scala2) {
          complete("""import <from><caret>""", Set("java", "javax", "scala") -- _)
          complete("""import <from>j<caret>""", Set("java", "javax") -- _)
          complete(
            """import <from>ja<caret>""",
            x => Set("java", "javax") ^ (x - "javafx" - "javassist")
          )
        }
        complete("""import java.<from><caret>""", Set("lang", "util") -- _)
        complete("""import java.<from>u<caret>""", Set("util") ^ _)
        complete("""import java.util.<from><caret>""", Set("LinkedHashMap", "LinkedHashSet") -- _)
        complete(
          """import java.util.<from>LinkedHa<caret>""",
          Set("LinkedHashMap", "LinkedHashSet") ^ _
        )
        complete(
          """import java.util.{<from>LinkedHa<caret>""",
          Set("LinkedHashMap", "LinkedHashSet") ^ _
        )
        complete(
          """import java.util.{LinkedHashMap, <from>Linke<caret>""",
          Set("LinkedHashMap", "LinkedHashSet", "LinkedList") ^ _
        )
        complete(
          """import scala.uti.<caret>""",
          Set.empty[String] -- _
        )
        complete(
          """import scala.colltion.<caret>""",
          Set.empty[String] -- _
        )
        complete("""object X { import y<caret> ; def y(z: Int)""", Set.empty[String] -- _)
        complete(
          """import scala.collection.immutable.List.{<from>em<caret>, fi}""",
          Set("empty") -- _
        )
        complete(
          """import scala.collection.immutable.List.{em, <from>fi<caret>}""",
          Set("fill") -- _
        )
      }

      test("scope") - checking { complete =>
        // these fail on Java 9, need investigation
        if (!Util.java9OrAbove && complete.check.scala2) {
          complete("""<caret>""", Set("scala") -- _)
          complete("""Seq(1, 2, 3).map(argNameLol => <from><caret>)""", Set("argNameLol") -- _)
          complete("""object Zomg{ <from><caret> }""", Set("Zomg") -- _)
          complete(
            "<from>printl<caret>",
            Set("println") ^ _,
            Set[String]() ^ _
          )
        }
        //      Not sure why this doesnt work
        //      complete(
        //        "println<caret>",
        //        Set[String]() ^ _,
        //        Set("def println(x: Any): Unit", "def println(): Unit") ^
        //      )
      }
      test("scopePrefix") - checking { complete =>
        // these fail on Java 9, need investigation
        if (!Util.java9OrAbove && complete.check.scala2) {
          complete("""<from>ammon<caret>""", Set("ammonite") ^ _)

          complete(
            """Seq(1, 2, 3).map(argNameLol => <from>argNam<caret>)""",
            Set("argNameLol") ^ _
          )

          complete("""object Zomg{ <from>Zom<caret> }""", Set("Zomg") ^ _)
          complete("""object Zomg{ <from>Zo<caret>m }""", Set("Zomg") ^ _)
          complete("""object Zomg{ <from>Z<caret>om }""", Set("Zomg") ^ _)
          complete("""object Zomg{ <from><caret>Zom }""", Set("Zomg") ^ _)
        }
      }
      test("dot") - checking { complete =>
        complete(
          """java.math.<caret>""",
          Set("MathContext", "BigDecimal", "BigInteger", "RoundingMode") ^ _
        )

        val extra =
          if (scala2_12) Set()
          else Set("unless", "when")
        complete("""scala.Option.<caret>""", (anyCompletion ++ Set("apply", "empty") ++ extra) ^ _)

        complete("""Seq(1, 2, 3).map(_.<caret>)""", (anyCompletion ++ Set("+", "-", "*", "/")) -- _)

        complete("""val x = 1; x + (x.<caret>)""", Set("-", "+", "*", "/") -- _)

      }

      def deepTests(complete: Completer) = {
        complete("""<from>fromN<caret>""", Set("scala.concurrent.duration.fromNow") ^ _)
        complete(
          """<from>Fut<caret>""",
          Set("scala.concurrent.Future", "java.util.concurrent.Future") -- _
        )
        complete("""SECO<caret>""", Set("scala.concurrent.duration.SECONDS") ^ _)
      }
      test("dotPrefix") - checking { complete =>
        complete("""java.math.<from>Big<caret>""", Set("BigDecimal", "BigInteger") ^ _)
        complete("""scala.Option.option2<caret>""", Set() ^ _)

        val expected =
          if (complete.check.scala2)
            Set(
              "def >(x: Double): Boolean",
              "def >(x: Float): Boolean",
              "def >(x: Int): Boolean",
              "def >(x: Short): Boolean",
              "def >(x: Long): Boolean",
              "def >(x: Char): Boolean",
              "def >(x: Byte): Boolean"
            )
          else
            Set(
              "def >>>(x: Long): Int",
              "def >>>(x: Int): Int",
              "def >>(x: Long): Int",
              "def >>(x: Int): Int",
              "def >=(x: Long): Boolean",
              "def >=(x: Int): Boolean",
              "def >=(x: Short): Boolean",
              "def >=(x: Char): Boolean",
              "def >=(x: Byte): Boolean",
              "def >=(x: Double): Boolean",
              "def >=(x: Float): Boolean",
              "def >(x: Long): Boolean",
              "def >(x: Int): Boolean",
              "def >(x: Short): Boolean",
              "def >(x: Char): Boolean",
              "def >(x: Byte): Boolean",
              "def >(x: Double): Boolean",
              "def >(x: Float): Boolean"
            )
        complete("""val x = 1; x + x.<from>><caret>""", Set(">>", ">>>") -- _, expected ^ _)

        // https://issues.scala-lang.org/browse/SI-9153
        //
        //      complete("""val x = 123; x + x.m<caret>""",
        //        Set("max"),
        //        _ -- _
        //      )

        // Doesn't work, probably pressy bug
        //      val compares = Set("compare", "compareTo")
        //      complete("""Seq(1, 2, 3).map(_.compa<caret>)""", compares ^)
        //      complete("""Seq(1, 2, 3).map(_.co<caret>mpa)""", compares ^)
        //      complete("""Seq(1, 2, 3).map(_.<caret>compa)""", compares, ^)
      }

      test("deep") - checking { complete =>
        if (complete.check.scala2)
          deepTests(complete)
        else
          "Disabled in Scala 3"
      }

      test("defTab") - checking { complete =>
        // Assert no NullPointerException was thrown. Does not verify any completions.
        complete("""def<caret>""", Set.empty -- _)
      }

      test("Array") - checking { complete =>
        // Test around https://github.com/lihaoyi/Ammonite/issues/252
        complete("""new Array<caret>""", Set() ^ _)
      }

      test("LOCAL_SUFFIX_STRING") - checking { complete =>
        complete(
          // The following object creation pattern triggers nsc to return
          // symbols w/ trailing whitespace, which we strip.
          """object x { val foo = 1 }; x.<caret>""",
          Set("foo") -- _
        )
      }
    }

    test("backquotes") {
      test("spaces") - checking { complete =>
        complete(
          """object x { val `Backquoted Bar` = 1 }; x.<caret>""",
          Set("`Backquoted Bar`") -- _
        )
      }

      test("keywords") - checking { complete =>
        complete(
          """object x { val `new` = 1; }; x.<caret>""",
          Set("`new`") -- _
        )
      }
    }

    test("dependencies") {
      def dependenciesTests(complete: Completer) = {
        complete(
          """import $ivy.<from>`io.get-c<caret>`""",
          Set("`io.get-coursier") ^ _
        )
        complete(
          """import $ivy.<from>`io.get-coursier::coursier-co<caret>`""",
          Set("`io.get-coursier::coursier-core") ^ _.filterNot(_.contains("_sjs"))
        )
        complete(
          """import $ivy.{<from>`io.get-coursier::coursier-co<caret>`}""",
          Set("`io.get-coursier::coursier-core") ^ _.filterNot(_.contains("_sjs"))
        )
        // Completions when the ivy import has no closing backtick
        complete(
          """import $ivy.<from>`io.get-c<caret>""",
          Set("`io.get-coursier") ^ _
        )
        complete(
          """import $ivy.<from>`io.get-coursier::coursier-co<caret>""",
          Set("`io.get-coursier::coursier-core") ^ _.filterNot(_.contains("_sjs"))
        )
        // these rely on versions not around in 2.13
        if (scala.util.Properties.versionNumberString.startsWith("2.12.")) {
          complete(
            """import $ivy.{`io.get-coursier::coursier-cache:1.0.3`, """ +
              """<from>`io.get-coursier::coursier-co<caret>`}""",
            Set("`io.get-coursier::coursier-core") ^ _.filterNot(_.contains("_sjs"))
          )
          complete(
            """import $ivy.{<from>`io.get-coursier::coursier-co<caret>`, """ +
              """`io.get-coursier::coursier-cache:1.0.3`}""",
            Set("`io.get-coursier::coursier-core") ^ _.filterNot(_.contains("_sjs"))
          )
          complete(
            """import $ivy.<from>`io.get-coursier::coursier-cache:1.0.<caret>`""",
            c =>
              (Set("1.0.0", "1.0.3").map(v => s"`io.get-coursier::coursier-cache:$v") -- c) ++
                c.filter(!_.startsWith("`io.get-coursier::coursier-cache:1.0."))
          )
        }
      }
      checking { complete =>
        if (complete.check.scala2) dependenciesTests(complete)
        else "Disabled in Scala 3"
      }
    }
  }
}
