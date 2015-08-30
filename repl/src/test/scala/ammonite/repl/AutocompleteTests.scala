package ammonite.repl

import ammonite.repl.TestUtils._
import ammonite.repl.frontend.Highlighter
import utest._
import acyclic.file
import scala.collection.{immutable => imm}
import scala.reflect.internal.util.BatchSourceFile

sealed trait Interval
object Interval{
  object Id extends Interval
  object String extends Interval
  object Symbol extends Interval
  object End extends Interval
}
sealed trait Span{ def start: Int; def end: Int }
object Span{
  case class Id(start: Int, end: Int) extends Span
  case class String(start: Int, end: Int) extends Span
  case class Symbol(start: Int, end: Int) extends Span
}

object AutocompleteTests extends TestSuite{
  val tests = TestSuite {
    println("AutocompleteTests")
    'selection{
      val check = new Checker()
      def complete(caretCode: String,
                   cmp: (Set[String]) => Set[String],
                   sigs: (Set[String]) => Set[String] = _ => Set()) = {
        val cursor = caretCode.indexOf("<caret>")
        val buf = caretCode.replace("<caret>", "")

        val (index, completions, signatures) = check.interp.pressy.complete(
          cursor,
          check.interp.eval.previousImportBlock,
          buf
        )
        val left = cmp(completions.toSet)
        assert(left == Set())
        val sigLeft = sigs(signatures.toSet)
        assert(sigLeft == Set())
      }

      // Not sure why clone and finalize don't appear in this list
      val anyCompletion = Set(
        "!=", "==",
        "toString", "equals", "hashCode",
        "getClass", "asInstanceOf", "isInstanceOf"
      )
      implicit class SetExt[T](s1: Set[T]) {
        def ^(s2: Set[T]): Set[T] = (s1 diff s2) | (s2 diff s1)
      }

      'import {
        complete( """import <caret>""", Set("java", "javax", "scala") -- _)
        complete( """import j<caret>""", Set("java", "javax", "jline", "jawn") -- _)
        complete( """import ja<caret>""", x => Set("java", "javax", "jawn") ^ (x - "javafx"))
        complete( """import java.<caret>""", Set("lang", "util") -- _)
        complete( """import java.u<caret>""", Set("util") ^ _)
        complete( """import java.util.<caret>""", Set("LinkedHashMap", "LinkedHashSet") -- _)
        complete( """import java.util.LinkedHa<caret>""", Set("LinkedHashMap", "LinkedHashSet") ^ _)
        complete( """import java.util.{LinkedHa<caret>""", Set("LinkedHashMap", "LinkedHashSet") ^ _)
        complete(
          """import java.util.{LinkedHashMap, Linke<caret>""",
          Set("LinkedHashMap", "LinkedHashSet", "LinkedList") ^ _
        )

      }

      'scope {
        complete( """<caret>""", Set("scala") -- _)
        complete( """Seq(1, 2, 3).map(argNameLol => <caret>)""", Set("argNameLol") -- _)
        complete( """object Zomg{ <caret> }""", Set("Zomg") -- _)
        complete(
          "printl<caret>",
          Set("println") ^,
          Set[String]() ^
        )
        //      Not sure why this doesnt work
        //      complete(
        //        "println<caret>",
        //        Set[String]() ^,
        //        Set("def println(x: Any): Unit", "def println(): Unit") ^
        //      )
      }
      'scopePrefix {
        complete( """ammon<caret>""", Set("ammonite") ^ _)

        complete( """Seq(1, 2, 3).map(argNameLol => argNam<caret>)""", Set("argNameLol") ^)

        complete( """object Zomg{ Zom<caret> }""", Set("Zomg") ^)
        complete( """object Zomg{ Zo<caret>m }""", Set("Zomg") ^)
        complete( """object Zomg{ Z<caret>om }""", Set("Zomg") ^)
        complete( """object Zomg{ <caret>Zom }""", Set("Zomg") ^)
      }
      'dot {
        if (!scala2_10) {
          complete( """java.math.<caret>""",
            Set("MathContext", "BigDecimal", "BigInteger", "RoundingMode") ^
          )

          complete( """scala.Option.<caret>""",
            (anyCompletion ++ Set("apply", "empty")) ^
          )

          complete( """Seq(1, 2, 3).map(_.<caret>)""",
            (anyCompletion ++ Set("+", "-", "*", "/")) -- _
          )

          complete( """val x = 1; x + (x.<caret>)""",
            Set("-", "+", "*", "/") -- _
          )
        }
      }

      'deep {
        complete( """fromN<caret>""",
          Set("scala.concurrent.duration.fromNow") ^
        )
        complete( """Fut<caret>""",
          Set("scala.concurrent.Future", "java.util.concurrent.Future") -- _
        )
        complete( """SECO<caret>""",
          Set("scala.concurrent.duration.SECONDS") ^
        )
      }
      'dotPrefix {
        complete( """java.math.Big<caret>""",
          Set("BigDecimal", "BigInteger") ^
        )
        complete( """scala.Option.option2<caret>""",
          Set() ^
        )
        complete( """val x = 1; x + x.><caret>""",
          Set(">>", ">>>") -- _,
          Set(
            "def >(x: Double): Boolean",
            "def >(x: Float): Boolean",
            "def >(x: Int): Boolean",
            "def >(x: Short): Boolean",
            "def >(x: Long): Boolean",
            "def >(x: Char): Boolean",
            "def >(x: Byte): Boolean"
          ) ^
        )
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
  }
    'path{

      def doThing(s: String, cursor: Int)
                 : Option[(Option[String], Seq[Option[String]], Option[String], Int)] = {
        val indices = Highlighter.highlightIndices(
          ammonite.repl.Parsers.Splitter,
          s.toVector,
          {
            case scalaparse.Scala.Id => Interval.Id
            case scalaparse.Scala.Literals.Expr.String => Interval.String
            case scalaparse.Scala.Literals.Symbol => Interval.Symbol
          },
          Interval.End
        )
        val spans =
          indices
            .drop(1)
            .grouped(2)
            .collect{
              case Seq((s, Interval.Id, _), (e, Interval.End, _)) => Span.Id(s, e)
              case Seq((s, Interval.String, _), (e, Interval.End, _)) => Span.String(s, e)
              case Seq((s, Interval.Symbol, _), (e, Interval.End, _)) => Span.Symbol(s, e)
            }
            .toVector
            .reverse


        pprint.pprintln(spans, colors = pprint.Config.Colors.PPrintConfig.colors)
        // None means we're not in a path autocomplete, Some(None) means
        // we are but there is no incomplete segment before the cursor
        val frag: Option[Option[String]] =
          spans.head match{
            case Span.Id(start, end) =>
              if (s.substring(start, end) == "/") Some(None)
              else None
            case Span.String(start, end) if end == cursor =>
              Some(Some(s.slice(start, end)))
            case Span.Symbol(start, end) if end == cursor =>
              Some(Some(s.slice(start, end)))
            case _ => None
          }


        for (frag <- frag) yield {
          def rec(prev: Int,
                  spans: List[Span],
                  segments: List[Option[String]]): (Seq[Option[String]], Option[String]) = {

            pprint.pprintln(spans)
            def printing[T](x: T) = {
              println(x)
              x
            }
            spans match{
              case Span.Id(start, end) :: next :: rest
                if printing(s.slice(start, end)) == "/"
                && printing(s.slice(end, prev).trim()) == ""
                && printing(s.slice(next.end, start).trim()) == "" =>

                (next, s.slice(next.start, next.end)) match{
                  case (_: Span.Id, "up") => rec(next.start, rest, None :: segments)
                  case (_: Span.String, v) => rec(next.start, rest, Some(v) :: segments)
                  case (_: Span.Symbol, v) => rec(next.start, rest, Some(v) :: segments)
                  case _ => (segments, None)
                }
              case _ => (segments, None)
            }
          }
          val (body, base) = rec(
            cursor,
            if (frag.isDefined) spans.toList.drop(1) else spans.toList,
            Nil
          )
          (base, body, frag, if (frag.isDefined) cursor - spans.head.start else 0)

        }

      }
      'parse{
        def check(s: String, expected: (Option[String], Seq[Option[String]], Option[String], Int)) = {
          val cursor = s.indexOf("<caret>")
          val value = doThing(s.take(cursor), cursor).get
          assert(value == expected)
        }
        'pos{
//          check("""'hello/<caret>""", (None, Seq(Some("hello")), None, 0))
//          check("""'hello / <caret>""", (None, Seq(Some("hello")), None, 0))
          check("""'hello / 'worl<caret>""", (None, Seq(Some("hello")), Some("'worl"), 5))
//          check("""'hello / "world" / <caret>""", (None, Seq(Some("hello"), Some("world")), None, 0))
//          check(
//            """'hello / "world" / "foo<caret>""",
//            (None, Seq(Some("hello"), Some("world")), Some("\"foo"), 4)
//          )
//          check(
//            """'hello / "\"" / "foo<caret>""",
//            (None, Seq(Some("hello"), Some("\"")), Some("\"foo"), 4)
//          )
//          check(
//            """wd/ 'hello / "\"" / "foo<caret>""",
//            (Some("wd"), Seq(Some("hello"), Some("\"")), Some("\"foo"), 4)
//          )
//
//          check(
//            """wd / up / 'hello / up / "\"" / "foo<caret>""",
//            (Some("wd"), Seq(None, Some("hello"), None, Some("\"")), Some("\"foo"), 4)
//          )
//
//          check("""home/'fi<caret>""", (Some("home"), Nil, Some("'fi"), 3))
//          check("""home/'fi<caret>nd""", (Some("home"), Nil, Some("'fi"), 3))
        }
        'neg{
          def check(s: String) = {

          }
          check(""" "hello".<caret>""")
        }
      }
      'complete{

      }
    }
  }
}

