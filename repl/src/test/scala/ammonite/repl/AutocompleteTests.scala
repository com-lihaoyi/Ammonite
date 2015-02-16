package ammonite.repl

import utest._

import scala.collection.{immutable => imm}
import scala.reflect.internal.util.BatchSourceFile

object AutocompleteTests extends TestSuite{

  val tests = TestSuite{
    val check = new Checker()
    def complete(caretCode: String,
                 expected: Set[String],
                 cmp: (Set[String], Set[String]) => Set[String]) = {
      val cursor = caretCode.indexOf("<caret>")
      val buf = caretCode.replace("<caret>", "")
      val prevImports = check.interp.eval.previousImportBlock
      val prev = prevImports + "\n" + "object Foo{"
      import collection.JavaConversions._
      val (index, completions) = check.interp.compiler.complete(
        cursor + prev.length,
        prev + buf + "}"
      )
      val left = cmp(expected, completions.toSet)
      assert(left == Set())
    }

    // Not sure why clone and finalize don't appear in this list
    val anyCompletion = Set(
      "synchronized",
      "##", "!=", "==",
      "ne", "eq",
      "wait", "notifyAll", "notify",
      "toString", "equals", "hashCode",
      "getClass", "asInstanceOf", "isInstanceOf",
      "+", "formatted", "ensuring",
      "→", "->"
    )

    def ^[T](s1: Set[T], s2: Set[T]) = (s1 diff s2) | (s2 diff s1)

    'scope{
      complete("""<caret>""", Set("scala"), _ -- _)

      complete("""Seq(1, 2, 3).map(argNameLol => <caret>)""", Set("argNameLol"), _ -- _)

      complete("""object Zomg{ <caret> }""", Set("Zomg"), _ -- _)
    }
    'scopePrefix{
      complete("""scal<caret>""", Set("scala"), ^)

      complete("""Seq(1, 2, 3).map(argNameLol => argNam<caret>)""", Set("argNameLol"), ^)

      complete("""object Zomg{ Zom<caret> }""", Set("Zomg"), ^)
      complete("""object Zomg{ Zo<caret>m }""", Set("Zomg"), ^)
      complete("""object Zomg{ Z<caret>om }""", Set("Zomg"), ^)
      complete("""object Zomg{ <caret>Zom }""", Set("Zomg"), ^)
    }
    'dot{
      complete("""java.math.<caret>""",
        Set("MathContext", "BigDecimal", "BigInteger", "RoundingMode"),
        ^
      )

      complete("""scala.Option.<caret>""",
        anyCompletion ++ Set("apply", "empty", "option2Iterable"),
        ^
      )

      complete("""Seq(1, 2, 3).map(_.<caret>)""",
        anyCompletion ++ Set("+", "-", "*", "/", "to", "until"),
        _ -- _
      )

      complete("""val x = 1; x + (x.<caret>)""",
        Set("to", "max", "-", "+", "*", "/"),
        _ -- _
      )
    }

    'deep{
      complete("""fromN<caret>""",
        Set("scala.concurrent.duration.fromNow"),
        ^
      )
      complete("""Fut<caret>""",
        Set("scala.concurrent.Future", "java.util.concurrent.Future"),
        _ -- _
      )
      complete("""SECO<caret>""",
        Set("scala.concurrent.duration.SECONDS"),
        ^
      )
    }
    'dotPrefix{
      complete("""java.math.Big<caret>""",
        Set("BigDecimal", "BigInteger"),
        ^
      )
      complete("""scala.Option.option2<caret>""",
        Set("option2Iterable"),
        ^
      )
      complete("""val x = 1; x + x.><caret>""",
        Set(">>", ">>>"),
        _ -- _
      )
      // https://issues.scala-lang.org/browse/SI-9153
      //
      //      complete("""val x = 123; x + x.m<caret>""",
      //        Set("max"),
      //        _ -- _
      //      )

      val compares = Set("compare", "compareTo")
      complete("""Seq(1, 2, 3).map(_.compa<caret>)""", compares, ^)
      complete("""Seq(1, 2, 3).map(_.co<caret>mpa)""", compares, ^)
      complete("""Seq(1, 2, 3).map(_.<caret>compa)""", compares, ^)
    }
  }
}

