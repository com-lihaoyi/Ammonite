package ammonite.sh

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
      val prevImports = check.eval.previousImportBlock
      val prev = prevImports + "\n" + "object Foo{\n"
      import collection.JavaConversions._
      val completions = check.compiler.complete(
        cursor + prev.length,
        prev + buf + "\n}"
      ).filter(_ != "<init>")
       .toSet
      val left = cmp(expected, completions)
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
    'simpleExpressions{
      complete("""""", anyCompletion ++ Set("clone", "finalize"), ^)

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
    }
  }
}

