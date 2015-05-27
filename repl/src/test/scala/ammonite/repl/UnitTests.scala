package ammonite.repl

import ammonite.repl.frontend.Highlighter
import ammonite.repl.frontend.Highlighter._
import utest._

import scala.collection.{immutable => imm}
import scalaparse.Scala._
import scalaparse.syntax.Identifiers._

object UnitTests extends TestSuite{

  def testHighlight(buffer: Vector[Char]) = Highlighter.highlight(
    buffer,
    {
      case Literals.Expr.Interp | Literals.Pat.Interp => ">"
      case ExprLiteral => "<G|"
      case Type => "<G|"
      case Stringy(BackTicked(body)) if alphaKeywords.contains(body) => "<Y|"
      case Literals.Comment => "<B|"
    },
    endColor = ">"
  )

  def test(source: String, expected: String) = {
    val highlighted = testHighlight(source.toVector).mkString
    assert(highlighted == expected)
  }
  val tests = TestSuite{
    println("UnitTests")

//    'comment - test("//a", "")
    'literal - test("1", "><G|1>")
//    'expressions - test("val (x, y) = 1 + 2 + 3", "><Y|val> (x, y) =<G| 1> + <G|2> + <G|3>")
//    'interpolation - test(
//      """(s"hello ${world + 1}")""",
//      """>(<G|s"hello >${world + <G|1>}<G|">)"""
//    )
//    'runOff - test(
//      """(1 + "Hello...""",
//      """>(<G|1> +<G| "Hello..."""
//    )
  }
}
