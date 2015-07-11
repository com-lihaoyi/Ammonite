package ammonite.repl

import ammonite.repl.frontend.Highlighter
import ammonite.repl.frontend.Highlighter._
import utest._

import scala.collection.{immutable => imm}
import scalaparse.Scala._
import scalaparse.syntax.Identifiers._

object UnitTests extends TestSuite{

  def testHighlight(buffer: Vector[Char]) = Highlighter.highlight(
    Parsers.Splitter,
    buffer,
    {
      case Literals.Expr.Interp | Literals.Pat.Interp => ">"
      case ExprLiteral => "<G|"
      case Type => "<G|"
      case BackTicked(body) if alphaKeywords.contains(body) => "<Y|"
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

//    'historyPPrint{
//      import pprint._
//      val hist = new History(Vector("1","2","{\nblock\n}"))
//
//      implicit val cfg = new Config
//      val pprinted = implicitly[PPrint[History]].render(hist, cfg).mkString
//      assert(pprinted == "\n@ 1\n@ 2\n@ {\nblock\n}\n")
//    }
    'comment - test("//a", "><B|//a>")
    'literal - test("1", "><G|1>")
    'expressions - test("val (x, y) = 1 + 2 + 3", "><Y|val> (x, y) = <G|1> + <G|2> + <G|3>")
    'interpolation - test(
      """(s"hello ${world + 1}")""",
      """>(<G|s"hello >${world + <G|1>}<G|">)"""
    )
    'runOff - test(
      """(1 + "Hello...""",
      """>(<G|1> + <G|"Hello..."""
    )
    'underscore - test(
      """val _ = 1""",
      """><Y|val> <Y|_> = <G|1>"""
    )
    'nonTrivial - test(
      """def processLine(stmts: Seq[String],
                          saveHistory: (String => Unit, String) => Unit,
                          printer: Iterator[String] => Unit) = for{
            _ <- Catching { case Ex(x@_*) =>
              val Res.Failure(trace) = Res.Failure(x)
              Res.Failure(trace + "\nSomething unexpected went wrong =(")
            }
            Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
            _ = saveHistory(history.append(_), stmts.mkString("; "))
            oldClassloader = Thread.currentThread().getContextClassLoader
            out <- try{
              Thread.currentThread().setContextClassLoader(eval.evalClassloader)
              eval.processLine(
                code,
                s"ReplBridge.shell.Internal.combinePrints(${printSnippet.mkString(", ")})",
                printer
              )
            } finally Thread.currentThread().setContextClassLoader(oldClassloader)
          } yield out
      """,
      """><Y|def> processLine(stmts: <G|Seq[String]>,
                          saveHistory: <G|(String => Unit, String) => Unit>,
                          printer: <G|Iterator[String] => Unit>) = <Y|for>{
            <Y|_> <- Catching { <Y|case> Ex(x@<Y|_>*) =>
              <Y|val> Res.Failure(trace) = Res.Failure(x)
              Res.Failure(trace + <G|"\nSomething unexpected went wrong =(">)
            }
            Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
            <Y|_> = saveHistory(history.append(<Y|_>), stmts.mkString(<G|"; ">))
            oldClassloader = Thread.currentThread().getContextClassLoader
            out <- <Y|try>{
              Thread.currentThread().setContextClassLoader(eval.evalClassloader)
              eval.processLine(
                code,
                <G|s"ReplBridge.shell.Internal.combinePrints(>${printSnippet.mkString(<G|", ">)}<G|)">,
                printer
              )
            } <Y|finally> Thread.currentThread().setContextClassLoader(oldClassloader)
          } <Y|yield> out
      """
    )
  }
}
