package ammonite.unit

import ammonite.compiler.Highlighter
import utest._

object HighlightTests extends TestSuite{

  def testHighlight(buffer: Vector[Char]) = Highlighter.defaultHighlight(
    buffer,
    fansi.Color.Blue,
    fansi.Color.Green,
    fansi.Color.Green,
    fansi.Color.Yellow,
    fansi.Attr.Reset
  )

  def check(source: String, expected: String) = {
    val highlighted =
      testHighlight(source.toVector)
        .mkString
        .replace(fansi.Color.Blue.escape, "<B|")
        .replace(fansi.Color.Green.escape, "<G|")
        .replace(fansi.Color.Yellow.escape, "<Y|")
        .replace(fansi.Color.Reset.escape, ">")
    assert(highlighted == expected)
  }
  val tests = Tests {
    println("HighlightTests")
    test("highlighting"){
      test("fuzz"){


        val paths = os.walk(os.pwd).filter(_.ext == "scala")
        for(path <- paths){
          val code = os.read(path)
          val out = Highlighter.defaultHighlight(
            code.toVector,
            fansi.Underlined.On,
            fansi.Underlined.On,
            fansi.Underlined.On,
            fansi.Underlined.On,
            fansi.Attr.Reset
          ).mkString
            .replace(fansi.Underlined.On.escape, "")
            .replace(fansi.Underlined.Off.escape, "")
          val outLength = out.length
          val codeLength = code.length

          assert{identity(path); outLength == codeLength}
          assert{identity(path); out == code}
        }
        paths.length
      }
      test("comment")- check("//a", "<B|//a>")
      test("type")- check("x: y.type", "x: <G|y>.<Y|type>")
      test("literal")- check("1", "<G|1>")
      test("expressions")- check("val (x, y) = 1 + 2 + 3", "<Y|val> (x, y) = <G|1> + <G|2> + <G|3>")
      test("interpolation")- check(
        """(s"hello ${world + 1}")""",
        """(<G|s"hello >${world + <G|1>}<G|">)"""
      )
      test("runOff")- check(
        """(1 + "Hello...""",
        """(<G|1> + <G|"Hello...>"""
      )
      test("underscore")- check(
        """val _ = 1""",
        """<Y|val> <Y|_> = <G|1>"""
      )
      test("nonTrivial")- check(
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
                  s"ReplBridge.combinePrints(${printSnippet.mkString(", ")})",
                  printer
                )
              } finally Thread.currentThread().setContextClassLoader(oldClassloader)
            } yield out
        """,
        """<Y|def> processLine(stmts: <G|Seq>[<G|String>],
                            saveHistory: (<G|String> => <G|Unit>, <G|String>) => <G|Unit>,
                            printer: <G|Iterator>[<G|String>] => <G|Unit>) = <Y|for>{
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
                  <G|s"ReplBridge.combinePrints(>${printSnippet.mkString(<G|", ">)}<G|)">,
                  printer
                )
              } <Y|finally> Thread.currentThread().setContextClassLoader(oldClassloader)
            } <Y|yield> out
        """
      )
    }
  }
}
