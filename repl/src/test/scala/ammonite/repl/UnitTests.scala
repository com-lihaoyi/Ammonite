package ammonite.repl

import ammonite.repl.frontend.Highlighter
import utest._

import acyclic.file
object UnitTests extends TestSuite{

  def testHighlight(buffer: Vector[Char]) = Highlighter.defaultHighlight(
    buffer,
    fansi.Color.Blue,
    fansi.Color.Green,
    fansi.Color.Green,
    fansi.Color.Yellow,
    fansi.Attr.Reset
  )

  def test(source: String, expected: String) = {
    val highlighted =
      testHighlight(source.toVector)
        .mkString
        .replace(fansi.Color.Blue.escape, "<B|")
        .replace(fansi.Color.Green.escape, "<G|")
        .replace(fansi.Color.Yellow.escape, "<Y|")
        .replace(fansi.Color.Reset.escape, ">")
    assert(highlighted == expected)
  }
  val tests = TestSuite {
    println("UnitTests")
    'transpose{
      // Make sure this doesn't stack-overflow
      val inner = List.fill(1000000)(1)
      assert(Util.transpose(List.fill(10)(inner)) == List.fill(1000000)(List.fill(10)(1)))
    }
    'highlighting {
      'fuzz - {
        import ammonite.ops._

        val paths = ls.rec! cwd |? (_.ext == "scala")
        for(path <- paths){
          val code = read! path
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
      'comment - test("//a", "<B|//a>")
      'type - test("x: y.type", "x: <G|y>.<Y|type>")
      'literal - test("1", "<G|1>")
      'expressions - test("val (x, y) = 1 + 2 + 3", "<Y|val> (x, y) = <G|1> + <G|2> + <G|3>")
      'interpolation - test(
        """(s"hello ${world + 1}")""",
        """(<G|s"hello >${world + <G|1>}<G|">)"""
      )
      'runOff - test(
        """(1 + "Hello...""",
        """(<G|1> + <G|"Hello...>"""
      )
      'underscore - test(
        """val _ = 1""",
        """<Y|val> <Y|_> = <G|1>"""
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
