package ammonite.unit

import ammonite.compiler.Parsers
import utest._

object HighlightTests extends TestSuite{

  def testHighlight(buffer: Vector[Char]) = Parsers.defaultHighlight(
    buffer,
    fansi.Color.Blue,
    fansi.Color.Green,
    fansi.Color.Green,
    fansi.Color.Yellow,
    fansi.Color.Red,
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
  val isScala2 = ammonite.compiler.CompilerBuilder.scalaVersion.startsWith("2.")
  val tests = Tests {
    println("HighlightTests")
    test("highlighting"){
      test("fuzz"){


        val paths = os.walk(os.pwd).filter(_.ext == "scala")
        for(path <- paths){
          val code = os.read(path)
          val out = Parsers.defaultHighlight(
            code.toVector,
            fansi.Underlined.On,
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
      test("type")- {
        val expected =
          if (isScala2) "x: <G|y>.<Y|type>"
          else "x: <G|y.type>"
        check("x: y.type", expected)
      }
      test("literal")- check("1", "<G|1>")
      test("expressions")- check("val (x, y) = 1 + 2 + 3", "<Y|val> (x, y) = <G|1> + <G|2> + <G|3>")
      test("interpolation")- check(
        """(s"hello ${world + 1}")""",
        """(<G|s"hello >${world + <G|1>}<G|">)"""
      )
      test("runOff")- {
        val expected =
          if (isScala2) """(<G|1> + <G|"Hello...>"""
          else """(<G|1> + "Hello...""" // Dotty highlighter doesn't color open strings
        check("""(1 + "Hello...""", expected)
      }
      test("underscore")- {
        val expected =
          if (isScala2) """<Y|val> <Y|_> = <G|1>"""
          else """<Y|val> _ = <G|1>"""
        check("""val _ = 1""", expected)
      }
      test("nonTrivial")- {
        val underscore =
          if (isScala2) "<Y|_>"
          else "_"
        val underscore0 =
          if (isScala2) "<Y|_>"
          else "_"
        def stringColl(collType: String) =
          if (isScala2) s"<G|$collType>[<G|String>]"
          else s"<G|$collType[String]>"
        check(
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
        s"""<Y|def> processLine(stmts: ${stringColl("Seq")},
                            saveHistory: (<G|String> => <G|Unit>, <G|String>) => <G|Unit>,
                            printer: ${stringColl("Iterator")} => <G|Unit>) = <Y|for>{
              $underscore <- Catching { <Y|case> Ex(x@$underscore0*) =>
                <Y|val> Res.Failure(trace) = Res.Failure(x)
                Res.Failure(trace + <G|"\\nSomething unexpected went wrong =(">)
              }
              Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
              $underscore = saveHistory(history.append($underscore), stmts.mkString(<G|"; ">))
              oldClassloader = Thread.currentThread().getContextClassLoader
              out <- <Y|try>{
                Thread.currentThread().setContextClassLoader(eval.evalClassloader)
                eval.processLine(
                  code,
                  <G|s"ReplBridge.combinePrints(>$${printSnippet.mkString(<G|", ">)}<G|)">,
                  printer
                )
              } <Y|finally> Thread.currentThread().setContextClassLoader(oldClassloader)
            } <Y|yield> out
        """
        )
      }
    }
  }
}
