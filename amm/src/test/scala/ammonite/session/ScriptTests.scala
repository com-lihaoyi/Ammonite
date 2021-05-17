package ammonite.session

import ammonite.{TestRepl, Main, main}
import ammonite.TestUtils._
import ammonite.main.{Defaults, Scripts}

import ammonite.runtime.Storage
import ammonite.util.Res
import utest._

object ScriptTests extends TestSuite{
  val tests = Tests{
    println("ScriptTests")
    val check = new TestRepl()

    val printedScriptPath = """pwd/"amm"/"src"/"test"/"resources"/"scripts""""

    test("exec"){
      test("compilationBlocks"){
        test("loadIvy") - retry(3){ // ivy or maven central seems to be flaky =/ =/ =/
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"LoadIvy.sc")

            @ val r = res
            r: String = "<a href=\\"www.google.com\\">omg</a>"

            """)
          }
        test("preserveImports"){
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"PreserveImports.sc")

            @ val r = res
            r: Left[String, Nothing] = ${Print.Left(value = "\"asd\"")}
            """)
        }
        test("annotation"){
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"Annotation.sc")

            @ val r = res
            r: Int = 24
            """)
        }
        test("syntax"){
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"BlockSepSyntax.sc")

            @ val r = res
            r: Int = 24
            """)
        }
        test("limitImports"){
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"LimitImports.sc")

            @ res
            error: ${check.notFound("res")}
            """)
        }
      }
      test("failures"){
        test("syntaxError"){
          val errorChunk =
            if (check.scala2)
          """
            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
          """
            else
          """
            @ val r = res
            error: val r = res
                    ^^^
                    Not found: res
            Compilation Failed
          """
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"SyntaxError.sc")
            error: CompilationError

            $errorChunk
            """)
        }
        test("compilationError"){
          val errorChunk =
            if (check.scala2)
          """
            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
          """
            else
          """
            @ val r = res
            error: val r = res
                    ^^^
                    Not found: res
            Compilation Failed
          """
          check.session(s"""
            @  import os._

            @ repl.load.exec($printedScriptPath/"CompilationError.sc")
            error: Compilation Failed

            $errorChunk
            """)
        }
        test("nofile"){
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        test("multiBlockError"){
          val errorChunk =
            if (check.scala2)
          """
            @ val r2 = res2
            error: not found: value res2
            val r2 = res2
                     ^
            Compilation Failed
          """
            else
          """
            @ val r2 = res2
            error: val r2 = res2
                     ^^^^
                     Not found: res2
            Compilation Failed
          """
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"MultiBlockError.sc")
            error: Compilation Failed

            $errorChunk
            """)
        }
      }
      test("nestedScripts"){
        check.session(s"""
          @ import os._

          @ repl.load.exec($printedScriptPath/"NestedScripts.sc")

          @ val a = asd
          error: ${check.notFound("asd")}

          @ val b = asd2
          b: Int = 1
          """)
      }
      test("sheBang"){
        test("singleLine"){
          check.session(s"""
            @  import os._

            @ repl.load.exec($printedScriptPath/"SheBang.sc")

            @ val r = res
            r: Int = 42
            """)
        }
        test("multiLine"){
          check.session(
            s"""
            @  import os._

            @ repl.load.exec($printedScriptPath/"MultilineSheBang.sc")

            @ val r = res
            r: Int = 42
            """)
        }
      }

    }

    test("module"){
      test("compilationBlocks"){
        test("loadIvy"){
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"LoadIvy.sc")

            @ val r = res
            r: String = "<a href=\\"www.google.com\\">omg</a>"
           """)
        }
        test("preserveImports"){
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"PreserveImports.sc")

            @ val r = res
            r: Left[String, Nothing] = ${Print.Left(value = "\"asd\"")}
            """)

        }
        test("annotation"){

          check.session(s"""
          @ import os._

          @ interp.load.module($printedScriptPath/"Annotation.sc")

          @ val r = res
          r: Int = 24
          """)
        }
        test("syntax"){
            check.session(s"""
              @ import os._

              @ interp.load.module($printedScriptPath/"BlockSepSyntax.sc")

              @ val r = res
              r: Int = 24
            """)
        }
        test("limitImports"){
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"LimitImports.sc")

            @ res
            error: ${check.notFound("res")}
            """)
        }
      }
      test("failures"){
        test("syntaxError"){
          val errorChunk =
            if (check.scala2)
              """
            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
              """
            else
              """
            @ val r = res
            error: val r = res
                    ^^^
                    Not found: res
            Compilation Failed
              """
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"SyntaxError.sc")
            error: CompilationError

            $errorChunk
            """)
        }
        test("compilationError"){
          val errorChunk =
            if (check.scala2)
              """
            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
              """
            else
              """
            @ val r = res
            error: val r = res
                    ^^^
                    Not found: res
            Compilation Failed
              """
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"CompilationError.sc")
            error: Compilation Failed

            $errorChunk""")
        }
        test("nofile"){
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        test("scriptWithoutExtension"){
          val storage = new Storage.Folder(os.temp.dir(prefix = "ammonite-tester"))
          val interp2 = createTestInterp(
            storage,
            predefImports = ammonite.interp.Interpreter.predefImports
          )

          val Res.Failure(msg) =
            Scripts.runScript(os.pwd, os.pwd/"scriptWithoutExtension", interp2)

          assert(msg.contains("Script file not found"))
        }
        test("multiBlockError"){
          val errorCheck =
            if (check.scala2)
              """
                @ val r2 = res2
                error: not found: value res2
                val r2 = res2
                         ^
                Compilation Failed
              """
            else
              s"""
                @ val r2 = res2
                error: val r2 = res2
                         ^^^^
                         Not found: res2
                Compilation Failed
              """
          check.session(s"""
                @ import os._

                @ interp.load.module($printedScriptPath/"MultiBlockError.sc")
                error: Compilation Failed
          """ + errorCheck)
        }
      }
      test("encapsulation"){
        check.session(s"""
            @ import os._

            @ val asd = "asd"

            @ interp.load.module($printedScriptPath/"Encapsulation.sc")
            error: ${check.notFound("asd")}
            """
        )
      }
      test("nestedScripts"){
        check.session(s"""
          @ import os._

          @ interp.load.module($printedScriptPath/"NestedScripts.sc")

          @ val a = asd
          error: ${check.notFound("asd")}

          @ val b = asd2
          b: Int = 1
          """)
      }
      test("noUnWrapping"){
        if (check.scala2) check.session(s"""
          @ import os._

          @ interp.load.module($printedScriptPath/"ScriptDontUnwrap.sc")

          @ foo
          res2: String = "foo def"

          @ wrappedValue
          error: ${check.notFound("wrappedValue")}
        """) else "Disabled in Scala 3"
        // not sure why, in Scala 3, the parser slurps the first '{'…
      }
      test("resolverWithinScript"){
        test("pass"){
          if (scala2_11) check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"Resolvers.sc")


          """)
        }
        test("fail"){
          if (scala2_11) check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"ResolversFail.sc")
            error: Failed to resolve ivy dependencies
          """)
        }
      }
      test("resolverStatic"){
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"ResolversStatic.sc")
          """)
      }
      test("loadIvyAdvanced"){
        check.session(s"""
        @ import os._

        @ interp.load.module($printedScriptPath/"loadIvyAdvanced.sc")

        @ serializer
        """)
      }
    }
  }
}
