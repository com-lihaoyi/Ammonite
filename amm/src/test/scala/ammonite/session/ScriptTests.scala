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

    val printedScriptPath = """pwd/'amm/'src/'test/'resources/'scripts"""

    test("exec"){
      test("compilationBlocks"){
        test("loadIvy") - retry(3){ // ivy or maven central seems to be flaky =/ =/ =/
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"LoadIvy.sc").toNIO)

            @ val r = res
            r: String = "<a href=\\"www.google.com\\">omg</a>"

            """)
          }
        test("preserveImports"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"PreserveImports.sc").toNIO)

            @ val r = res
            r: Left[String, Nothing] = Left("asd")
            """)
        }
        test("annotation"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"Annotation.sc").toNIO)

            @ val r = res
            r: Int = 24
            """)
        }
        test("syntax"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"BlockSepSyntax.sc").toNIO)

            @ val r = res
            r: Int = 24
            """)
        }
        test("limitImports"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"LimitImports.sc").toNIO)

            @ res
            error: not found: value res
            """)
        }
      }
      test("failures"){
        test("syntaxError"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"SyntaxError.sc").toNIO)
            error: CompilationError

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
            """)
        }
        test("compilationError"){
          check.session(s"""
            @  import os._

            @ repl.load.exec(($printedScriptPath/"CompilationError.sc").toNIO)
            error: Compilation Failed

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
            """)
        }
        test("nofile"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"notHere").toNIO)
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        test("multiBlockError"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"MultiBlockError.sc").toNIO)
            error: Compilation Failed

            @ val r2 = res2
            error: not found: value res2
            val r2 = res2
                     ^
            Compilation Failed
            """)
        }
      }
      test("nestedScripts"){
        check.session(s"""
          @ import os._

          @ repl.load.exec(($printedScriptPath/"NestedScripts.sc").toNIO)

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
      }
      test("sheBang"){
        test("singleLine"){
          check.session(s"""
            @  import os._

            @ repl.load.exec(($printedScriptPath/"SheBang.sc").toNIO)

            @ val r = res
            r: Int = 42
            """)
        }
        test("multiLine"){
          check.session(
            s"""
            @  import os._

            @ repl.load.exec(($printedScriptPath/"MultilineSheBang.sc").toNIO)

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

            @ interp.load.module(($printedScriptPath/"LoadIvy.sc").toNIO)

            @ val r = res
            r: String = "<a href=\\"www.google.com\\">omg</a>"
           """)
        }
        test("preserveImports"){
          check.session(s"""
            @ import os._

            @ interp.load.module(($printedScriptPath/"PreserveImports.sc").toNIO)

            @ val r = res
            r: Left[String, Nothing] = Left("asd")
            """)

        }
        test("annotation"){

          check.session(s"""
          @ import os._

          @ interp.load.module(($printedScriptPath/"Annotation.sc").toNIO)

          @ val r = res
          r: Int = 24
          """)
        }
        test("syntax"){
            check.session(s"""
              @ import os._

              @ interp.load.module(($printedScriptPath/"BlockSepSyntax.sc").toNIO)

              @ val r = res
              r: Int = 24
            """)
        }
        test("limitImports"){
          check.session(s"""
            @ import os._

            @ interp.load.module(($printedScriptPath/"LimitImports.sc").toNIO)

            @ res
            error: not found: value res
            """)
        }
      }
      test("failures"){
        test("syntaxError"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"SyntaxError.sc").toNIO)
            error: CompilationError

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
            """)
        }
        test("compilationError"){
          check.session(s"""
            @ import os._

            @ interp.load.module(($printedScriptPath/"CompilationError.sc").toNIO)
            error: Compilation Failed

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed""")
        }
        test("nofile"){
          check.session(s"""
            @ import os._

            @ repl.load.exec(($printedScriptPath/"notHere").toNIO)
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
          check.session(s"""
            @ import os._

            @ interp.load.module(($printedScriptPath/"MultiBlockError.sc").toNIO)
            error: Compilation Failed

            @ val r2 = res2
            error: not found: value res2
            val r2 = res2
                     ^
            Compilation Failed
            """)
        }
      }
      test("encapsulation"){
        check.session(s"""
            @ import os._

            @ val asd = "asd"

            @ interp.load.module(($printedScriptPath/"Encapsulation.sc").toNIO)
            error: not found: value asd
            """
        )
      }
      test("nestedScripts"){
        check.session(s"""
          @ import os._

          @ interp.load.module(($printedScriptPath/"NestedScripts.sc").toNIO)

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
      }
      test("noUnWrapping"){
        check.session(s"""
          @ import os._

          @ interp.load.module(($printedScriptPath/"ScriptDontUnwrap.sc").toNIO)

          @ foo
          res2: String = "foo def"

          @ wrappedValue
          error: not found: value wrappedValue
        """)
      }
      test("resolverWithinScript"){
        test("pass"){
          if (scala2_11) check.session(s"""
            @ import os._

            @ interp.load.module(($printedScriptPath/"Resolvers.sc").toNIO)


          """)
        }
        test("fail"){
          if (scala2_11) check.session(s"""
            @ import os._

            @ interp.load.module(($printedScriptPath/"ResolversFail.sc").toNIO)
            error: Failed to resolve ivy dependencies
          """)
        }
      }
      test("resolverStatic"){
          check.session(s"""
            @ import os._

            @ interp.load.module(($printedScriptPath/"ResolversStatic.sc").toNIO)
          """)
      }
      test("loadIvyAdvanced"){
        check.session(s"""
        @ import os._

        @ interp.load.module(($printedScriptPath/"loadIvyAdvanced.sc").toNIO)

        @ serializer
        """)
      }
    }
  }
}
