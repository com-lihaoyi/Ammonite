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

    'exec{
      'compilationBlocks{
        'loadIvy - retry(3){ // ivy or maven central seems to be flaky =/ =/ =/
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"LoadIvy.sc")

            @ val r = res
            r: String = "<a href=\\"www.google.com\\">omg</a>"

            """)
          }
        'preserveImports{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"PreserveImports.sc")

            @ val r = res
            r: Left[String, Nothing] = Left("asd")
            """)
        }
        'annotation{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"Annotation.sc")

            @ val r = res
            r: Int = 24
            """)
        }
        'syntax{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"BlockSepSyntax.sc")

            @ val r = res
            r: Int = 24
            """)
        }
        'limitImports{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"LimitImports.sc")

            @ res
            error: not found: value res
            """)
        }
      }
      'failures{
        'syntaxError{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"SyntaxError.sc")
            error: CompilationError

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
            """)
        }
        'compilationError{
          check.session(s"""
            @  import os._

            @ repl.load.exec($printedScriptPath/"CompilationError.sc")
            error: Compilation Failed

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
            """)
        }
        'nofile{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        'multiBlockError{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"MultiBlockError.sc")
            error: Compilation Failed

            @ val r2 = res2
            error: not found: value res2
            val r2 = res2
                     ^
            Compilation Failed
            """)
        }
      }
      'nestedScripts{
        check.session(s"""
          @ import os._

          @ repl.load.exec($printedScriptPath/"NestedScripts.sc")

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
      }
      'sheBang{
        'singleLine{
          check.session(s"""
            @  import os._

            @ repl.load.exec($printedScriptPath/"SheBang.sc")

            @ val r = res
            r: Int = 42
            """)
        }
        'multiLine {
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

    'module{
      'compilationBlocks{
        'loadIvy{
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"LoadIvy.sc")

            @ val r = res
            r: String = "<a href=\\"www.google.com\\">omg</a>"
           """)
        }
        'preserveImports{
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"PreserveImports.sc")

            @ val r = res
            r: Left[String, Nothing] = Left("asd")
            """)

        }
        'annotation{

          check.session(s"""
          @ import os._

          @ interp.load.module($printedScriptPath/"Annotation.sc")

          @ val r = res
          r: Int = 24
          """)
        }
        'syntax{
            check.session(s"""
              @ import os._

              @ interp.load.module($printedScriptPath/"BlockSepSyntax.sc")

              @ val r = res
              r: Int = 24
            """)
        }
        'limitImports{
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"LimitImports.sc")

            @ res
            error: not found: value res
            """)
        }
      }
      'failures{
        'syntaxError{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"SyntaxError.sc")
            error: CompilationError

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed
            """)
        }
        'compilationError{
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"CompilationError.sc")
            error: Compilation Failed

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed""")
        }
        'nofile{
          check.session(s"""
            @ import os._

            @ repl.load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        'scriptWithoutExtension{
          val storage = new Storage.Folder(os.temp.dir(prefix = "ammonite-tester"))
          val interp2 = createTestInterp(
            storage,
            Defaults.predefString + Main.extraPredefString
          )

          val Res.Failure(msg) =
            Scripts.runScript(os.pwd, os.pwd/"scriptWithoutExtension", interp2)

          assert(msg.contains("Script file not found"))
        }
        'multiBlockError{
          check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"MultiBlockError.sc")
            error: Compilation Failed

            @ val r2 = res2
            error: not found: value res2
            val r2 = res2
                     ^
            Compilation Failed
            """)
        }
      }
      'encapsulation{
        check.session(s"""
            @ import os._

            @ val asd = "asd"

            @ interp.load.module($printedScriptPath/"Encapsulation.sc")
            error: not found: value asd
            """
        )
      }
      'nestedScripts{
        check.session(s"""
          @ import os._

          @ interp.load.module($printedScriptPath/"NestedScripts.sc")

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
      }
      'noUnWrapping{
        check.session(s"""
          @ import os._

          @ interp.load.module($printedScriptPath/"ScriptDontUnwrap.sc")

          @ foo
          res2: String = "foo def"

          @ wrappedValue
          error: not found: value wrappedValue
        """)
      }
      'resolverWithinScript{
        'pass - {
          if (!scala2_12) check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"Resolvers.sc")


          """)
        }
        'fail - {
          if (!scala2_12) check.session(s"""
            @ import os._

            @ interp.load.module($printedScriptPath/"ResolversFail.sc")
            error: Failed to resolve ivy dependencies
          """)
        }
      }
      'loadIvyAdvanced{
        check.session(s"""
        @ import os._

        @ interp.load.module($printedScriptPath/"loadIvyAdvanced.sc")

        @ serializer
        """)
      }
    }
  }
}
