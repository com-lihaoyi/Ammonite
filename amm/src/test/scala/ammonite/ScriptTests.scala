package ammonite

import ammonite.TestUtils.scala2_10
import ammonite.interp.{History, Interpreter, Storage}
import ammonite.main.Defaults
import ammonite.ops._
import ammonite.tools.IvyConstructor._
import ammonite.util.{Colors, Printer, Ref, Timer}
import utest._

object ScriptTests extends TestSuite{
  val tests = TestSuite{
    println("ScriptTests")
    val check = new TestRepl()

    val scriptPath = cwd/'amm/'src/'test/'resources/'scripts
    val printedScriptPath = """cwd/'amm/'src/'test/'resources/'scripts"""

    val resourcesPath = cwd/'amm/'src/'test/'resources

    'exec{
      'compilationBlocks{
        'loadIvy - retry(3){ // ivy or maven central seems to be flaky =/ =/ =/
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"LoadIvy.scala")

            @ val r = res
            r: String = ${"\"\"\""}
            <a href="www.google.com">omg</a>
            ${"\"\"\""}
            """)
        }
        'preserveImports{
          val typeString =
            if (!scala2_10)
              """Left[String, Nothing]"""
            else
              """util.Left[String,Nothing]"""
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"PreserveImports.scala")

            @ val r = res
            r: $typeString = Left("asd")
            """)
        }
        'annotation{
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"Annotation.scala")

            @ val r = res
            r: Int = 24
            """)
        }
        'syntax{
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"BlockSepSyntax.scala")

            @ val r = res
            r: Int = 24
            """)
        }
        'limitImports{
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"LimitImports.scala")

            @ res
            error: not found: value res
            """)
        }
      }
      'failures{
        'syntaxError{
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"SyntaxError.scala")
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
            @  import ammonite.ops._

            @ load.exec($printedScriptPath/"CompilationError.scala")
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
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        'multiBlockError{
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"MultiBlockError.scala")
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
          @ import ammonite.ops._

          @ load.exec($printedScriptPath/"NestedScripts.scala")

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
      }
      'sheBang{
        check.session(s"""
            @  import ammonite.ops._

            @ load.exec($printedScriptPath/"SheBang.scala")

            @ val r = res
            r: Int = 42
            """)
      }

    }

    'module{
      'compilationBlocks{
        'loadIvy{
          check.session(s"""
            @ import ammonite.ops._

            @ load.module($printedScriptPath/"LoadIvy.scala")

            @ val r = res
            r: String = ${"\"\"\""}
            <a href="www.google.com">omg</a>
            ${"\"\"\""}
            """)
        }
        'preserveImports{
          val typeString =
            if (!scala2_10)
              """Left[String, Nothing]"""
            else
              """util.Left[String,Nothing]"""
          check.session(s"""
              @ import ammonite.ops._

              @ load.module($printedScriptPath/"PreserveImports.scala")

              @ val r = res
              r: $typeString = Left("asd")
              """)

        }
        'annotation{
          if (!scala2_10) //buggy in 2.10
            check.session(s"""
            @ import ammonite.ops._

            @ load.module($printedScriptPath/"Annotation.scala")

            @ val r = res
            r: Int = 24
            """)
        }
        'syntax{
          check.session(s"""
            @ import ammonite.ops._

            @ load.module($printedScriptPath/"BlockSepSyntax.scala")

            @ val r = res
            r: Int = 24
          """)
        }
        'limitImports{
          check.session(s"""
            @ import ammonite.ops._

            @ load.module($printedScriptPath/"LimitImports.scala")

            @ res
            error: not found: value res
            """)
        }
      }
      'failures{
        'syntaxError{
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"SyntaxError.scala")
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
            @ import ammonite.ops._

            @ load.module($printedScriptPath/"CompilationError.scala")
            error: Compilation Failed

            @ val r = res
            error: not found: value res
            val r = res
                    ^
            Compilation Failed""")
        }
        'nofile{
          check.session(s"""
            @ import ammonite.ops._

            @ load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        'multiBlockError{
          check.session(s"""
            @ import ammonite.ops._

            @ load.module($printedScriptPath/"MultiBlockError.scala")
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
            @ import ammonite.ops._

            @ val asd = "asd"

            @ load.module($printedScriptPath/"Encapsulation.scala")
            error: not found: value asd
            """
        )
      }
      'nestedScripts{
        check.session(s"""
          @ import ammonite.ops._

          @ load.module($printedScriptPath/"NestedScripts.scala")

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
      }
    }
  }
}
