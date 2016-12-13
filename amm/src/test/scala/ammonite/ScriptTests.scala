package ammonite

import ammonite.TestUtils._
import ammonite.ops._
import ammonite.main.Defaults
import utest._
import ammonite.runtime.{Storage}

object ScriptTests extends TestSuite{
  val tests = TestSuite{
    println("ScriptTests")
    val check = new TestRepl()

    val scriptPath = pwd/'amm/'src/'test/'resources/'scripts
    val printedScriptPath = """pwd/'amm/'src/'test/'resources/'scripts"""

    val resourcesPath = pwd/'amm/'src/'test/'resources

    'exec{
      'compilationBlocks{
        'loadIvy - retry(3){ // ivy or maven central seems to be flaky =/ =/ =/
          check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"LoadIvy.sc")

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

            @ interp.load.exec($printedScriptPath/"PreserveImports.sc")

            @ val r = res
            r: $typeString = Left("asd")
            """)
        }
        'annotation{
          check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"Annotation.sc")

            @ val r = res
            r: Int = 24
            """)
        }
        'syntax{
          check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"BlockSepSyntax.sc")

            @ val r = res
            r: Int = 24
            """)
        }
        'limitImports{
          check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"LimitImports.sc")

            @ res
            error: not found: value res
            """)
        }
      }
      'failures{
        'syntaxError{
          check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"SyntaxError.sc")
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

            @ interp.load.exec($printedScriptPath/"CompilationError.sc")
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

            @ interp.load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        'multiBlockError{
          check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"MultiBlockError.sc")
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

          @ interp.load.exec($printedScriptPath/"NestedScripts.sc")

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
      }
      'sheBang{
        'singleLine{
          check.session(s"""
            @  import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"SheBang.sc")

            @ val r = res
            r: Int = 42
            """)
        }
        'multiLine {
          check.session(
            s"""
            @  import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"MultilineSheBang.sc")

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
            @ import ammonite.ops._

            @ interp.load.module($printedScriptPath/"LoadIvy.sc")

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

              @ interp.load.module($printedScriptPath/"PreserveImports.sc")

              @ val r = res
              r: $typeString = Left("asd")
              """)

        }
        'annotation{
          if (!scala2_10) //buggy in 2.10
            check.session(s"""
            @ import ammonite.ops._

            @ interp.load.module($printedScriptPath/"Annotation.sc")

            @ val r = res
            r: Int = 24
            """)
        }
        'syntax{
            check.session(s"""
              @ import ammonite.ops._

              @ interp.load.module($printedScriptPath/"BlockSepSyntax.sc")

              @ val r = res
              r: Int = 24
            """)
        }
        'limitImports{
          check.session(s"""
            @ import ammonite.ops._

            @ interp.load.module($printedScriptPath/"LimitImports.sc")

            @ res
            error: not found: value res
            """)
        }
      }
      'failures{
        'syntaxError{
          check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"SyntaxError.sc")
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
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """
          )
        }
        'scriptWithoutExtension{
          val res = intercept[java.nio.file.NoSuchFileException]{
            val storage = new Storage.Folder(tmp.dir(prefix="ammonite-tester"))
            val interp2 = createTestInterp(
              storage,
              Defaults.predefString
            )
            interp2.interpApi.load.module(pwd/"scriptWithoutExtension")
          }.toString
          assert(res.contains("java.nio.file.NoSuchFileException"))
        }
        'multiBlockError{
          check.session(s"""
            @ import ammonite.ops._

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
            @ import ammonite.ops._

            @ val asd = "asd"

            @ interp.load.module($printedScriptPath/"Encapsulation.sc")
            error: not found: value asd
            """
        )
      }
      'nestedScripts{
        check.session(s"""
          @ import ammonite.ops._

          @ interp.load.module($printedScriptPath/"NestedScripts.sc")

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
      }
      'noUnWrapping{
        check.session(s"""
        @ import ammonite.ops._

        @ interp.load.module($printedScriptPath/"ScriptDontUnwrap.sc")

        @ foo
        res2: String = "foo def"

        @ wrappedValue
        error: not found: value wrappedValue
        """)
      }
    }
  }
}
