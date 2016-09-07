package ammonite

import ammonite.TestUtils._
import ammonite.ops._
import ammonite.main.Defaults
import ammonite.runtime.{Storage}
import org.scalatest.FreeSpec

class ScriptTests extends FreeSpec {

  def check = new TestRepl()

  val printedScriptPath = """pwd/'src/'test/'resources/'scripts"""

  "exec" - {
    "compilationBlocks" - {
      "loadIvy" in { // ivy or maven central seems to be flaky =/ =/ =/
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"LoadIvy.sc")

            @ val r = res
            r: String = ${"\"\"\""}
            <a href="www.google.com">omg</a>
            ${"\"\"\""}
            """)
      }
      "preserveImports" in {
        val typeString = """Left[String, Nothing]"""
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"PreserveImports.sc")

            @ val r = res
            r: $typeString = Left("asd")
            """)
      }
      "annotation" in {
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"Annotation.sc")

            @ val r = res
            r: Int = 24
            """)
      }
      "syntax" in {
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"BlockSepSyntax.sc")

            @ val r = res
            r: Int = 24
            """)
      }
      "limitImport" in {
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"LimitImports.sc")

            @ res
            error: not found: value res
            """)
      }
    }
    "failures" - {
      "syntaxError" in {
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
      "compilationError" in {
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
      "nofile" in {
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """)
      }
      "multiBlockError" in {
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
    "nestedScripts" in {
      check.session(s"""
          @ import ammonite.ops._

          @ interp.load.exec($printedScriptPath/"NestedScripts.sc")

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
    }
    "sheBang" in {
      check.session(s"""
            @  import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"SheBang.sc")

            @ val r = res
            r: Int = 42
            """)
    }

  }

  "module" - {
    "compilationBlocks" - {
      "loadIvy" in {
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.module($printedScriptPath/"LoadIvy.sc")

            @ val r = res
            r: String = ${"\"\"\""}
            <a href="www.google.com">omg</a>
            ${"\"\"\""}
           """)
      }
      "preserveImports" in {
        val typeString = """Left[String, Nothing]"""
        check.session(s"""
              @ import ammonite.ops._

              @ interp.load.module($printedScriptPath/"PreserveImports.sc")

              @ val r = res
              r: $typeString = Left("asd")
              """)
      }
      "annotation" in {
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.module($printedScriptPath/"Annotation.sc")

            @ val r = res
            r: Int = 24
            """)
      }
      "syntax" in {
        check.session(s"""
              @ import ammonite.ops._

              @ interp.load.module($printedScriptPath/"BlockSepSyntax.sc")

              @ val r = res
              r: Int = 24
            """)
      }
      "limitImports" in {
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.module($printedScriptPath/"LimitImports.sc")

            @ res
            error: not found: value res
            """)
      }
    }
    "failures" - {
      "syntaxError" in {
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
      "compilationError" in {
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
      "nofile" in {
        check.session(s"""
            @ import ammonite.ops._

            @ interp.load.exec($printedScriptPath/"notHere")
            error: java.nio.file.NoSuchFileException
            """)
      }
      "scriptWithoutExtension" in {
        val res = intercept[java.nio.file.NoSuchFileException] {
          val storage =
            new Storage.Folder(tmp.dir(prefix = "ammonite-tester"))
          val interp2 = createTestInterp(
            storage,
            Defaults.predefString
          )
          interp2.interpApi.load.module(pwd / "scriptWithoutExtension")
        }.toString
        assert(res.contains("java.nio.file.NoSuchFileException"))
      }
      "multiBlockError" in {
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
    "encapsulation" in {
      check.session(s"""
            @ import ammonite.ops._

            @ val asd = "asd"

            @ interp.load.module($printedScriptPath/"Encapsulation.sc")
            error: not found: value asd
            """)
    }
    "nestedScripts" in {
      check.session(s"""
          @ import ammonite.ops._

          @ interp.load.module($printedScriptPath/"NestedScripts.sc")

          @ val a = asd
          error: not found: value asd

          @ val b = asd2
          b: Int = 1
          """)
    }
    "noUnWrapping" in {
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
