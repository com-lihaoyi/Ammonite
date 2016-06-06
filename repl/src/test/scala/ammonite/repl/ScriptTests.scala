package ammonite.repl

import ammonite.ops._
import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import ammonite.repl.tools.IvyConstructor._
import TestUtils.scala2_10
import utest._
import acyclic.file
object ScriptTests extends TestSuite{
  val tests = TestSuite{
    println("ScriptTests")
    val check = new TestRepl()

    val scriptPath = cwd/'repl/'src/'test/'resources/'scripts
    val printedScriptPath = """cwd/'repl/'src/'test/'resources/'scripts"""


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
      'caching{

        def createTestInterp(storage: Storage) = new Interpreter(
          Ref[String](""),
          Ref(null),
          80,
          80,
          Ref(Colors.BlackWhite),
          printer = Printer(_ => (), _ => (), _ => (), _ => ()),
          storage = storage,
          history = new History(Vector()),
          predef = "",
          wd = ammonite.ops.cwd,
          replArgs = Seq()
        )

        'blocks{
          val cases = Seq("OneBlock.scala" -> 2, "TwoBlocks.scala" -> 3, "ThreeBlocks.scala" -> 4)
          for((fileName, expected) <- cases){
            val storage = Storage.InMemory()
            val interp = createTestInterp(storage)
            val n0 = storage.compileCache.size

            assert(n0 == 1) // Predef + hardcodedPredef
            interp.replApi.load.module(scriptPath/fileName)

            val n = storage.compileCache.size
            assert(n == expected)
          }
        }
        'persistence{

          val tempDir = ammonite.ops.Path(
            java.nio.file.Files.createTempDirectory("ammonite-tester-x")
          )

          val interp1 = createTestInterp(Storage.Folder(tempDir))
          val interp2 = createTestInterp(Storage.Folder(tempDir))
          interp1.replApi.load.module(scriptPath/"OneBlock.scala")
          interp2.replApi.load.module(scriptPath/"OneBlock.scala")
          val n1 = interp1.eval.compilationCount
          val n2 = interp2.eval.compilationCount
          assert(n1 == 2) // hardcodedPredef + loadedPredef
          assert(n2 == 0) // all three should be cached
        }
        'tags{
          val storage = Storage.InMemory()
          val interp = createTestInterp(storage)
          interp.replApi.load.module(scriptPath/"TagBase.scala")
          interp.replApi.load.module(scriptPath/"TagPrevCommand.scala")
          interp.replApi.load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")
          interp.replApi.load.module(scriptPath/"TagBase.scala")
          val n = storage.compileCache.size
          assert(n == 5) // predef + two blocks for initial load
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
        'noAutoIncrementWrapper{
          val storage = Storage.InMemory()
          val interp = createTestInterp(storage)
          interp.replApi.load.module(scriptPath/"ThreeBlocks.scala")
          try{
            Class.forName("cmd0")
            assert(false)
          } catch {
            case e: ClassNotFoundException => assert(true)
            case e: Exception => assert(false)
          }
        }
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
