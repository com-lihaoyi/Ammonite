package ammonite.repl

import ammonite.ops._
import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import ammonite.repl.IvyConstructor._
import TestUtils.scala2_10
import utest._
import acyclic.file
object ScriptTests extends TestSuite{
  val tests = TestSuite{
    println("ScriptTests")
    val check = new Checker()

    val scriptPath = cwd/RelPath("repl/src/test/resource/scripts")
    val printedScriptPath = {
      import pprint.Config.Defaults.PPrintConfig
      pprint.tokenize(scriptPath)
            .mkString
    }

    'exec{
      'compilationBlocks{
        'loadIvy{
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
            error: SyntaxError

            @ val r = res
            error: Compilation Failed
            Main.scala:\\d\\+: not found: value res
            res
            ^
            """)
        }
        'compilationError{
          check.session(s"""
            @  import ammonite.ops._

            @ load.exec($printedScriptPath/"CompilationError.scala")
            error: Compilation Failed

            @ val r = res
            error: Compilation Failed
            Main.scala:\\d\\+: not found: value res
            res
            ^

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

            @ val r1 = res1
            r1: Int = 1

            @ val r2 = res2
            error: Compilation Failed
            Main.scala:\\d\\+: not found: value res2
            res2 
            ^
            """)
        }
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
            error: SyntaxError

            @ val r = res
            error: Compilation Failed
            Main.scala:\\d\\+: not found: value res
            res
            ^
            """)
        }
        'compilationError{
          check.session(s"""
            @ import ammonite.ops._

            @ load.module($printedScriptPath/"CompilationError.scala")
            error: Compilation Failed

            @ val r = res
            error: Compilation Failed
            Main.scala:\\d\\+: not found: value res
            res
            ^

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

            @ load.module($printedScriptPath/"MultiBlockError.scala")
            error: Compilation Failed

            @ val r1 = res1
            r1: Int = 1

            @ val r2 = res2
            error: Compilation Failed
            Main.scala:\\d\\+: not found: value res2
            res2 
            ^
            """)
        }
      }
      'caching{
        
        def createTestInterp(storage: Storage) = new Interpreter(
          Ref[String](""),
          Ref(null),
          80,
          80,
          pprint.Config.Defaults.PPrintConfig.copy(height = 15),
          Ref(Colors.BlackWhite),
          stdout = _ => (),
          storage = Ref(storage),
          new History(Vector()),
          predef = ""
        )

        'blocks{
          val cases = Seq("OneBlock.scala" -> 3, "TwoBlocks.scala" -> 4, "ThreeBlocks.scala" -> 5)
          for((fileName, expected) <- cases){
            val storage = new MemoryStorage
            val interp = createTestInterp(storage)
            val n0 = storage.compileCache.size
            assert(n0 == 2) // Predef + hardcodedPredef
            interp.replApi.load.module(scriptPath/fileName)
            val n = storage.compileCache.size
            assert(n == expected)
          }
        }
        'persistence{

          val tempDir = ammonite.ops.Path(
            java.nio.file.Files.createTempDirectory("ammonite-tester-x")
          )
          println(tempDir)
          val interp1 = createTestInterp(Storage(tempDir))
          val interp2 = createTestInterp(Storage(tempDir))
          interp1.replApi.load.module(scriptPath/"OneBlock.scala")
          interp2.replApi.load.module(scriptPath/"OneBlock.scala")
          val n1 = interp1.eval.compilationCount
          val n2 = interp2.eval.compilationCount
          assert(n1 == 3) // hardcodedPredef + predef + first init
          assert(n2 == 0) // all three should be cached
        }
        'tags{
          val storage = new MemoryStorage
          val interp = createTestInterp(storage)
          interp.replApi.load.module(scriptPath/"TagBase.scala")
          interp.replApi.load.module(scriptPath/"TagPrevCommand.scala")
          interp.replApi.load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")
          interp.replApi.load.module(scriptPath/"TagBase.scala")
          val n = storage.compileCache.size
          assert(n == 9) // predef + two blocks for each loading
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
          val storage = new MemoryStorage
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
    }
  }
}
