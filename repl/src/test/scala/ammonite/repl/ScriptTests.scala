package ammonite.repl

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
    val scriptPath = "repl/src/test/resource/scripts"
    'exec{
      'compilationBlocks{
        'loadIvy{
          check.session(s"""
            @ load.exec("$scriptPath/LoadIvy.scala")

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
            @ load.exec("$scriptPath/PreserveImports.scala")

            @ val r = res
            r: $typeString = Left("asd")
            """)
        }
        'annotation{
          check.session(s"""
            @ load.exec("$scriptPath/Annotation.scala")

            @ val r = res
            r: Int = 24
            """)
        }
        'syntax{
          check.session(s"""
            @ load.exec("$scriptPath/BlockSepSyntax.scala")

            @ val r = res
            r: Int = 24
            """)
        }
      }
      'failures{
        'syntaxError{
          check.session(s"""
            @ load.exec("$scriptPath/SyntaxError.scala")
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
            @ load.exec("$scriptPath/CompilationError.scala")
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
            @ load.exec("$scriptPath/notHere")
            error: java.nio.file.NoSuchFileException: $scriptPath/notHere
            """
          )
        }
        'multiBlockError{
          check.session(s"""
            @ load.exec("$scriptPath/MultiBlockError.scala")
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
          if (!scala2_10) //buggy in 2.10
          check.session(s"""
            @ load.module("$scriptPath/LoadIvy.scala")

            @ val r = res
            r: String = ${"\"\"\""}
            <a href="www.google.com">omg</a>
            ${"\"\"\""}
            """)
        }
        'preserveImports{
          if (!scala2_10) { //buggy in 2.10
            val typeString =
              if (!scala.util.Properties.versionString.contains("2.10"))
                """Left[String, Nothing]"""
              else
                """util.Left[String,Nothing]"""
            check.session(s"""
              @ load.module("$scriptPath/PreserveImports.scala")

              @ val r = res
              r: $typeString = Left("asd")
              """)
          }
        }
        'annotation{
          if (!scala2_10) //buggy in 2.10
          check.session(s"""
            @ load.module("$scriptPath/Annotation.scala")

            @ val r = res
            r: Int = 24
            """)
        }
        'syntax{
          if (!scala2_10) //buggy in 2.10
          check.session(s"""
            @ load.module("$scriptPath/BlockSepSyntax.scala")

            @ val r = res
            r: Int = 24
            """)
        }
      }
      'failures{
        'syntaxError{
          check.session(s"""
            @ load.exec("$scriptPath/SyntaxError.scala")
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
            @ load.module("$scriptPath/CompilationError.scala")
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
            @ load.exec("$scriptPath/notHere")
            error: java.nio.file.NoSuchFileException: $scriptPath/notHere
            """
            )
        }
        'multiBlockError{
          check.session(s"""
            @ load.module("$scriptPath/MultiBlockError.scala")
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
          predef = ""
        )

        'blocks{
          'one{
            val storage = new MemoryStorage
            val interp = createTestInterp(storage)
            interp.replApi.load.module(s"$scriptPath/OneBlock.scala")
            assert(storage.compileCache.size==2) //ReplBridge adds an object to cache
          }
          'two{
            val storage = new MemoryStorage
            val interp = createTestInterp(storage)
            interp.replApi.load.module(s"$scriptPath/TwoBlocks.scala")
            assert(storage.compileCache.size==3)
          }
          'three{
            val storage = new MemoryStorage
            val interp = createTestInterp(storage)
            interp.replApi.load.module(s"$scriptPath/ThreeBlocks.scala")
            assert(storage.compileCache.size==4)
          }
        }
        'persistence{
          if (!scala2_10) {//buggy in 2.10
            val tempDir = java.nio.file.Files.createTempDirectory("ammonite-tester").toFile
            val interp1 = createTestInterp(Storage(tempDir))
            val interp2 = createTestInterp(Storage(tempDir))
            interp1.replApi.load.module(s"$scriptPath/OneBlock.scala")
            interp2.replApi.load.module(s"$scriptPath/OneBlock.scala")
            assert(interp1.eval.compilationCount == 2) //first init adds a compilation because of ReplBridge
            assert(interp2.eval.compilationCount == 0)
          }
        }
        'tags{
          if (!scala2_10) {//buggy in 2.10
            val storage = new MemoryStorage
            val interp = createTestInterp(storage)
            interp.replApi.load.module(s"$scriptPath/TagBase.scala")
            interp.replApi.load.module(s"$scriptPath/TagPrevCommand.scala")
            interp.replApi.load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")
            interp.replApi.load.module(s"$scriptPath/TagBase.scala")
            assert(storage.compileCache.size == 7) //two blocks for each loading + ReplBridge
          }
        }
        'encapsulation{
          check.session(s"""
            @ val asd = "asd"

            @ load.module("$scriptPath/Encapsulation.scala")
            error: not found: value asd
            """
            )
        }
        'noAutoIncrementWrapper{
          val storage = new MemoryStorage
          val interp = createTestInterp(storage)
          interp.replApi.load.module(s"$scriptPath/ThreeBlocks.scala")
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
