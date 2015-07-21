package ammonite.repl

import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import ammonite.repl.IvyConstructor._
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
            if (!scala.util.Properties.versionString.contains("2.10"))
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
            @ load.module("$scriptPath/LoadIvy.scala")

            @ val r = res
            r: String = ${"\"\"\""}
            <a href="www.google.com">omg</a>
            ${"\"\"\""}
            """)
        }
        'preserveImports{
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
        'annotation{
          check.session(s"""
            @ load.module("$scriptPath/Annotation.scala")

            @ val r = res
            r: Int = 24
            """)
        }
        'syntax{
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
        'blocks{
          'one{
            val storage = new MemoryStorage
            val interp = new Interpreter(
              Ref[String](""),
              Ref(null),
              pprint.Config.Defaults.PPrintConfig.copy(height = 15),
              Ref(ColorSet.BlackWhite),
              stdout = _ => (),
              storage = Ref(storage),
              predef = ""
            )
            interp.replApi.load.module(s"$scriptPath/OneBlock.scala")
            assert(storage.compileCache.size==1)
          }
          'two{
            val storage = new MemoryStorage
            val interp = new Interpreter(
              Ref[String](""),
              Ref(null),
              pprint.Config.Defaults.PPrintConfig.copy(height = 15),
              Ref(ColorSet.BlackWhite),
              stdout = _ => (),
              storage = Ref(storage),
              predef = ""
            )
            interp.replApi.load.module(s"$scriptPath/TwoBlocks.scala")
            assert(storage.compileCache.size==2)
          }
          'three{
            val storage = new MemoryStorage
            val interp = new Interpreter(
              Ref[String](""),
              Ref(null),
              pprint.Config.Defaults.PPrintConfig.copy(height = 15),
              Ref(ColorSet.BlackWhite),
              stdout = _ => (),
              storage = Ref(storage),
              predef = ""
            )
            interp.replApi.load.module(s"$scriptPath/ThreeBlocks.scala")
            assert(storage.compileCache.size==3)
          }
        }
        'persistence{
          val tempDir = java.nio.file.Files.createTempDirectory("ammonite-tester").toFile
          val interp1 = new Interpreter(
            Ref[String](""),
            Ref(null),
            pprint.Config.Defaults.PPrintConfig.copy(height = 15),
            Ref(ColorSet.BlackWhite),
            stdout = _ => (),
            storage = Ref(Storage(tempDir)),
            predef = ""
          )
          val interp2 = new Interpreter(
            Ref[String](""),
            Ref(null),
            pprint.Config.Defaults.PPrintConfig.copy(height = 15),
            Ref(ColorSet.BlackWhite),
            stdout = _ => (),
            storage = Ref(Storage(tempDir)),
            predef = ""
          )
          interp1.replApi.load.module(s"$scriptPath/OneBlock.scala")
          interp2.replApi.load.module(s"$scriptPath/OneBlock.scala")
          assert(interp1.eval.compilationCount == 3) //each reintialization adds a compilation
          assert(interp2.eval.compilationCount == 2)
        }
        'tags{
          val storage = new MemoryStorage
          val interp = new Interpreter(
            Ref[String](""),
            Ref(null),
            pprint.Config.Defaults.PPrintConfig.copy(height = 15),
            Ref(ColorSet.BlackWhite),
            stdout = _ => (),
            storage = Ref(storage),
            predef = ""
          )
          interp.replApi.load.module(s"$scriptPath/TagBase.scala")
          interp.replApi.load.module(s"$scriptPath/TagPrevCommand.scala")
          interp.replApi.load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")
          interp.replApi.load.module(s"$scriptPath/TagBase.scala")
          assert(storage.compileCache.size == 6) //two blocks for each loading
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
          val interp = new Interpreter(
            Ref[String](""),
            Ref(null),
            pprint.Config.Defaults.PPrintConfig.copy(height = 15),
            Ref(ColorSet.BlackWhite),
            stdout = _ => (),
            storage = Ref(storage),
            predef = ""
          )
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
