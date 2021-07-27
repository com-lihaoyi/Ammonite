package ammonite.interp

import ammonite.TestUtils._

import ammonite.compiler.test.CompilerTestExtensions._
import ammonite.runtime.Storage
import ammonite.main.Scripts
import utest._

object CompilerSettingsTests extends TestSuite {
  val tests = Tests {
    println("CompilerSettingsTests")

    val scriptPath = os.pwd / 'amm / 'src / 'test / 'resources / 'scriptCompilerSettings

    test("configureYrangepos"){
    
      // In this test, the script sets -Yrangepos to true using "configureCompiler",
      // which is called AFTER the compiler instantiates. As useOffsetPositions
      // is set eagerly during the compiler instantiation as !Yrangepos, its
      // value remains "true".
      if (scala2_12){
        val storage = Storage.InMemory()
        val interp = createTestInterp(storage)
        Scripts.runScript(os.pwd, scriptPath / "configureCompiler.sc", interp)

        assert(
          interp
            .compilerManager
            .asInstanceOf[ammonite.compiler.CompilerLifecycleManager]
            .compiler
            .compiler
            .useOffsetPositions
        )
      } else "Disabled"
    }

    test("preConfigureYrangepos"){
      // In this test, the script sets -Yrangepos using "preConfigureCompiler",
      // which is called BEFORE the compiler instantiates, resulting in
      // useOffsetPositions initializing as false, as expected
      if (scala2) {
        val storage = Storage.InMemory()
        val interp = createTestInterp(
          storage,
          predefImports = Interpreter.predefImports
        )
        Scripts.runScript(os.pwd, scriptPath / "preConfigureCompiler.sc", interp)

        assert(
          !interp
            .compilerManager
            .asInstanceOf[ammonite.compiler.CompilerLifecycleManager]
            .compiler
            .compiler
            .useOffsetPositions
        )
      } else "Disabled in Scala 3"
    }
  }
}
