package ammonite.interp

import ammonite.TestUtils._

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
      if (scala.util.Properties.versionNumberString.startsWith("2.12.")){
        val storage = Storage.InMemory()
        val interp = createTestInterp(storage)
        Scripts.runScript(os.pwd, scriptPath / "configureCompiler.sc", interp)

        assert(interp.compilerManager.compiler.compiler.useOffsetPositions)
      }
    }

    test("preConfigureYrangepos"){
      // In this test, the script sets -Yrangepos using "preConfigureCompiler",
      // which is called BEFORE the compiler instantiates, resulting in
      // useOffsetPositions initializing as false, as expected
      val storage = Storage.InMemory()
      val interp = createTestInterp(storage)
      Scripts.runScript(os.pwd, scriptPath / "preConfigureCompiler.sc", interp)

      assert(!interp.compilerManager.compiler.compiler.useOffsetPositions)
    }
  }
}
