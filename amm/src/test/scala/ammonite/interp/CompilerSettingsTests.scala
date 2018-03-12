package ammonite.interp

import ammonite.TestUtils._
import ammonite.ops._
import ammonite.runtime.Storage
import ammonite.main.Scripts
import utest._

object CompilerSettingsTests extends TestSuite {
  val tests = Tests {
    println("CompilerSettingsTests")

    val scriptPath = pwd / 'amm / 'src / 'test / 'resources / 'scriptCompilerSettings

    'configureYrangepos {
      // In this test, the script sets -Yrangepos to true using "configureCompiler",
      // which is called AFTER the compiler instantiates. As useOffsetPositions
      // is set eagerly during the compiler instantiation as !Yrangepos, its
      // value remains "true".
      val storage = Storage.InMemory()
      val interp = createTestInterp(storage)
      Scripts.runScript(pwd, scriptPath / "configureCompiler.sc", interp)

      assert(interp.compilerManager.compiler.compiler.useOffsetPositions)
    }

    'preConfigureYrangepos {
      // In this test, the script sets -Yrangepos using "preConfigureCompiler",
      // which is called BEFORE the compiler instantiates, resulting in
      // useOffsetPositions initializing as false, as expected
      val storage = Storage.InMemory()
      val interp = createTestInterp(storage)
      Scripts.runScript(pwd, scriptPath / "preConfigureCompiler.sc", interp)

      assert(!interp.compilerManager.compiler.compiler.useOffsetPositions)
    }
  }
}
