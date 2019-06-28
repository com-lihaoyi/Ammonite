package ammonite

import java.io.PrintStream

import ammonite.interp.{CodeWrapper, Interpreter, Preprocessor}
import ammonite.main.Defaults
import ammonite.runtime.{Frame, Storage}
import ammonite.util._
import ammonite.runtime.ImportHook

object TestUtils {
  def scala2_11 = scala.util.Properties.versionNumberString.contains("2.11")
  def scala2_12 = scala.util.Properties.versionNumberString.contains("2.12")

  def createTestInterp(storage: Storage, predef: String = "") = {
    val startFrame = Frame.createInitial(
      classOf[ammonite.interp.api.InterpAPI].getClassLoader
    )
    val printStream = new PrintStream(System.out)
    val interp = new Interpreter(

      printer = Printer(
        printStream, new PrintStream(System.err), printStream,
        println, println, println
      ),
      storage = storage,
      wd = os.pwd,
      // Provide a custom predef so we can verify in tests that the predef gets cached
      basePredefs = Seq(),
      customPredefs = Seq(
        PredefInfo(Name("predef"), predef, false, None)
      ),
      extraBridges = Seq(),
      colors = Ref(Colors.BlackWhite),
      getFrame = () => startFrame,
      createFrame = () => throw new Exception("unsupported"),
      replCodeWrapper = CodeWrapper,
      scriptCodeWrapper = CodeWrapper,
      alreadyLoadedDependencies = Defaults.alreadyLoadedDependencies("amm-test-dependencies.txt"),
      importHooks = ImportHook.defaults
    )
    interp.initializePredef()
    interp
  }
}
