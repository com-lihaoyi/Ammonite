package ammonite

import java.io.PrintStream

import ammonite.interp.{Interpreter, Preprocessor}
import ammonite.runtime.{Frame, History, Storage}
import ammonite.util._

object TestUtils {
  def scala2_11 = scala.util.Properties.versionNumberString.contains("2.11")
  def scala2_12 = scala.util.Properties.versionNumberString.contains("2.12")

  def createTestInterp(storage: Storage, predef: String = "") = {
    val startFrame = Frame.createInitial()
    val printStream = new PrintStream(System.out)
    val interp = new Interpreter(

      printer = Printer(
        printStream, new PrintStream(System.err), printStream,
        println, println, println
      ),
      storage = storage,
      wd = ammonite.ops.pwd,
      // Provide a custom predef so we can verify in tests that the predef gets cached
      basePredefs = Seq(),
      customPredefs = Seq(
        PredefInfo(Name("predef"), predef, false, None)
      ),
      extraBridges = Seq(),
      colors = Ref(Colors.BlackWhite),
      getFrame = () => startFrame,
      replCodeWrapper = Preprocessor.CodeWrapper,
      scriptCodeWrapper = Preprocessor.CodeWrapper
    )
    interp.initializePredef()
    interp
  }
}
