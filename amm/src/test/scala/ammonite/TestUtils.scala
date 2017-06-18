package ammonite

import java.io.PrintStream

import ammonite.interp.Interpreter
import ammonite.runtime.{Frame, History, Storage}
import ammonite.util._

object TestUtils {
  val sessionPrefix = if (scala2_10) "ammonite.$sess." else ""
  def scala2_10 = scala.util.Properties.versionNumberString.contains("2.10")
  def scala2_11 = scala.util.Properties.versionNumberString.contains("2.11")
  def scala2_12 = scala.util.Properties.versionNumberString.contains("2.12")

  def createTestInterp(storage: Storage, predef: String = "") = {
    val startFrame = Frame.createInitial()
    val interp = new Interpreter(

      printer = Printer(
        new PrintStream(System.out), new PrintStream(System.err),
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
      getFrame = () => startFrame
    )
    interp.initializePredef()
    interp
  }
}
