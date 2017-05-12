package ammonite

import ammonite.interp.Interpreter
import ammonite.runtime.{History, Storage}
import ammonite.util._

object TestUtils {
  val sessionPrefix = if (scala2_10) "$sess." else ""
  def scala2_10 = scala.util.Properties.versionNumberString.contains("2.10")
  def scala2_11 = scala.util.Properties.versionNumberString.contains("2.11")
  def scala2_12 = scala.util.Properties.versionNumberString.contains("2.12")

  def createTestInterp(storage: Storage, predef: String = "") = new Interpreter(
    printer = Printer(println, println, println, println),
    storage = storage,
    wd = ammonite.ops.pwd,
    // Provide a custom predef so we can verify in tests that the predef gets cached
    customPredefs = Seq(
      Interpreter.PredefInfo(Name("predef"), predef, false)
    ),
    extraBridges = _ => Seq()
  )
}
