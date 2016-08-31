package ammonite

import ammonite.runtime.{History, Interpreter, Storage}
import ammonite.util._

object TestUtils {
  val sessionPrefix = if (scala2_10) "$sess." else ""
  def scala2_10 = scala.util.Properties.versionNumberString.contains("2.10")

  def createTestInterp(storage: Storage, predef: String = "") = new Interpreter(
    printer = Printer(_ => (), _ => (), _ => (), _ => ()),
    storage = storage,
    wd = ammonite.ops.pwd,
    // Provide a custom predef so we can verify in tests that the predef gets cached
    customPredefs = Seq(
      Name("predef") -> predef
    ),
    extraBridges = _ => Seq()
  )
}
