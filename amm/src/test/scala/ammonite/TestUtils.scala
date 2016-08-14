package ammonite

import ammonite.interp.{History, Interpreter, Storage}
import ammonite.util._

object TestUtils {
  val sessionPrefix = if (scala2_10) "$sess." else ""
  def scala2_10 = scala.util.Properties.versionNumberString.contains("2.10")

  def createTestInterp(storage: Storage, predef: String = "") = new Interpreter(
    printer = Printer(_ => (), _ => (), _ => (), _ => ()),
    storage = storage,
    predef = predef,
    wd = ammonite.ops.cwd,
    // Provide a custom predef so we can verify in tests that the predef gets cached
    customPredefs = Seq("val customLolz = 1" -> Name("custom")),
    extraBridges = _ => Seq()
  )
}
