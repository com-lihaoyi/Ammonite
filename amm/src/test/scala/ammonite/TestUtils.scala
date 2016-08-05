package ammonite

import ammonite.interp.{History, Interpreter, Storage}
import ammonite.util.{Colors, Printer, Ref, Util}

object TestUtils {
  val sessionPrefix = if (scala2_10) "$sess." else ""
  def scala2_10 = scala.util.Properties.versionNumberString.contains("2.10")

  def createTestInterp(storage: Storage, predef: String = "") = new Interpreter(
    Ref[String](""),
    Ref(null),
    80,
    80,
    Ref(Colors.BlackWhite),
    printer = Printer(_ => (), _ => (), _ => (), _ => ()),
    storage = storage,
    new History(Vector()),
    predef = predef,
    wd = ammonite.ops.cwd,
    replArgs = Seq()
  )
}
