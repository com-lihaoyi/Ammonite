package ammonite.unit

import ammonite.repl.api.Clipboard
import ammonite.repl.ClipboardImpl

import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor

import utest._

import scala.util.Try

object ClipboardTests extends TestSuite {

  val clipboard: Clipboard = ClipboardImpl

  /**
   * This test suite requires an environment with access to a window
   * service (either under, Windows, MacOs, X11, ...) CI environment
   * doesn't satisfy that condition and that is detected in the following
   * check in order to skip [[Clipboard]] test when running on CI.
   */
  val canTest = Try(
    Toolkit.getDefaultToolkit.getSystemClipboard.isDataFlavorAvailable(
      DataFlavor.stringFlavor
    )
  ).getOrElse(false)

  override def tests = Tests {
    println("ClipboardTests")
    test("clipboard") {
      val newClipboardContents = "hello Ammonite"
      test("copyandpaste") {
        if (canTest) {
          clipboard.write(newClipboardContents)
          assert(clipboard.read == newClipboardContents)
        } else {
          println(
            "The environment doesn't allow testing clipboard - skipping"
          )
        }
      }
    }
  }
}
