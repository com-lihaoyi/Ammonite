package ammonite.tools

import ammonite.repl.Checker
import utest._

/**
 * Created by haoyi on 8/30/15.
 */
object SessionTests extends TestSuite{
  val check = new Checker()
  val tests = TestSuite{
    'workingDir{
      check.session(s"""
        @ import ammonite.ops._

        @ load.module(cwd / RelPath("readme/resources/example-predef.scala"))

        @ val originalWd = wd

        @ val originalLs1 = %%ls

        @ val originalLs2 = ls!

        @ cd! up

        @ assert(wd == originalWd/up)

        @ cd! root

        @ assert(wd == root)

        @ assert(originalLs1 != (%%ls))

        @ assert(originalLs2 != (ls!))
      """)
    }
  }
}
