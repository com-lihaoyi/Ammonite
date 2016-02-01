package ammonite.shell

import ammonite.repl.TestRepl
import ammonite.repl.TestUtils._
import utest._

/**
 * Created by haoyi on 8/30/15.
 */
object SessionTests extends TestSuite{
  val check = new TestRepl()
  val tests = TestSuite{
    'workingDir{
      check.session(s"""
        @ import ammonite.ops._

        @ load.module(cwd/'shell/'src/'main/'resources/'ammonite/'shell/"example-predef-bare.scala")

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
    'specialPPrint{
      // Make sure these various "special" data structures get pretty-printed
      // correctly, i.e. not as their underlying type but as something more
      // pleasantly human-readable
      val typeString =
        if (!scala2_10) "CommandResult"
        else "ammonite.ops.CommandResult"
      check.session(s"""
        @ import ammonite.ops._

        @ load.module(cwd/'shell/'src/'main/'resources/'ammonite/'shell/"example-predef-bare.scala")

        @ import ammonite.shell.PPrints._

        @ import ammonite.ops.ImplicitWd

        @ %%ls 'ops
        res4: $typeString =
        src
        target
      """)

    }
  }
}
