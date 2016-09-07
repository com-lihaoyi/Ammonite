package ammonite.shell

import ammonite.TestRepl
import ammonite.TestUtils._
import utest._

/**
 * Created by haoyi on 8/30/15.
 */
object SessionTests extends TestSuite{

  val bareSrc =
    """pwd/'shell/'src/'main/'resources/'ammonite/'shell/"example-predef-bare.sc""""

  val tests = TestSuite{
    val check = new TestRepl()
    'workingDir{
      check.session(s"""
        @ import ammonite.ops._

        @ interp.load.module($bareSrc)

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

        @ interp.load.module($bareSrc)

        @ import ammonite.shell.PPrints._

        @ import ammonite.ops.ImplicitWd

        @ %%ls 'ops
        res4: $typeString =
        src
        target
      """)
    }

    'cdIntoDirSymlink {
      check.session(
        s"""
        @ import ammonite.ops._

        @ interp.load.module($bareSrc)

        @ rm! 'destSymLink

        @ val originalWd = wd

        @ val srcDir0 = tmp.dir()

        @ ln.s!(srcDir0)! 'destSymLink

        @ cd! 'destSymLink

        @ assert(srcDir0.name == wd.name)

        @ rm! 'destSymLink

        @ cd! up

        @ rm! srcDir0
      """)
    }
  }
}
