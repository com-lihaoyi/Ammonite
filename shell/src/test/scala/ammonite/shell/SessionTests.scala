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

        @ val originalWd = wd

        @ val tmpdir = tmp.dir()

        @ cd! tmpdir

        @ mkdir! 'srcDir0

        @ ln.s!('srcDir0)! 'destSymLink

        @ cd! 'destSymLink

        @ assert("srcDir0" == wd.tryFollowLinks.get.name)

        @ assert("destSymLink" == wd.name)

        @ cd! originalWd

        @ rm! tmpdir
      """)
    }

    'nestedSymlinks {
      check.session(
        s"""
        @ import ammonite.ops._

        @ interp.load.module($bareSrc)

        @ val originalWd = wd

        @ val names = Seq('test123, 'test124, 'test125, 'test126)

        @ names.foreach(p => rm! wd/p)

        @ mkdir! wd/'test123

        @ ln.s(wd/'test123, wd/'test124)

        @ ln.s(wd/'test124, wd/'test125)

        @ ln.s(wd/'test125, wd/'test126)

        @ cd! 'test126

        @ assert(wd.tryFollowLinks.get == originalWd/'test123)

        @ assert(wd == originalWd/'test126)

        @ cd! originalWd

        @ assert(wd == originalWd)

        @ rm! 'test123

        @ assert( (wd / 'test126).tryFollowLinks == None )

        @ names.foreach(p => rm! wd/p)

        @ names.foreach(p => assert(!exists(wd/p)))
      """)
    }
  }
}
