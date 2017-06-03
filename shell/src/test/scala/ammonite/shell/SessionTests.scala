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

        @ import ammonite.ops.ImplicitWd

        @ %%ls 'ops
        res3: $typeString =
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

        @ ln.s('srcDir0, 'destSymLink)

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

        @ val tmpdir = tmp.dir()

        @ cd! tmpdir

        @ val names = Seq('test123, 'test124, 'test125, 'test126)

        @ mkdir! wd/'test123

        @ ln.s(wd/'test123, wd/'test124)

        @ ln.s(wd/'test124, wd/'test125)

        @ ln.s(wd/'test125, wd/'test126)

        @ cd! 'test126

        @ assert(wd.tryFollowLinks.get == (tmpdir/'test123).tryFollowLinks.get)

        @ assert(wd == tmpdir/'test126)

        @ cd! tmpdir

        @ assert(wd == tmpdir)

        @ rm! 'test123

        @ assert( (wd/'test126).tryFollowLinks == None)

        @ names.foreach(p => rm! wd/p)

        @ names.foreach(p => assert(!exists(wd/p)))

        @ cd! originalWd

        @ rm! tmpdir
      """)
    }
    'opsInSymlinkedDir {
      // test mkdir, write, read, stat/stat.full, cp, and ls inside a symlinked directory
      // (both while wd = symlinked and outside)
      check.session(
        s"""
        @ import ammonite.ops._

        @ interp.load.module($bareSrc)

        @ val originalWd = wd

        @ val tmpdir = tmp.dir()

        @ cd! tmpdir

        @ mkdir! wd/'test123

        @ ln.s(wd/'test123, wd/'test124)

        @ ln.s(wd/'test124, wd/'test125)

        @ ln.s(wd/'test125, wd/'test126)

        @ cd! 'test126

        @ mkdir! 'test130

        @ assert(exists(wd/'test130))

        @ cd! 'test130

        @ assert(wd == tmpdir/'test126/'test130)

        @ write('test131, "abcdef") // writing while inside symlinked dir

        @ assert(read('test131) == "abcdef") // reading while inside symlinked dir

        @ cp('test131, 'test132)

        @ cd! up

        @ assert(wd == tmpdir/'test126)

        @ write('test132, "qqq")

        @ assert(read('test132) == "qqq")

        @ cp('test132, 'test133) // cp inside symlinked dir while inside that dir

        @ assert( (ls!).toList == List(wd/'test130, wd/'test132, wd/'test133)) // ls inside dir

        @ assert(!stat('test132).isDir && !stat('test132).isSymLink && stat('test130).isDir)

        @ assert(!stat.full('test132).isDir && !stat.full('test132).isSymLink)

        @ assert(stat.full('test130).isDir)

        @ assert(ls.rec(wd).length == 5)

        @ cd! originalWd

        @ val t = tmpdir/'test126 // this dir is symlinked

        @ assert(read(t/'test130/'test131) == "abcdef") // reading while outside symlinked dir

        @ assert(read(t/'test132) == "qqq")

        @ write.over(t/'test132, "www") // writing into file in symlinked dir

        @ assert(read(t/'test132) == "www")

        @ assert(read(t/'test133) == "qqq")

        @ assert(ls(t).toList == List(t/'test130, t/'test132, t/'test133))

        @ cp(t/'test133, t/'test134) // cp inside symlinked dir while outside that dir

        @ assert(read(t/'test134) == "qqq")

        @ assert(ls.rec(tmpdir).length == 10) // ls.rec of symlinked dirs while outside that dir

        @ assert(ls.rec(t).length == 6)

        @ assert(!stat(t/'test132).isDir && !stat(t/'test132).isSymLink)

        @ assert(!stat.full(t/'test132).isDir && !stat.full(t/'test132).isSymLink)

        @ assert(stat(t/'test130).isDir && stat(t).isSymLink)

        @ assert(stat.full(t/'test130).isDir && stat.full(t).isSymLink)

        @ rm! tmpdir
      """)
    }

  }
}
