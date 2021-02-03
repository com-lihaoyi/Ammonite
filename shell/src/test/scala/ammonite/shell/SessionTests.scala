package ammonite.shell

import ammonite.TestRepl
import ammonite.TestUtils._
import utest._

/**
 * Created by haoyi on 8/30/15.
 */
object SessionTests extends TestSuite{

  val bareSrc =
    """pwd/"shell"/"src"/"main"/"resources"/"ammonite"/"shell"/"example-predef-bare.sc""""

  val tests = Tests{
    val check = new TestRepl()
//    test("workingDir"){
//      check.session(s"""
//        @ import ammonite.ops._
//
//        @ interp.load.module($bareSrc)
//
//        @ val originalWd = wd
//
//        @ val originalLs1 = %%ls
//
//        @ val originalLs2 = ls!
//
//        @ cd! up
//
//        @ assert(wd == originalWd/up)
//
//        @ cd! root
//
//        @ assert(wd == root)
//
//        @ assert(originalLs1 != (%%ls))
//
//        @ assert(originalLs2 != (ls!))
//      """)
//    }
    test("specialPPrint"){
      // Make sure these various "special" data structures get pretty-printed
      // correctly, i.e. not as their underlying type but as something more
      // pleasantly human-readable
      check.session(s"""
        @ import ammonite.ops._

        @ interp.load.module($bareSrc)

        @ import ammonite.ops.ImplicitWd

        @ %%ls "ops/src/test"
        res3: CommandResult =
        resources
        scala
      """)
    }

    test("cdIntoDirSymlink"){
      check.session(
        s"""
        @ import ammonite.ops._

        @ interp.load.module($bareSrc)

        @ val originalWd = wd

        @ val tmpdir = tmp.dir()

        @ cd! tmpdir

        @ mkdir! "srcDir0"

        @ ln.s("destSymLink", os.FilePath("srcDir0"))

        @ cd! "destSymLink"

        @ assert("srcDir0" == os.followLink(wd).get.last)

        @ assert("destSymLink" == wd.last)

        @ cd! originalWd

        @ rm! tmpdir
      """)
    }

    test("nestedSymlinks"){
      check.session(
        s"""
        @ import ammonite.ops._

        @ interp.load.module($bareSrc)

        @ val originalWd = wd

        @ val tmpdir = tmp.dir()

        @ cd! tmpdir

        @ val names = Seq("test123", "test124", "test125", "test126")

        @ mkdir! wd/"test123"

        @ ln.s(wd/"test124", wd/"test123")

        @ ln.s(wd/"test125", wd/"test124")

        @ ln.s(wd/"test126", wd/"test125")

        @ cd! "test126"

        @ assert(os.followLink(wd).get == os.followLink(tmpdir/"test123").get)

        @ assert(wd == tmpdir/"test126")

        @ cd! tmpdir

        @ assert(wd == tmpdir)

        @ rm! "test123"

        @ assert(os.followLink(wd/"test126") == None)

        @ names.foreach(p => rm! wd/p)

        @ names.foreach(p => assert(!exists(wd/p)))

        @ cd! originalWd

        @ rm! tmpdir
      """)
    }
    test("opsInSymlinkedDir"){
      // test mkdir, write, read, stat/stat.full, cp, and ls inside a symlinked directory
      // (both while wd = symlinked and outside)
      check.session(
        s"""
        @ import ammonite.ops._

        @ interp.load.module($bareSrc)

        @ val originalWd = wd

        @ val tmpdir = tmp.dir()

        @ cd! tmpdir

        @ mkdir! wd/"test123"

        @ ln.s(wd/"test124", wd/"test123")

        @ ln.s(wd/"test125", wd/"test124")

        @ ln.s(wd/"test126", wd/"test125")

        @ cd! "test126"

        @ mkdir! "test130"

        @ assert(exists(wd/"test130"))

        @ cd! "test130"

        @ assert(wd == tmpdir/"test126"/"test130")

        @ write("test131", "abcdef") // writing while inside symlinked dir

        @ assert(read("test131") == "abcdef") // reading while inside symlinked dir

        @ cp("test131", "test132")

        @ cd! up

        @ assert(wd == tmpdir/"test126")

        @ write("test132", "qqq")

        @ assert(read("test132") == "qqq")

        @ cp("test132", "test133") // cp inside symlinked dir while inside that dir

        @ assert( (ls!).toList == List(wd/"test130", wd/"test132", wd/"test133")) // ls inside dir

        @ assert(!stat("test132").isDir && !stat("test132").isSymLink && stat("test130").isDir)

        @ assert(!stat("test132").isDir && !stat("test132").isSymLink)

        @ assert(stat("test130").isDir)

        @ assert(ls.rec(wd).length == 5)

        @ cd! originalWd

        @ val t = tmpdir/"test126" // this dir is symlinked

        @ assert(read(t/"test130"/"test131") == "abcdef") // reading while outside symlinked dir

        @ assert(read(t/"test132") == "qqq")

        @ write.over(t/"test132", "www") // writing into file in symlinked dir

        @ assert(read(t/"test132") == "www")

        @ assert(read(t/"test133") == "qqq")

        @ assert(ls(t).toList == List(t/"test130", t/"test132", t/"test133"))

        @ cp(t/"test133", t/"test134") // cp inside symlinked dir while outside that dir

        @ assert(read(t/"test134") == "qqq")

        @ assert(ls.rec(tmpdir).length == 10) // ls.rec of symlinked dirs while outside that dir

        @ assert(ls.rec(t).length == 6)

        @ assert(!stat(t/"test132").isDir && !stat(t/"test132").isSymLink)

        @ assert(!stat(t/"test132").isDir && !stat(t/"test132").isSymLink)

        @ assert(stat(t/"test130").isDir && stat(t, followLinks = false).isSymLink)

        @ assert(stat(t/"test130").isDir && stat(t, followLinks = false).isSymLink)

        @ rm! tmpdir
      """)
    }

  }
}
