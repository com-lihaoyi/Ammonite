package ammonite

import ammonite.BasePath.NoRelativePathException
import ammonite.Path._
import ammonite.RelPath._
import utest._

object OpTests extends TestSuite{
  val wd = cwd
  val tests = TestSuite {
    val res = wd/'target/"scala-2.11"/"test-classes"/'testdata
    'ls - assert(
      ls(res).toSet == Set(res/'folder1, res/'folder2, res/"File.txt"),
      ls(res/'folder2).toSet == Set(res/'folder2/'folder2a, res/'folder2/'folder2b),
      ls(res/'folder2/'folder2b) == Seq()
    )
    'lsR{
      assert(
        ls.rec(wd/'target/'nonexistent) == Seq(),
        ls.rec(res/'folder2/'folder2b) == Seq(),
        ls.rec(res/'folder2) == Seq(
          res/'folder2/'folder2a,
          res/'folder2/'folder2b,
          res/'folder2/'folder2a/"I am.txt"
        ),
        ls.rec(res) == Seq(
          res/"File.txt",
          res/'folder1,
          res/'folder2,
          res/'folder1/"Yoghurt Curds Cream Cheese.txt",
          res/'folder2/'folder2a,
          res/'folder2/'folder2b,
          res/'folder2/'folder2a/"I am.txt"
        )
      )
    }

    'rm{
      // shouldn't crash
      rm! wd/'target/'nonexistent
    }
    'Mutating{
      val test = wd/'target/'test
      rm! test
      mkdir! test

      'mv{
        val moving = test/'moving
        mkdir! moving/'folder
        assert(ls(moving) == Seq(moving/'folder))
        mv! moving/'folder ! moving/'folder2
        assert(ls(moving) == Seq(moving/'folder2))
      }

      'mkdirRm{
        'singleFolder{
          val single = test/'single
          mkdir! single/'inner
          assert(ls(single) == Seq(single/'inner))
          rm! single/'inner
          assert(ls(single) == Seq())
        }
        'nestedFolders{
          val nested = test/'nested
          mkdir! nested/'inner/'innerer/'innerest
          assert(
            ls(nested) == Seq(nested/'inner),
            ls(nested/'inner) == Seq(nested/'inner/'innerer),
            ls(nested/'inner/'innerer) == Seq(nested/'inner/'innerer/'innerest)
          )
          rm! nested/'inner
          assert(ls(nested) == Seq())
        }
      }
      'readWrite{
        val readWrite = test/'readWrite
        mkdir! readWrite
        'simple{
          write! readWrite/'file ! "i am a cow"
          assert(read(readWrite/'file) == "i am a cow")
        }
        'autoMkdir{
          write! readWrite/'folder/'folder/'file ! "i am a cow"
          assert(read(readWrite/'folder/'folder/'file) == "i am a cow")
        }
        'binary{
          write! readWrite/'file ! Array[Byte](1, 2, 3, 4)
          assert(read(readWrite/'file).toSeq == Array[Byte](1, 2, 3, 4).toSeq)
        }
      }
    }
  }
}
