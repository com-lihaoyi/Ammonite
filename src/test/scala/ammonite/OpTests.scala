package ammonite

import ammonite.BasePath.NoRelativePathException
import ammonite.Path._
import ammonite.RelPath._
import utest._

object OpTests extends TestSuite{
  val wd = processWorkingDir
  val tests = TestSuite {
    val res = wd/'target/"scala-2.11"/"test-classes"/'testdata
    'ls - assert(
//      ls(res).toSet == Set(res/'folder1, res/'folder2, res/"File.txt"),
//      ls(res/'folder2).toSet == Set(res/'folder2/'folder2a, res/'folder2/'folder2b),
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
        'basic{
          val moving = test/'moving
          mkdir! moving/'folder
          assert(ls(moving) == Seq(moving/'folder))
          mv! moving/'folder ! moving/'folder2
          assert(ls(moving) == Seq(moving/'folder2))
        }
        'shallow{
          val moving = test/'moving2
          write! moving/"A.scala" ! "AScala"
          write! moving/"B.scala" ! "BScala"
          write! moving/"A.py" ! "APy"
          write! moving/"B.py" ! "BPy"
          def fileSet = ls(moving).map(_.last).toSet
          assert(fileSet == Set("A.scala", "B.scala", "A.py", "B.py"))
          'partialMoves{
            ls! moving | mv{case r"$x.scala" => s"$x.java"}
            assert(fileSet == Set("A.java", "B.java", "A.py", "B.py"))
            ls! moving | mv{case r"A.$x" => s"C.$x"}
            assert(fileSet == Set("C.java", "B.java", "C.py", "B.py"))
          }
          'fullMoves{
            ls! moving | mv.all{case r"$x.$y" => s"$y.$x"}
            assert(fileSet == Set("scala.A", "scala.B", "py.A", "py.B"))
            def die = ls! moving | mv.all{case r"A.$x" => s"C.$x"}
            intercept[MatchError]{ die }
          }
        }
        'deep{
          val moving = test/'moving2
          write! moving/'scala/'A ! "AScala"
          write! moving/'scala/'B ! "BScala"
          write! moving/'py/'A ! "APy"
          write! moving/'py/'B ! "BPy"
          'partialMoves{
            ls.rec! moving | mv*{case d/"py"/x => d/x }
            assert(
              ls.rec(moving).toSet == Set(
                moving/'py,
                moving/'scala,
                moving/'scala/'A,
                moving/'scala/'B,
                moving/'A,
                moving/'B
              )
            )
          }
          'fullMoves{
            def die = ls.rec! moving | mv.all*{case d/"py"/x => d/x }
            intercept[MatchError]{ die }

            ls.rec! moving |? (_.isFile) | mv.all*{
              case d/"py"/x => d/'scala/'py/x
              case d/"scala"/x => d/'py/'scala/x
              case d => println("NOT FOUND " + d); d
            }

            ls.rec(moving).toSet.foreach(println)
            assert(
              ls.rec(moving).toSet == Set(
                moving/'py,
                moving/'scala,
                moving/'py/'scala,
                moving/'scala/'py,
                moving/'scala/'py/'A,
                moving/'scala/'py/'B,
                moving/'py/'scala/'A,
                moving/'py/'scala/'B
              )
            )
          }
        }
        //          ls! wd | mv*
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
