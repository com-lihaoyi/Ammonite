package test.ammonite.ops

import java.nio.file.NoSuchFileException
import java.nio.{file => nio}

import ammonite.ops._

import utest._
object OpTests extends TestSuite{

  val tests = Tests {
    val res = pwd/'ops/'src/'test/'resources/'testdata
    test("ls") - assert(
      ls(res).toSet == Set(res/'folder1, res/'folder2, res/"File.txt"),
      ls(res/'folder2).toSet == Set(
        res/'folder2/'folder2a,
        res/'folder2/'folder2b
      )

//      ls(res/'folder2/'folder2b) == Seq()
    )
    test("lsR"){
      ls.rec(res).foreach(println)
      intercept[java.nio.file.NoSuchFileException](ls.rec(pwd/'target/'nonexistent))
      assert(
        ls.rec(res/'folder2/'folder2b) == Seq(res/'folder2/'folder2b/"b.txt"),
        ls.rec(res/'folder2) == Seq(
          res/'folder2/'folder2a,
          res/'folder2/'folder2b,
          res/'folder2/'folder2a/"I am.txt",
          res/'folder2/'folder2b/"b.txt"
        ),
        ls.rec(res) == Seq(
          res/"File.txt",
          res/'folder1,
          res/'folder2,
          res/'folder1/"Yoghurt Curds Cream Cheese.txt",
          res/'folder2/'folder2a,
          res/'folder2/'folder2b,
          res/'folder2/'folder2a/"I am.txt",
          res/'folder2/'folder2b/"b.txt"
        )
      )
    }
    test("lsRecPermissions"){
      if(Unix()){
        assert(ls.rec(root/'var/'run).nonEmpty)
      }
    }
    test("readResource"){
      test("positive"){
        test("absolute"){
          val contents = read(resource/'test/'ammonite/'ops/'folder/"file.txt")
          assert(contents.contains("file contents lols"))

          val cl = getClass.getClassLoader
          val contents2 = read(resource(cl)/'test/'ammonite/'ops/'folder/"file.txt")
          assert(contents2.contains("file contents lols"))
        }

        test("relative"){
          val cls = classOf[test.ammonite.ops.Testing]
          val contents = read! resource(cls)/'folder/"file.txt"
          assert(contents.contains("file contents lols"))

          val contents2 = read! resource(getClass)/'folder/"file.txt"
          assert(contents2.contains("file contents lols"))
        }
      }
      test("negative"){
        test - intercept[ResourceNotFoundException]{
          read(resource/'folder/"file.txt")
        }

        test - intercept[ResourceNotFoundException]{
          read(resource(classOf[test.ammonite.ops.Testing])/'test/'ammonite/'ops/'folder/"file.txt")
        }
        test - intercept[ResourceNotFoundException]{
          read(resource(getClass)/'test/'ammonite/'ops/'folder/"file.txt")
        }
        test - intercept[ResourceNotFoundException]{
          read(resource(getClass.getClassLoader)/'folder/"file.txt")
        }
      }
    }
    test("rm"){
      // shouldn't crash
      rm! pwd/'target/'nonexistent
    }
    test("Mutating"){
      val test = pwd/'target/'test
      rm! test
      mkdir! test
      test("cp"){
        val d = test/'copying
        test("basic"){
          assert(
            !exists(d/'folder),
            !exists(d/'file)
          )
          mkdir! d/'folder
          write(d/'file, "omg")
          assert(
            exists(d/'folder),
            exists(d/'file),
            read(d/'file) == "omg"
          )
          cp(d/'folder, d/'folder2)
          cp(d/'file, d/'file2)

          assert(
            exists(d/'folder),
            exists(d/'file),
            read(d/'file) == "omg",
            exists(d/'folder2),
            exists(d/'file2),
            read(d/'file2) == "omg"
          )
        }
        test("deep"){
          write(d/'folderA/'folderB/'file, "Cow", createFolders = true)
          cp(d/'folderA, d/'folderC)
          assert(read(d/'folderC/'folderB/'file) == "Cow")
        }
      }
      test("mv"){
        test("basic"){
          val d = test/'moving
          mkdir! d/'folder
          assert(ls(d) == Seq(d/'folder))
          mv(d/'folder, d/'folder2)
          assert(ls(d) == Seq(d/'folder2))
        }
        test("shallow"){
          val d = test/'moving2
          mkdir(d)
          write(d/"A.scala", "AScala")
          write(d/"B.scala", "BScala")
          write(d/"A.py", "APy")
          write(d/"B.py", "BPy")
          def fileSet = ls(d).map(_.last).toSet
          assert(fileSet == Set("A.scala", "B.scala", "A.py", "B.py"))
          test("partialMoves"){
            ls! d | mv{case r"$x.scala" => s"$x.java"}
            assert(fileSet == Set("A.java", "B.java", "A.py", "B.py"))
            ls! d | mv{case r"A.$x" => s"C.$x"}
            assert(fileSet == Set("C.java", "B.java", "C.py", "B.py"))
          }
          test("fullMoves"){
            ls! d | mv.all{case r"$x.$y" => s"$y.$x"}
            assert(fileSet == Set("scala.A", "scala.B", "py.A", "py.B"))
            def die = ls! d | mv.all{case r"A.$x" => s"C.$x"}
            intercept[MatchError]{ die }
          }
        }
        test("deep"){
          val d = test/'moving2
          mkdir(d)
          mkdir(d/'scala)
          mkdir(d/'py)
          write(d/'scala/'A, "AScala")
          write(d/'scala/'B, "BScala")
          write(d/'py/'A, "APy")
          write(d/'py/'B, "BPy")
          test("partialMoves"){
            ls.rec! d | mv*{case d/"py"/x => d/x }
            assert(
              ls.rec(d).toSet == Set(
                d/'py,
                d/'scala,
                d/'scala/'A,
                d/'scala/'B,
                d/'A,
                d/'B
              )
            )
          }
          test("fullMoves"){
            def die = ls.rec! d | mv.all*{case d/"py"/x => d/x }
            intercept[MatchError]{ die }

            ls.rec! d |? (_.isFile) | mv.all*{
              case d/"py"/x => d/'scala/'py/x
              case d/"scala"/x => d/'py/'scala/x
              case d => println("NOT FOUND " + d); d
            }

            assert(
              ls.rec(d).toSet == Set(
                d/'py,
                d/'scala,
                d/'py/'scala,
                d/'scala/'py,
                d/'scala/'py/'A,
                d/'scala/'py/'B,
                d/'py/'scala/'A,
                d/'py/'scala/'B
              )
            )
          }
        }
        //          ls! wd | mv*
      }
      test("mkdirRm"){
        test("singleFolder"){
          val single = test/'single
          mkdir! single/'inner
          assert(ls(single) == Seq(single/'inner))
          rm! single/'inner
          assert(ls(single) == Seq())
        }
        test("nestedFolders"){
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
      test("readWrite"){
        val d = test/'readWrite
        mkdir! d
        test("simple"){
          write(d/'file, "i am a cow")
          assert(read(d/'file) == "i am a cow")
        }
        test("autoMkdir"){
          write(d/'folder/'folder/'file, "i am a cow", createFolders = true)
          assert(read(d/'folder/'folder/'file) == "i am a cow")
        }
        test("binary"){
          write(d/'file, Array[Byte](1, 2, 3, 4))
          assert(read(d/'file).toSeq == Array[Byte](1, 2, 3, 4).toSeq)
        }
        test("concatenating"){
          write(d/'concat1, Seq("a", "b", "c"))
          assert(read(d/'concat1) == "abc")
          write(d/'concat2, Array(Array[Byte](1, 2), Array[Byte](3, 4)))
          assert(read.bytes(d/'concat2).toSeq == Array[Byte](1, 2, 3, 4).toSeq)
        }
        test("writeAppend"){
          write.append(d/"append.txt", "Hello")
          assert(read(d/"append.txt") == "Hello")
          write.append(d/"append.txt", " World")
          assert(read(d/"append.txt") == "Hello World")
        }
        test("writeOver"){
          write.over(d/"append.txt", "Hello")
          assert(read(d/"append.txt") == "Hello")
          write.over(d/"append.txt", " Wor")
          assert(read(d/"append.txt") == " Wor")
        }
      }
      test("Failures"){
        val d = test/'failures
        mkdir! d
        test("nonexistant"){
          test - intercept[nio.NoSuchFileException](ls! d/'nonexistent)
          test - intercept[nio.NoSuchFileException](read! d/'nonexistent)
          test - intercept[ResourceNotFoundException](read! resource/'failures/'nonexistent)
          test - intercept[nio.NoSuchFileException](cp(d/'nonexistent, d/'yolo))
          test - intercept[nio.NoSuchFileException](mv(d/'nonexistent, d/'yolo))
        }
        test("collisions"){
          mkdir! d/'folder
          write(d/'file, "lolol")
          test - intercept[nio.FileAlreadyExistsException](mv(d/'file, d/'folder))
          test - intercept[nio.FileAlreadyExistsException](cp(d/'file, d/'folder))
          test - intercept[nio.FileAlreadyExistsException](write(d/'file, "lols"))
         }
      }
    }
  }
}
