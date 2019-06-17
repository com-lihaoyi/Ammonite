package test.ammonite.ops

import java.nio.file.Paths

import ammonite.ops._
import utest._
object PathTests extends TestSuite{
  val tests = Tests {
    test("Basic"){
      val rel = 'src/'main/'scala
      test("Transformers"){
        if(Unix()){
          assert(
            // ammonite.Path to java.nio.file.Path
            (root/'omg).toNIO == Paths.get("/omg"),

            // java.nio.file.Path to ammonite.Path
            root/'omg == Path(Paths.get("/omg")),
            empty/'omg == RelPath(Paths.get("omg")),

            // ammonite.Path to String
            (root/'omg).toString == "/omg",
            (empty/'omg).toString == "omg",
            (up/'omg).toString == "../omg",
            (up/up/'omg).toString == "../../omg",

            // String to ammonite.Path
            root/'omg == Path("/omg"),
            empty/'omg == RelPath("omg")
          )
        }
      }

      test("RelPath"){
        test("Constructors"){
          test("Symbol"){
            if (Unix()){
              val rel1 = rel / 'ammonite
              assert(
                rel1.segments == Seq("src", "main", "scala", "ammonite"),
                rel1.toString == "src/main/scala/ammonite"
              )
            }
          }
          test("String"){
            if (Unix()){
              val rel1 = rel / "Path.scala"
              assert(
                rel1.segments == Seq("src", "main", "scala", "Path.scala"),
                rel1.toString == "src/main/scala/Path.scala"
              )
            }
          }
          test("Combos"){
            def check(rel1: RelPath) = assert(
              rel1.segments == Seq("src", "main", "scala", "sub1", "sub2"),
              rel1.toString == "src/main/scala/sub1/sub2"
            )
            test("ArrayString"){
              if (Unix()){
                val arr = Array("sub1", "sub2")
                check(rel / arr)
              }
            }
            test("ArraySymbol"){
              if (Unix()){
                val arr = Array('sub1, 'sub2)
                check(rel / arr)
              }
            }
            test("SeqString"){
              if (Unix()) check(rel / Seq("sub1", "sub2"))
            }
            test("SeqSymbol"){
              if (Unix()) check(rel / Seq('sub1, 'sub2))
            }
            test("SeqSeqSeqSymbol"){
              if (Unix()){
                check(
                  rel / Seq(Seq(Seq('sub1), Seq()), Seq(Seq('sub2)), Seq())
                )
              }
            }
          }
        }
        test("Relativize"){
          def eq[T](p: T, q: T) = assert(p == q)
          test - eq('omg/'bbq/'wtf relativeTo 'omg/'bbq/'wtf, empty)
          test - eq('omg/'bbq relativeTo 'omg/'bbq/'wtf, up)
          test - eq('omg/'bbq/'wtf relativeTo 'omg/'bbq, empty/'wtf)
          test - eq('omg/'bbq relativeTo 'omg/'bbq/'wtf, up)
          test - eq(up/'omg/'bbq relativeTo 'omg/'bbq, up/up/up/'omg/'bbq)
          test - intercept[PathError.NoRelativePath]('omg/'bbq relativeTo up/'omg/'bbq)
        }
      }
      test("AbsPath"){
        val d = pwd
        val abs = d / rel
        test("Constructor"){
          if (Unix()) assert(
            abs.toString.drop(d.toString.length) == "/src/main/scala",
            abs.toString.length > d.toString.length
          )
        }
        test("Relativize"){
          def eq[T](p: T, q: T) = assert(p == q)
          test - eq(root/'omg/'bbq/'wtf relativeTo root/'omg/'bbq/'wtf, empty)
          test - eq(root/'omg/'bbq relativeTo root/'omg/'bbq/'wtf, up)
          test - eq(root/'omg/'bbq/'wtf relativeTo root/'omg/'bbq, empty/'wtf)
          test - eq(root/'omg/'bbq relativeTo root/'omg/'bbq/'wtf, up)
          test - intercept[PathError.NoRelativePath]('omg/'bbq relativeTo up/'omg/'bbq)
        }
      }
      test("Ups"){
        test("RelativeUps"){
          val rel2 = rel/up
          assert(
            rel2 == 'src/'main,
            rel/up/up == empty/'src,
            rel/up/up/up == empty,
            rel/up/up/up/up == up,
            rel/up/up/up/up/up == up/up,
            up/rel == up/'src/'main/'scala
          )
        }
        test("AbsoluteUps"){
          // Keep applying `up` and verify that the path gets
          // shorter and shorter and eventually errors.
          var abs = pwd
          var i = abs.segmentCount
          while(i > 0){
            abs/=up
            i-=1
            assert(abs.segmentCount == i)
          }
          intercept[PathError.AbsolutePathOutsideRoot.type]{ abs/up }
        }
        test("RootUpBreak"){
          intercept[PathError.AbsolutePathOutsideRoot.type]{ root/up }
          val x = root/"omg"
          val y = x/up
          intercept[PathError.AbsolutePathOutsideRoot.type]{ y / up }
        }
      }
      test("Comparison"){
        test("Relative") - assert(
          'omg/'wtf == 'omg/'wtf,
          'omg/'wtf != 'omg/'wtf/'bbq,
          'omg/'wtf/'bbq startsWith 'omg/'wtf,
          'omg/'wtf startsWith 'omg/'wtf,
          up/'omg/'wtf startsWith up/'omg/'wtf,
          !('omg/'wtf startsWith 'omg/'wtf/'bbq),
          !(up/'omg/'wtf startsWith 'omg/'wtf),
          !('omg/'wtf startsWith up/'omg/'wtf)
        )
        test("Absolute") - assert(
          root/'omg/'wtf == root/'omg/'wtf,
          root/'omg/'wtf != root/'omg/'wtf/'bbq,
          root/'omg/'wtf/'bbq startsWith root/'omg/'wtf,
          root/'omg/'wtf startsWith root/'omg/'wtf,
          !(root/'omg/'wtf startsWith root/'omg/'wtf/'bbq)
        )
        test("Invalid"){
          compileError("""root/'omg/'wtf < 'omg/'wtf""")
          compileError("""root/'omg/'wtf > 'omg/'wtf""")
          compileError("""'omg/'wtf < root/'omg/'wtf""")
          compileError("""'omg/'wtf > root/'omg/'wtf""")
        }
      }
    }

    test("Errors"){
      test("InvalidChars"){
        val ex = intercept[PathError.InvalidSegment]('src/"Main/.scala")

        val PathError.InvalidSegment("Main/.scala", msg1) = ex

        assert(msg1.contains("[/] is not a valid character to appear in a path segment"))

        val ex2 = intercept[PathError.InvalidSegment](root/"hello"/".."/"world")

        val PathError.InvalidSegment("..", msg2) = ex2

        assert(msg2.contains("use the `up` segment from `os.up`"))
      }
      test("InvalidSegments"){
        intercept[PathError.InvalidSegment]{root/ "core/src/test"}
        intercept[PathError.InvalidSegment]{root/ ""}
        intercept[PathError.InvalidSegment]{root/ "."}
        intercept[PathError.InvalidSegment]{root/ ".."}
      }
      test("EmptySegment"){
        intercept[PathError.InvalidSegment]('src / "")
        intercept[PathError.InvalidSegment]('src / ".")
        intercept[PathError.InvalidSegment]('src / "..")
      }
      test("CannotRelativizeAbsAndRel"){
        val abs = pwd
        val rel = 'omg/'wtf
        compileError("""
          abs relativeTo rel
        """).check(
          """
          abs relativeTo rel
                         ^
          """,
          "type mismatch"
        )
        compileError("""
          rel relativeTo abs
                     """).check(
            """
          rel relativeTo abs
                         ^
            """,
            "type mismatch"
          )
      }
      test("InvalidCasts"){
        if(Unix()){
          intercept[IllegalArgumentException](Path("omg/cow"))
          intercept[IllegalArgumentException](RelPath("/omg/cow"))
        }
      }
    }
    test("Extractors"){
      test("regex"){
        val r"omg$x" = "omgasd"
        assert(x == "asd")
        val r"${y}omg" = "asdomg"
        assert(y == "asd")
        val r"omg${z}bbq" = "omgasdbbq"
        assert(z == "asd")
        val r"omg${a}b${b}bq" = "omgasdbbq"
        assert(a == "asd", b == "")
      }
      test("paths"){
        val a/b/c/d/"omg" = pwd/'A/'B/'C/'D/"omg"
        assert(
          a == pwd/'A,
          b == "B",
          c == "C",
          d == "D"
        )

        // If the paths aren't deep enough, it
        // just doesn't match but doesn't blow up
        root/'omg match {
          case a3/b3/c3/d3/e3 => assert(false)
          case _ =>
        }
      }
    }
    test("sorting"){
      assert(
        Seq(root/'c, root, root/'b, root/'a).sorted == Seq(root, root/'a, root/'b, root/'c),
        Seq(up/'c, up/up/'c, 'b/'c, 'a/'c, 'a/'d).sorted ==
          Seq('a/'c, 'a/'d, 'b/'c, up/'c, up/up/'c)
      )
    }
    test("construction"){
      test("success"){
        if(Unix()){
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"

          assert(
            RelPath(relStr) == 'hello/'cow,
            // Path(...) also allows paths starting with ~,
            // which is expanded to become your home directory
            Path(absStr) == root/'hello/'world
          )

          // You can also pass in java.io.File and java.nio.file.Path
          // objects instead of Strings when constructing paths
          val relIoFile = new java.io.File(relStr)
          val absNioFile = java.nio.file.Paths.get(absStr)

          assert(
            RelPath(relIoFile) == 'hello/'cow,
            Path(absNioFile) == root/'hello/'world,
            Path(relIoFile, root/'base) == root/'base/'hello/'cow
          )
        }
      }
      test("basepath"){
        if(Unix()){
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"
          assert(
            FilePath(relStr) == 'hello/'cow,
            FilePath(absStr) == root/'hello/'world
          )
        }
      }
      test("based"){
        if(Unix()){
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"
          val basePath: FilePath = FilePath(relStr)
          assert(
            Path(relStr, root/'base) == root/'base/'hello/'cow,
            Path(absStr, root/'base) == root/'hello/'world,
            Path(basePath, root/'base) == root/'base/'hello/'cow,
            Path(".", pwd).last != ""
          )
        }
      }
      test("failure"){
        if(Unix()){
          val relStr = "hello/.."
          intercept[java.lang.IllegalArgumentException]{
            Path(relStr)
          }

          val absStr = "/hello"
          intercept[java.lang.IllegalArgumentException]{
            RelPath(absStr)
          }

          val tooManyUpsStr = "/hello/../.."
          intercept[PathError.AbsolutePathOutsideRoot.type]{
            Path(tooManyUpsStr)
          }
        }
      }
      test("symlinks"){

        val names = Seq('test123, 'test124, 'test125, 'test126)
        val twd = tmp.dir()

        test("nestedSymlinks"){
          if(Unix()) {
            names.foreach(p => rm ! twd/p)
            mkdir ! twd/'test123
            ln.s(twd/'test124, twd/'test123)
            ln.s(twd/'test125, twd/'test124)
            ln.s(twd/'test126, twd/'test125)
            assert(os.followLink(twd/'test126).get == os.followLink(twd/'test123).get)
            names.foreach(p => rm ! twd/p)
            names.foreach(p => assert(!exists(twd/p)))
          }
        }

        test("danglingSymlink"){
          if(Unix()) {
            names.foreach(p => rm ! twd/p)
            mkdir ! twd/'test123
            ln.s(twd/'test124, twd/'test123)
            ln.s(twd/'test125, twd/'test124)
            ln.s(twd/'test126, twd/'test125)
            rm ! twd / 'test123
            assert( os.followLink(twd / 'test126).isEmpty)
            names.foreach(p => rm ! twd / p)
            names.foreach(p => assert(!exists(twd / p)))
            names.foreach(p => rm ! twd/p)
            names.foreach(p => assert(!exists(twd/p)))
          }
        }
      }
    }
  }
}
