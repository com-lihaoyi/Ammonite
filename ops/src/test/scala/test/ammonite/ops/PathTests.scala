package test.ammonite.ops

import java.nio.file.Paths

import ammonite.ops._

import utest._
object PathTests extends TestSuite{
  val tests = TestSuite {
    'Basic{
      val rel = 'src/'main/'scala
      'Transformers{
        assert(
          // ammonite.Path to java.nio.file.Path
          (root/'omg).nio == Paths.get("/omg"),
          (empty/'omg).nio == Paths.get("omg"),

          // java.nio.file.Path to ammonite.Path
          root/'omg == Paths.get("/omg").amm,
          empty/'omg == Paths.get("omg").amm,

          // ammonite.Path to String
          (root/'omg).toString == "/omg",
          (empty/'omg).toString == "omg",

          // String to ammonite.Path
          root/'omg == Path("/omg"),
          empty/'omg == RelPath("omg")
        )
      }

      'RelPath{
        'Constructors {
          'Symbol {
            val rel1 = rel / 'ammonite
            assert(
              rel1.segments == Seq("src", "main", "scala", "ammonite"),
              rel1.toString == "src/main/scala/ammonite"
            )
          }
          'String {
            val rel1 = rel / "Path.scala"
            assert(
              rel1.segments == Seq("src", "main", "scala", "Path.scala"),
              rel1.toString == "src/main/scala/Path.scala"
            )
          }
          'Combos{
            def check(rel1: RelPath) = assert(
              rel1.segments == Seq("src", "main", "scala", "sub1", "sub2"),
              rel1.toString == "src/main/scala/sub1/sub2"
            )
            'ArrayString - {
              val arr = Array("sub1", "sub2")
              check(rel / arr)
            }
            'ArraySymbol - {
              val arr = Array('sub1, 'sub2)
              check(rel / arr)
            }
            'SeqString - check(rel / Seq("sub1", "sub2"))
            'SeqSymbol - check(rel / Seq('sub1, 'sub2))
            'SeqSeqSeqSymbol - check(
              rel / Seq(Seq(Seq('sub1), Seq()), Seq(Seq('sub2)), Seq())
            )
          }
        }
        'Relativize{
          def eq[T](p: T, q: T) = assert(p == q)
          * - eq('omg/'bbq/'wtf - 'omg/'bbq/'wtf, empty)
          * - eq('omg/'bbq - 'omg/'bbq/'wtf, up)
          * - eq('omg/'bbq/'wtf - 'omg/'bbq, empty/'wtf)
          * - eq('omg/'bbq - 'omg/'bbq/'wtf, up)
          * - eq(up/'omg/'bbq - 'omg/'bbq, up/up/up/'omg/'bbq)
          * - intercept[PathError.NoRelativePath]('omg/'bbq - up/'omg/'bbq)
        }
      }
      'AbsPath{
        val d = cwd
        val abs = d / rel
        'Constructor {
          assert(
            abs.toString.drop(d.toString.length) == "/src/main/scala",
            abs.toString.length > d.toString.length
          )
        }
        'Relativize{
          def eq[T](p: T, q: T) = assert(p == q)
          * - eq(root/'omg/'bbq/'wtf - root/'omg/'bbq/'wtf, empty)
          * - eq(root/'omg/'bbq - root/'omg/'bbq/'wtf, up)
          * - eq(root/'omg/'bbq/'wtf - root/'omg/'bbq, empty/'wtf)
          * - eq(root/'omg/'bbq - root/'omg/'bbq/'wtf, up)
          * - intercept[PathError.NoRelativePath]('omg/'bbq - up/'omg/'bbq)
        }
      }
      'Ups{
        'RelativeUps{
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
        'AbsoluteUps{
          // Keep applying `up` and verify that the path gets
          // shorter and shorter and eventually errors.
          var abs = cwd
          var i = abs.segments.length
          while(i > 0){
            abs/=up
            i-=1
            assert(abs.segments.length == i)
          }
          intercept[PathError.AbsolutePathOutsideRoot.type]{ abs/up }
        }
        'RootUpBreak{
          intercept[PathError.AbsolutePathOutsideRoot.type]{ root/up }
          val x = root/"omg"
          val y = x/up
          intercept[PathError.AbsolutePathOutsideRoot.type]{ y / up }
        }
      }
      'Comparison{
        'Relative - assert(
          'omg/'wtf == 'omg/'wtf,
          'omg/'wtf != 'omg/'wtf/'bbq,
          'omg/'wtf/'bbq startsWith 'omg/'wtf,
          'omg/'wtf startsWith 'omg/'wtf,
          up/'omg/'wtf startsWith up/'omg/'wtf,
          !('omg/'wtf startsWith 'omg/'wtf/'bbq),
          !(up/'omg/'wtf startsWith 'omg/'wtf),
          !('omg/'wtf startsWith up/'omg/'wtf)
        )
        'Absolute - assert(
          root/'omg/'wtf == root/'omg/'wtf,
          root/'omg/'wtf != root/'omg/'wtf/'bbq,
          root/'omg/'wtf/'bbq startsWith root/'omg/'wtf,
          root/'omg/'wtf startsWith root/'omg/'wtf,
          !(root/'omg/'wtf startsWith root/'omg/'wtf/'bbq)
        )
        'Invalid{
          compileError("""root/'omg/'wtf < 'omg/'wtf""")
          compileError("""root/'omg/'wtf > 'omg/'wtf""")
          compileError("""'omg/'wtf < root/'omg/'wtf""")
          compileError("""'omg/'wtf > root/'omg/'wtf""")
        }
      }
    }
    'Errors{
      'InvalidChars {
        val ex = intercept[PathError.InvalidSegment]('src / "Main/.scala")
        val PathError.InvalidSegment("Main/.scala") = ex
      }
      'InvalidSegments{
        intercept[PathError.InvalidSegment]{root/ "core/src/test"}
        intercept[PathError.InvalidSegment]{root/ ""}
      }
      'EmptySegment {
        intercept[PathError.InvalidSegment]('src / "")
      }
      'CannotRelativizeAbsAndRel{
        val abs = cwd
        val rel = 'omg/'wtf
        compileError("""
          abs - rel
        """).check(
          """
          abs - rel
                ^
          """,
          "type mismatch"
        )
        compileError("""
          rel - abs
                     """).check(
            """
          rel - abs
                ^
            """,
            "type mismatch"
          )
      }
      'InvalidCasts{
        intercept[IllegalArgumentException](Path("omg/cow"))
        intercept[IllegalArgumentException](RelPath("/omg/cow"))
      }
      'Pollution{
        // Make sure we're not polluting too much
        compileError("""'omg.ext""")
        compileError(""" "omg".ext """)
      }
    }
    'Extractors{
      'regex{
        val r"omg$x" = "omgasd"
        assert(x == "asd")
        val r"${y}omg" = "asdomg"
        assert(y == "asd")
        val r"omg${z}bbq" = "omgasdbbq"
        assert(z == "asd")
        val r"omg${a}b${b}bq" = "omgasdbbq"
        assert(a == "asd", b == "")
      }
      'paths{
        val a/b/c/d/"omg" = cwd/'A/'B/'C/'D/"omg"
        assert(
          a == cwd/'A,
          b == "B",
          c == "C",
          d == "D"
        )


        val relative = 'omg/'wtf/'bbq
        val a2/b2/c2 = relative
        assert(
          a2 == empty/'omg,
          b2 == "wtf",
          c2 == "bbq"
        )

        // If the paths aren't deep enough, it
        // just doesn't match but doesn't blow up
        root/'omg match {
          case a3/b3/c3/d3/e3 => assert(false)
          case _ =>
        }

        relative match {
          case a3/b3/c3/d3/e3 => assert(false)
          case _ =>
        }

      }
    }
    'sorting{
      assert(
        Seq(root/'c, root, root/'b, root/'a).sorted == Seq(root, root/'a, root/'b, root/'c),
        Seq(up/'c, up/up/'c, 'b/'c, 'a/'c, 'a/'d).sorted ==
          Seq('a/'c, 'a/'d, 'b/'c, up/'c, up/up/'c)
      )
    }
  }
}
