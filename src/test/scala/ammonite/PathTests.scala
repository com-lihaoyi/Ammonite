package ammonite

import ammonite.BasePath.NoRelativePathException
import utest._
import RelPath._
import Path._
object PathTests extends TestSuite{
  val tests = TestSuite {
    'Basic{
      val rel = 'src/'main/'scala

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
            'ArrayString - check(rel / Array("sub1", "sub2"))
            'ArraySymbol - check(rel / Array('sub1, 'sub2))
            'SeqString - check(rel / Seq("sub1", "sub2"))
            'SeqSymbol - check(rel / Seq('sub1, 'sub2))
            'SeqSeqSeqSymbol - check(
              rel / Seq(Seq(Seq('sub1), Seq()), Seq(Seq('sub2)), Seq())
            )
          }
        }
      }
      'AbsPath{
        val wd = processWorkingDir
        val abs = wd / rel
        'Constructor {
          assert(
            abs.toString.drop(wd.toString.length) == "/src/main/scala",
            abs.toString.length > wd.toString.length
          )
        }
        'Relativize{
          def eq[T](p: T, q: T) = assert(p == q)
          * - eq('omg/'bbq/'wtf - 'omg/'bbq/'wtf, empty)
          * - eq('omg/'bbq - 'omg/'bbq/'wtf, up)
          * - eq('omg/'bbq/'wtf - 'omg/'bbq, empty/'wtf)
          * - eq('omg/'bbq - 'omg/'bbq/'wtf, up)
          * - eq(up/'omg/'bbq - 'omg/'bbq, up/up/up/'omg/'bbq)
          * - intercept[NoRelativePathException]('omg/'bbq - up/'omg/'bbq)
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
          var abs = processWorkingDir
          var i = abs.segments.length
          while(i > 0){
            abs/=up
            i-=1
            assert(abs.segments.length == i)
          }
          intercept[BasePath.AbsolutePathOutsideRoot.type]{ abs/up }
        }
        'RootUpBreak{
          intercept[BasePath.AbsolutePathOutsideRoot.type]{ root/up }
          val x = root/"omg"
          val y = x/up
          intercept[BasePath.AbsolutePathOutsideRoot.type]{ y / up }

        }
      }
    }
    'Errors{
      'InvalidChars {
        val ex = intercept[BasePath.InvalidCharException]('src / "Main/.scala")
        val BasePath.InvalidCharException("Main/.scala", chars) = ex
        assert(chars == Set('/'))
      }
      'InvalidSegments{
        intercept[BasePath.InvalidSegmentException]{root/".."}
        intercept[BasePath.InvalidSegmentException]{root/"."}
      }
      'EmptySegment {
        intercept[BasePath.EmptySegmentException.type]('src / "")
      }
      'CannotRelativizeAbsAndRel{
        val abs = processWorkingDir
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
        val wd = processWorkingDir
        val a/b/c/d/"omg" = wd/'A/'B/'C/'D/"omg"
        assert(
          a == wd/'A,
          b == "B",
          c == "C",
          d == "D"
        )
      }
    }
  }
}
