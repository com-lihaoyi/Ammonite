package test.ammonite.ops

import java.nio.file.Paths

import ammonite.ops._

import utest._
object PathTests extends TestSuite {
  val tests = TestSuite {
    'Basic {
      val rel = 'src / 'main / 'scala
      'Transformers - {
        if (Unix()) {
          assert(
            // ammonite.Path to java.nio.file.Path
            (root / 'omg).toNIO == Paths.get("/omg"),
            // java.nio.file.Path to ammonite.Path
            root / 'omg == Path(Paths.get("/omg")),
            empty / 'omg == RelPath(Paths.get("omg")),
            // ammonite.Path to String
            (root / 'omg).toString == "/omg",
            (empty / 'omg).toString == "omg",
            // String to ammonite.Path
            root / 'omg == Path("/omg"),
            empty / 'omg == RelPath("omg")
          )
        }
      }

      'RelPath {
        'Constructors {
          'Symbol {
            if (Unix()) {
              val rel1 = rel / 'ammonite
              assert(
                rel1.segments == Seq("src", "main", "scala", "ammonite"),
                rel1.toString == "src/main/scala/ammonite"
              )
            }
          }
          'String {
            if (Unix()) {
              val rel1 = rel / "Path.scala"
              assert(
                rel1.segments == Seq("src", "main", "scala", "Path.scala"),
                rel1.toString == "src/main/scala/Path.scala"
              )
            }
          }
          'Combos {
            def check(rel1: RelPath) = assert(
              rel1.segments == Seq("src", "main", "scala", "sub1", "sub2"),
              rel1.toString == "src/main/scala/sub1/sub2"
            )
            'ArrayString - {
              if (Unix()) {
                val arr = Array("sub1", "sub2")
                check(rel / arr)
              }
            }
            'ArraySymbol - {
              if (Unix()) {
                val arr = Array('sub1, 'sub2)
                check(rel / arr)
              }
            }
            'SeqString - {
              if (Unix()) check(rel / Seq("sub1", "sub2"))
            }
            'SeqSymbol - {
              if (Unix()) check(rel / Seq('sub1, 'sub2))
            }
            'SeqSeqSeqSymbol - {
              if (Unix()) {
                check(
                  rel / Seq(Seq(Seq('sub1), Seq()), Seq(Seq('sub2)), Seq())
                )
              }
            }
          }
        }
        'Relativize {
          def eq[T](p: T, q: T) = assert(p == q)
          * - eq('omg / 'bbq / 'wtf relativeTo 'omg / 'bbq / 'wtf, empty)
          * - eq('omg / 'bbq relativeTo 'omg / 'bbq / 'wtf, up)
          * - eq('omg / 'bbq / 'wtf relativeTo 'omg / 'bbq, empty / 'wtf)
          * - eq('omg / 'bbq relativeTo 'omg / 'bbq / 'wtf, up)
          * - eq(up / 'omg / 'bbq relativeTo 'omg / 'bbq, up / up / up / 'omg / 'bbq)
          * - intercept[PathError.NoRelativePath]('omg / 'bbq relativeTo up / 'omg / 'bbq)
        }
      }
      'AbsPath {
        val d = pwd
        val abs = d / rel
        'Constructor {
          if (Unix())
            assert(
              abs.toString.drop(d.toString.length) == "/src/main/scala",
              abs.toString.length > d.toString.length
            )
        }
        'Relativize {
          def eq[T](p: T, q: T) = assert(p == q)
          * - eq(root / 'omg / 'bbq / 'wtf relativeTo root / 'omg / 'bbq / 'wtf, empty)
          * - eq(root / 'omg / 'bbq relativeTo root / 'omg / 'bbq / 'wtf, up)
          * - eq(root / 'omg / 'bbq / 'wtf relativeTo root / 'omg / 'bbq, empty / 'wtf)
          * - eq(root / 'omg / 'bbq relativeTo root / 'omg / 'bbq / 'wtf, up)
          * - intercept[PathError.NoRelativePath]('omg / 'bbq relativeTo up / 'omg / 'bbq)
        }
      }
      'Ups {
        'RelativeUps {
          val rel2 = rel / up
          assert(
            rel2 == 'src / 'main,
            rel / up / up == empty / 'src,
            rel / up / up / up == empty,
            rel / up / up / up / up == up,
            rel / up / up / up / up / up == up / up,
            up / rel == up / 'src / 'main / 'scala
          )
        }
        'AbsoluteUps {
          // Keep applying `up` and verify that the path gets
          // shorter and shorter and eventually errors.
          var abs = pwd
          var i = abs.segments.length
          while (i > 0) {
            abs /= up
            i -= 1
            assert(abs.segments.length == i)
          }
          intercept[PathError.AbsolutePathOutsideRoot.type] { abs / up }
        }
        'RootUpBreak {
          intercept[PathError.AbsolutePathOutsideRoot.type] { root / up }
          val x = root / "omg"
          val y = x / up
          intercept[PathError.AbsolutePathOutsideRoot.type] { y / up }
        }
      }
      'Comparison {
        'Relative - assert(
          'omg / 'wtf == 'omg / 'wtf,
          'omg / 'wtf != 'omg / 'wtf / 'bbq,
          'omg / 'wtf / 'bbq startsWith 'omg / 'wtf,
          'omg / 'wtf startsWith 'omg / 'wtf,
          up / 'omg / 'wtf startsWith up / 'omg / 'wtf,
          !('omg / 'wtf startsWith 'omg / 'wtf / 'bbq),
          !(up / 'omg / 'wtf startsWith 'omg / 'wtf),
          !('omg / 'wtf startsWith up / 'omg / 'wtf)
        )
        'Absolute - assert(
          root / 'omg / 'wtf == root / 'omg / 'wtf,
          root / 'omg / 'wtf != root / 'omg / 'wtf / 'bbq,
          root / 'omg / 'wtf / 'bbq startsWith root / 'omg / 'wtf,
          root / 'omg / 'wtf startsWith root / 'omg / 'wtf,
          !(root / 'omg / 'wtf startsWith root / 'omg / 'wtf / 'bbq)
        )
        'Invalid {
          compileError("""root/'omg/'wtf < 'omg/'wtf""")
          compileError("""root/'omg/'wtf > 'omg/'wtf""")
          compileError("""'omg/'wtf < root/'omg/'wtf""")
          compileError("""'omg/'wtf > root/'omg/'wtf""")
        }
      }
    }
    'Errors {
      'InvalidChars {
        val ex = intercept[PathError.InvalidSegment]('src / "Main/.scala")

        val PathError.InvalidSegment("Main/.scala", msg1) = ex

        assert(msg1.contains("[/] is not a valid character to appear in a path segment"))

        val ex2 =
          intercept[PathError.InvalidSegment](root / "hello" / ".." / "world")

        val PathError.InvalidSegment("..", msg2) = ex2

        assert(msg2.contains("use the `up` segment from `ammonite.ops.up`"))
      }
      'InvalidSegments {
        intercept[PathError.InvalidSegment] { root / "core/src/test" }
        intercept[PathError.InvalidSegment] { root / "" }
        intercept[PathError.InvalidSegment] { root / "." }
        intercept[PathError.InvalidSegment] { root / ".." }
      }
      'EmptySegment {
        intercept[PathError.InvalidSegment]('src / "")
        intercept[PathError.InvalidSegment]('src / ".")
        intercept[PathError.InvalidSegment]('src / "..")
      }
      'CannotRelativizeAbsAndRel {
        val abs = pwd
        val rel = 'omg / 'wtf
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
      'InvalidCasts {
        if (Unix()) {
          intercept[IllegalArgumentException](Path("omg/cow"))
          intercept[IllegalArgumentException](RelPath("/omg/cow"))
        }
      }
      'Pollution {
        // Make sure we're not polluting too much
        compileError("""'omg.ext""")
        compileError(""" "omg".ext """)
      }
    }
    'Extractors {
      'regex {
        val r"omg$x" = "omgasd"
        assert(x == "asd")
        val r"${ y }omg" = "asdomg"
        assert(y == "asd")
        val r"omg${ z }bbq" = "omgasdbbq"
        assert(z == "asd")
        val r"omg${ a }b${ b }bq" = "omgasdbbq"
        assert(a == "asd", b == "")
      }
      'paths {
        val a / b / c / d / "omg" = pwd / 'A / 'B / 'C / 'D / "omg"
        assert(
          a == pwd / 'A,
          b == "B",
          c == "C",
          d == "D"
        )

        val relative = 'omg / 'wtf / 'bbq
        val a2 / b2 / c2 = relative
        assert(
          a2 == empty / 'omg,
          b2 == "wtf",
          c2 == "bbq"
        )

        // If the paths aren't deep enough, it
        // just doesn't match but doesn't blow up
        root / 'omg match {
          case a3 / b3 / c3 / d3 / e3 => assert(false)
          case _ =>
        }

        relative match {
          case a3 / b3 / c3 / d3 / e3 => assert(false)
          case _ =>
        }

      }
    }
    'sorting {
      assert(
        Seq(root / 'c, root, root / 'b, root / 'a).sorted == Seq(root, root / 'a, root / 'b, root / 'c),
        Seq(up / 'c, up / up / 'c, 'b / 'c, 'a / 'c, 'a / 'd).sorted ==
          Seq('a / 'c, 'a / 'd, 'b / 'c, up / 'c, up / up / 'c)
      )
    }
    'construction {
      'success {
        if (Unix()) {
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"

          assert(
            RelPath(relStr) == 'hello / 'cow,
            Path(absStr) == root / 'hello / 'world
          )

          // You can also pass in java.io.File and java.nio.file.Path
          // objects instead of Strings when constructing paths
          val relIoFile = new java.io.File(relStr)
          val absNioFile = java.nio.file.Paths.get(absStr)

          assert(
            RelPath(relIoFile) == 'hello / 'cow,
            Path(absNioFile) == root / 'hello / 'world,
            Path(relIoFile, root / 'base) == root / 'base / 'hello / 'cow
          )
        }
      }
      'basepath {
        if (Unix()) {
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"
          assert(
            FilePath(relStr) == 'hello / 'cow,
            FilePath(absStr) == root / 'hello / 'world
          )
        }
      }
      'based {
        if (Unix()) {
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"
          val basePath: FilePath = FilePath(relStr)
          assert(
            Path(relStr, root / 'base) == root / 'base / 'hello / 'cow,
            Path(absStr, root / 'base) == root / 'hello / 'world,
            Path(basePath, root / 'base) == root / 'base / 'hello / 'cow
          )
        }
      }
      'failure {
        if (Unix()) {
          val relStr = "hello/.."
          intercept[java.lang.IllegalArgumentException] {
            Path(relStr)
          }

          val absStr = "/hello"
          intercept[java.lang.IllegalArgumentException] {
            RelPath(absStr)
          }

          val tooManyUpsStr = "/hello/../.."
          intercept[PathError.AbsolutePathOutsideRoot.type] {
            Path(tooManyUpsStr)
          }
        }
      }
    }
  }
}
