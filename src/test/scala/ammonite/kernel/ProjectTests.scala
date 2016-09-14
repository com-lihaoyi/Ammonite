package ammonite.kernel

import org.scalatest.FreeSpec
import KernelTests._
import scalaz._

class ProjectTests extends FreeSpec {

  val kernel = buildKernel()

  def checkImportSuccess(groupId: String, artifactId: String, version: String): Unit =
    assert(kernel.loadIvy(groupId, artifactId, version).isSuccess)

  "scalatags" in {
    checkFailure(kernel,
                 Vector(
                   ("import scalatags.Text.all._", {
                     case NonEmptyList(h, tl) => tl.isEmpty && h.msg.contains("not found: value scalatags")
                   })
                 ))
    checkImportSuccess("com.lihaoyi", "scalatags_2.11", "0.5.4")
    checkSuccess(kernel,
                 Vector(
                   ("import scalatags.Text.all._", checkUnit),
                   ("""a("omg", href:="www.google.com").render""", {
                     case s: String => s.contains("""<a href="www.google.com">omg</a>""")
                     case _ => false
                   })
                 ))
  }

  "shapeless" in {
    checkImportSuccess("com.chuusai", "shapeless_2.11", "2.2.5")
    checkSuccess(kernel,
                 Vector(
                   ("import shapeless._", checkUnit),
                   ("""val a = (1 :: "lol" :: List(1, 2, 3) :: HNil)""", checkUnit),
                   ("a(0)", checkInt(1)),
                   ("a(1)", checkString("lol")),
                   ("import shapeless.syntax.singleton._", checkUnit),
                   ("2.narrow", checkInt(2))
                 ))
  }

  "guava" in {
    checkImportSuccess("com.google.guava", "guava", "18.0")
    checkSuccess(kernel,
                 Vector(
                   ("import com.google.common.collect._", checkUnit),
                   ("""val bimap = ImmutableBiMap.of(1, "one", 2, "two", 3, "three")""", checkUnit),
                   ("bimap.get(1)", checkString("one")),
                   ("""bimap.inverse.get("two")""", checkInt(2))
                 ))
  }

  "spire" in {
    checkImportSuccess("org.spire-math", "spire_2.11", "0.11.0")
    checkSuccess(kernel,
                 Vector(
                   ("import spire.implicits._", checkUnit),
                   ("import spire.math._", checkUnit),
                   ("""
          def euclidGcd[A: Integral](x: A, y: A): A = {
            if (y == 0) x
            else euclidGcd(y, x % y)
          }
          """,
                    checkUnit),
                   ("euclidGcd(42, 96)", checkInt(6)),
                   ("euclidGcd(42L, 96L)", checkLong(6L)),
                   ("def mean[A: Fractional](xs: A*): A = xs.reduceLeft(_ + _) / xs.size", checkUnit),
                   ("mean(0.5, 1.5, 0.0, -0.5)", checkDouble(0.375))
                 ))

  }

}
