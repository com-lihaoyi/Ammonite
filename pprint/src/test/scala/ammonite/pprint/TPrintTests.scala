package test.ammonite.pprint

import utest._
import ammonite.pprint.TPrint

object TPrintTests extends TestSuite{

  class M
  def check[T](expected: String)(implicit tprint: TPrint[T]) = {
    val tprinted = tprint.value
    assert(tprinted == expected)
  }
  val tests = TestSuite{

    type X = scala.Int with scala.Predef.String{}
    val x = ""

    'simple {
      check[X]("X")
      check[String]("String")
      check[java.lang.String]("String")
      check[Int]("Int")
      check[scala.Int]("Int")
      def t[T] = check[T]("T")
      t
    }

    'singleton{
      check[x.type]("x.type")
      check[TPrintTests.this.M]("M")
      check[TPrintTests.type]("TPrintTests.type")
    }

    'java {
      check[java.util.Set[_]]("util.Set[_$1]")
      check[java.util.Set[_ <: String]]("util.Set[_$2] forSome { type _$2 <: String }")
      check[java.util.Set[String]]("util.Set[String]")
    }

    'mutable{
      import collection.mutable
      check[mutable.Buffer[Int]]("mutable.Buffer[Int]")
      check[Seq[Int]]("Seq[Int]")
      check[collection.Seq[Int]]("Seq[Int]")

    }
    'compound{
      check[Map[Int, List[String]]]("Map[Int, List[String]]")
      check[Int => String]("Function1[Int, String]")
      check[Int {val x: Int}]("Int{val x: Int}")
      check[Int with String]("Int with String")
    }
    'existential{
      check[{type T = Int}]("{type T = Int}")

      check[Map[_, _]]("Map[_$3, _$4]")
      check[Map[K, Int] forSome { type K }](
        "Map[K, Int] forSome { type K }"
      )
      check[Map[K, Int] forSome { type K <: Int }](
           "Map[K, Int] forSome { type K <: Int }"
      )
      check[Map[K, V] forSome { type K <: Int; type V >: String }](
           "Map[K, V] forSome { type K <: Int; type V >: String }"
      )
      check[Map[K, V] forSome { type K <: Int; val x: Float; type V >: String }](
           "Map[K, V] forSome { type K <: Int; val x: Float; type V >: String }"
      )
    }


    'typeMember{
      class C{ type V; class U}
      check[C#V]("C#V")
      check[C#U]("C#U")
      object O{
        class P
      }
      check[O.P]("O.P")
    }
    'thisType {
      class T {
        check[T.this.type]("T.this.type")
      }
      new T()
    }
    'annotated{
      // Can't use the normal implicit method, because of SI-8079
      assert(TPrint.default[M@deprecated].value == "M @deprecated")
    }

    class Custom
    object Custom{
      implicit def customTPrint: TPrint[Custom] = new TPrint("+++Custom+++")
    }
    'custom{

      check[Custom]("+++Custom+++")
      check[List[Custom]]("List[+++Custom+++]")
    }

    'complex{
      class A
      class B{
        class C
      }
      check[(A with B)#C]("(A with B)#C")
      check[({type T = Int})#T]("Int")
      check[(Custom with B)#C]("(+++Custom+++ with B)#C")

    }
  }

}

