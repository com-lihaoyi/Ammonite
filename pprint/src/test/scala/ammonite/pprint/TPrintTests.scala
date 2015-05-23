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
      check[Int]("Int")
      def t[T] = check[T]("T")
      t
    }

    'singleton{
      check[x.type]("x.type")
      check[TPrintTests.this.M]("M")
      check[TPrintTests.type]("TPrintTests.type")
    }

    'java {
      check[java.util.Set[_ <: String]]("util.Set[_$1]")
      check[java.util.Set[String]]("util.Set[String]")
    }

    'compound{
      check[Map[Int, List[String]]]("Map[Int, List[String]]")
      check[Int => String]("Function1[Int, String]")
      check[Int {val x: Int}]("Int{val x: Int}")
      check[Int with String]("Int with String")
    }
    'existential{
//      check[Map[_, _]]("Map[_$2, _$3]")
//      check[Map[K, V] forSome {type K <: Int; type V <: String}]("Map[_$2, _$3]")
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
      assert(TPrint.default[M@deprecated].value == "M @deprecated")
    }


    'custom{
      class Custom
      object Custom{
        implicit def customTPrint: TPrint[Custom] = new TPrint("+++Custom+++")
      }
      check[Custom]("+++Custom+++")
      check[List[Custom]]("+++Custom+++")
    }
  }

}

