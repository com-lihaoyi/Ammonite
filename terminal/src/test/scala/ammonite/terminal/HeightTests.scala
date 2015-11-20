package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object HeightTests extends TestSuite{
  val tests = TestSuite{

    'a - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("abcde".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //ab
      //cd
      //e
    }
    'b - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("abcd".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //ab
      //cd
      //|
    }
    'c - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("abcd".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //|b
      //cd
      //_
    }

    'd - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("ab\ncd".toVector),
        width = 2
      )
      assert(height == Seq(2, 2))
      //|b
      //_
      //cd
      //_
    }

    'e - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("ab\ncd".toVector),
        width = 2
      )
      assert(height == Seq(2, 2))
      //ab
      //_
      //cd
      //|
    }
    'f - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("ab\ncd".toVector),
        width = 2
      )
      assert(height == Seq(2, 2))
      //ab
      //|
      //cd
      //_
    }
   


  }
}
