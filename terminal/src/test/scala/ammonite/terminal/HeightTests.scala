package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object HeightTests extends TestSuite{
  val tests = TestSuite{

    'a - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("abcde".toVector),
        cursor = 0,
        width = 2
      )
      assert(height == (3, 0, 0))
      //ab
      //cd
      //e
    }
    'b - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("abcd".toVector),
        cursor = 4,
        width = 2
      )
      assert(height == (3, 2, 0))
      //ab
      //cd
      //|
    }
    'c - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("abcd".toVector),
        cursor = 0,
        width = 2
      )
      assert(height == (3, 0, 0))
      //|b
      //cd
      //_
    }

    'd - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("ab\ncd".toVector),
        cursor = 0,
        width = 2
      )
      assert(height == (4, 0, 0))
      //|b
      //_
      //cd
      //_
    }

    'e - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("ab\ncd".toVector),
        cursor = 5,
        width = 2
      )
      assert(height == (4, 3, 0))
      //ab
      //_
      //cd
      //|
    }
    'f - {
      val height = TermCore.calculateHeight0(
        TermCore.splitBuffer("ab\ncd".toVector),
        cursor = 2,
        width = 2
      )
      assert(height == (4, 1, 0))
      //ab
      //|
      //cd
      //_
    }
   


  }
}
