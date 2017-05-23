package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object HeightTests extends TestSuite{
  val tests = TestSuite{

    'a - {
      val height = LineReader.calculateHeight0(
        LineReader.splitBuffer("abcde".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //ab
      //cd
      //e
    }
    'b - {
      val height = LineReader.calculateHeight0(
        LineReader.splitBuffer("abcd".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //ab
      //cd
      //|
    }
    'c - {
      val height = LineReader.calculateHeight0(
        LineReader.splitBuffer("abcd".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //|b
      //cd
      //_
    }

    'd - {
      val height = LineReader.calculateHeight0(
        LineReader.splitBuffer("ab\ncd".toVector),
        width = 2
      )
      assert(height == Seq(2, 2))
      //|b
      //_
      //cd
      //_
    }

    'e - {
      val height = LineReader.calculateHeight0(
        LineReader.splitBuffer("ab\ncd".toVector),
        width = 2
      )
      assert(height == Seq(2, 2))
      //ab
      //_
      //cd
      //|
    }
    'f - {
      val height = LineReader.calculateHeight0(
        LineReader.splitBuffer("ab\ncd".toVector),
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
