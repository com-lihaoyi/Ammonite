package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object HeightTests extends TestSuite{
  val tests = TestSuite{

    'a - {
      val height = Terminal.calculateHeight0(
        Terminal.splitBuffer("abcde".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //ab
      //cd
      //e
    }
    'b - {
      val height = Terminal.calculateHeight0(
        Terminal.splitBuffer("abcd".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //ab
      //cd
      //|
    }
    'c - {
      val height = Terminal.calculateHeight0(
        Terminal.splitBuffer("abcd".toVector),
        width = 2
      )
      assert(height == Seq(3))
      //|b
      //cd
      //_
    }

    'd - {
      val height = Terminal.calculateHeight0(
        Terminal.splitBuffer("ab\ncd".toVector),
        width = 2
      )
      assert(height == Seq(2, 2))
      //|b
      //_
      //cd
      //_
    }

    'e - {
      val height = Terminal.calculateHeight0(
        Terminal.splitBuffer("ab\ncd".toVector),
        width = 2
      )
      assert(height == Seq(2, 2))
      //ab
      //_
      //cd
      //|
    }
    'f - {
      val height = Terminal.calculateHeight0(
        Terminal.splitBuffer("ab\ncd".toVector),
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
