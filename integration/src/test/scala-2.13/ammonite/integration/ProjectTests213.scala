package ammonite.integration

import ammonite.integration.TestUtils._
import ammonite.ops._
import utest._

object ProjectTests213 extends TestSuite{

  val tests = Tests {
    println("Running ProjectTest213")

    test("httpApi"){
      test("addPost"){
        val res = exec('basic / "HttpApi.sc", "addPost", "title", "some text")
        assert(res.out.trim.contains("101"))
      }
      test("comments"){
        val res = exec('basic / "HttpApi.sc", "comments", "40")
        assert(res.out.trim.contains("totam vel saepe aut"))
        assert(res.out.trim.contains("aperiam et omnis totam"))
      }
    }
  }
}
