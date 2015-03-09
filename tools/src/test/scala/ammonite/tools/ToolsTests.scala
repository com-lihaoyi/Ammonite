package test.ammonite

import ammonite.ops._

import utest._

import scala.collection.generic.{SeqFactory, GenericTraversableTemplate}
import scala.collection.{TraversableLike, mutable}

object ToolsTests extends TestSuite{

  val tests = TestSuite {
    var wd = cwd


    'grep{
      import ammonite.pprint.Config.Defaults._
      val items = Seq(123, 456, 789)
      items |? grep! "45"
      items |? grep! "^[123456]+$".r
      assert(
        (items |? grep! "45") == Seq(456),
        (items |? grep! "45".r) == Seq(456),
        (items |? grep! "[123456]+".r) == Seq(123, 456),
        (items |? grep! "^[123456]+$".r) == Seq(123, 456),
        (items |? grep! "[123456]".r) == Seq(123, 456),
        (items |? grep! "^[123456]$".r) == Seq()
      )
    }
  }
}
