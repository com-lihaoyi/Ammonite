package ammonite.unit


import ammonite.repl.tools.Location
import utest._
import ammonite.repl.tools.source.load
import ammonite.util.Util
//import fastparse.utils.{ElemSetHelper, Generator, IndexedParserInput}

object SourceTests212 extends TestSuite{
  val tests = Tests{

    def check(loaded: Location, expectedFileName: String, expected: String, slop: Int = 10) = {


      val loadedFileName = loaded.fileName
      assert(loadedFileName == expectedFileName)
      // The line number from first bytecode of earliest concrete method
      // may be inexact, but it should put you *somewhere* near what you
      // are looking for
      val nearby = Predef.augmentString(loaded.fileContent).lines.slice(
        loaded.lineNum - slop,
        loaded.lineNum + slop
      ).mkString(Util.newLine)
      assert(nearby.contains(expected))
    }


    test("objectInfo"){
      test("fieldsAreTreatedAsObjects"){
        // Can't use Java Std Lib methods because SBT screws up classloaders in test suite
        check(
          load(com.github.javaparser.JavaToken.INVALID),
          "JavaToken.java",
          "public class JavaToken"
        )
      }

    }
    test("objectMemberInfo"){
      test("implementedBySuperclass"){
        // The file has changed names since earlier versions...

        val list: List[Int] = List(1, 2, 3)
        check(
          load(list.toString),
          "SeqLike.scala",
          "override def toString = super[IterableLike].toString"
        )
      }

    }
    test("staticMethod"){
      // Can't use Java Std Lib methods because SBT screws up classloaders in test suite
      check(
        load(com.github.javaparser.JavaParser.parseBlock _),
        "JavaParser.java",
        "public static BlockStmt parseBlock"
      )
    }

    test("fuzz"){
      // Feed a bunch of arbitrary classes and methods from a variety of places
      // through our location-finder logic to try and find edge cases where
      // things misbehave or blow up


      /**
        * We only bother to run these "fuzz"-style tests under Scala 2.12,
        * because they're too fragile w.r.t. traits/methods moving around
        * between major versions of Scala. Nevertheless, this should give us a
        * reasonable amount of confidence that the functionality works across
        * a range of inputs, and the earlier unit tests should give us some
        * confidence it works across a range of Scala versions
        */

      test("List"){


        test("head") - check(load(List().head), "IterableLike.scala", "def head")
        test("apply") - check(load(List().apply _), "LinearSeqOptimized.scala", "def apply")
        test("take") - check(load(List().take _), "List.scala", "override def take")
        test("drop") - check(load(List().drop _), "List.scala", "override def drop")
        test("slice") - check(load(List().slice _), "List.scala", "override def slice")
        test("iterator") - check(load(List().iterator _), "LinearSeqLike.scala", "def iterator")
        test("hashCode") - check(
          load(List().hashCode _),
          "LinearSeqLike.scala",
          "override def hashCode"
        )
        test("reverse") - check(load(List().reverse _), "List.scala", "def reverse")
        test("isEmpty") - check(load(List().isEmpty _), "SeqLike.scala", "def isEmpty")
        test("nonEmpty") - check(load(List().nonEmpty _), "TraversableOnce.scala", "def nonEmpty")
        test("orElse") - check(load(List().orElse _), "PartialFunction.scala", "def orElse")
        test("mkString") - check(load(List().mkString _), "TraversableOnce.scala", "def mkString")
        test("aggregate") - check(
          load(List().aggregate _),
          "TraversableOnce.scala", "def aggregate"
        )

        //    These result in a divering implicit expansion, even in plain Scala
  //      test("min") - check(load(List().min _), "TraversableOnce.scala", "def min")
  //      test("max") - check(load(List().max _), "TraversableOnce.scala", "def max")

        test("groupBy") - check(load(List().groupBy _), "TraversableLike.scala", "def groupBy")
        test("compose") - check(load(List().compose _), "Function1.scala", "def compose")

        test("prefixLength") - check(
          load(List().prefixLength _),
          "GenSeqLike.scala",
          "def prefixLength"
        )

        test("hasDefiniteSize") - check(
          load(List().hasDefiniteSize _),
          "TraversableLike.scala",
          "def hasDefiniteSize"
        )

        test("productIterator") - check(
          load(List().productIterator _),
          "Product.scala",
          "def productIterator"
        )
      }
      test("scalaz"){

        test("base") - check(load(scalaz.==>>), "Map.scala", "object ==>>")
        // Some aliases the make our code shorter
        import scalaz.==>>
        implicit val scalazOrder: scalaz.Order[Int] = scalaz.Order.fromScalaOrdering(Ordering[Int])
        implicit val scalazShow: scalaz.Show[Int] = scalaz.Show.show[Int](_.toString)
        type I = Int
        test("empty") - check(load(==>>.empty[I, I] _), "Map.scala", "final def empty")
        test("singleton") - check(load(==>>.singleton[I, I] _), "Map.scala", "final def singleton")
        test("unions") - check(load(==>>.unions[I, I] _), "Map.scala", "final def unions")
        // inherited
        test("mapShow") - check(load(==>>.mapShow[I, I]), "Map.scala", "implicit def mapShow")
        test("mapOrder") - check(load(==>>.mapOrder[I, I]), "Map.scala", "implicit def mapOrder")
        test("mapBifoldable") - check(
          load(==>>.mapBifoldable),
          "Map.scala",
          "implicit val mapBifoldable"
        )

        val instance = ==>>.empty[I, I]
        test("instance") - check(load(instance), "Map.scala", "case object Tip")
        test("adjust") - check(load(instance.adjust _), "Map.scala", "def adjust")
        test("values") - check(load(instance.values _), "Map.scala", "def values")
        test("mapAccumL") - check(load(instance.mapAccumL _), "Map.scala", "def mapAccumL")
        test("split") - check(load(instance.split _), "Map.scala", "def split")
      }
//      test("fastparse"){
//        import fastparse.all._
//        test("all") - check(load(fastparse.all),
      //        "StringApi.scala", "object all extends StringApi")
//        test("pApply") - check(load(P("hello")), "Api.scala", "def P")
//        test("pParse") - check(load(P("hello").parse _), "Parsing.scala", "def parse")
//        test("elemsWhileRaw") - check(load(ElemsWhile.raw _), "Api.scala", "def raw")
//        test("frameIndex") - check(
//          load(fastparse.core.Frame(1, null).index),
//          "Parsing.scala",
//          "case class Frame"
//        )
//        test("successValue") - check(
//          load(fastparse.all.Parsed.Success(0, 0).value),
//          "Parsing.scala",
//          "case class Success"
//        )
//        test("bitSet") - check(
//          load(new fastparse.utils.Utils.BitSet[Char](Array[Int](), 0, 0).apply _),
//          "Utils.scala",
//          "def apply(c: Elem)"
//        )
//
//        test("elemPredApply") - check(
//          load(fastparse.parsers.Intrinsics.ElemPred.apply[Char, String] _),
//          "Intrinsics.scala",
//          "case class ElemPred"
//        )
//
//
//        test("parserInput") - check(
//          load(IndexedParserInput("")),
//          "ParserInput.scala",
//          "case class IndexedParserInput"
//        )
//        test("parserInputData") - check(
//          load(IndexedParserInput("").data),
//          "ParserInput.scala",
//          "case class IndexedParserInput"
//        )
//        test("implicitlyHelper") - check(
//          load(implicitly[ElemSetHelper[Char]]),
//          "Predef.scala",
//          "def implicitly"
//        )
//        test("implicitlyReprGenerate") - check(
//          // Type annotation necessary in 2.11 and below
//          load(implicitly[ElemSetHelper[Char]].generateValues(_: Generator.Callback[Char])),
//          "ElemSetHelper.scala",
//          "def generateValues"
//        )
//      }
    }
  }
}
