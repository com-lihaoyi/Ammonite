package ammonite.unit


import ammonite.TestUtils
import ammonite.ops._
import ammonite.repl.tools.Location
import utest._
import ammonite.repl.tools.source.load
import ammonite.util.Util
import fastparse.utils.{ElemSetHelper, Generator, IndexedParserInput}

import scala.tools.nsc.interpreter.InputStream
object SourceTests extends TestSuite{
  val tests = Tests{

    def check(loaded: Location, expectedFileName: String, expected: String, slop: Int = 10) = {


      val loadedFileName = loaded.fileName
      assert(loadedFileName == expectedFileName)
      // The line number from first bytecode of earliest concrete method
      // may be inexact, but it should put you *somewhere* near what you
      // are looking for
      val nearby = loaded.fileContent.lines.slice(
        loaded.lineNum - slop,
        loaded.lineNum + slop
      ).mkString(Util.newLine)
      assert(nearby.contains(expected))
    }
    def check212(loaded: => Location, expectedFileName: String, expected: String) = {
      if (TestUtils.scala2_12) check(loaded, expectedFileName, expected)
    }


    'objectInfo{
      'thirdPartyJava{
        check(
          load(new javassist.ClassPool()),
          "ClassPool.java",
          "public class ClassPool"
        )
      }
      'thirdPartyScala{
//        Not published for 2.10
//        check(
//          load(shapeless.::),
//          "hlists.scala",
//          "final case class ::"
//        )
        check(
          load(scopt.Read),
          "options.scala",
          "object Read"
        )
      }
      'stdLibScala{
        'direct - {
          // The Scala standard library classes for some reason don't get
          // properly included in the classpath in 2.10; it's unfortunate but
          // we'll just ignore it since the community has already moved on to
          // 2.11 and 2.12
          check(
            load(Nil),
            "List.scala",
            "case object Nil extends List[Nothing]"
          )
        }
        'runtimeTyped - {
          val empty: Seq[Int] = Seq()
          val nonEmpty: Seq[Int] = Seq(1)
          check(
            load(empty),
            "List.scala",
            "case object Nil extends List[Nothing]"
          )
          check(
            load(nonEmpty),
            "List.scala",
            "final case class ::"
          )
        }
      }
      'fieldsAreTreatedAsObjects{
        // Can't use Java Std Lib methods because SBT screws up classloaders in test suite
        check212(
          load(com.github.javaparser.JavaToken.INVALID),
          "JavaToken.java",
          "public class JavaToken"
        )
      }

    }
    'objectMemberInfo{
      'thirdPartyJava{
        val pool = new javassist.ClassPool()
        check(
          load(pool.find _),
          "ClassPool.java",
          "public URL find(String classname)"
        )

        check(
          load(new javassist.ClassPool().find _),
          "ClassPool.java",
          "public URL find(String classname)"
        )
      }
      'void{
        check(
          load(Predef.println()),
          "Predef.scala",
          "def println() ="
        )
      }

      'overloaded{
        val pool = new javassist.ClassPool()
        check(
          load(pool.makeClass(_: InputStream)),
          "ClassPool.java",
          "public CtClass makeClass(InputStream classfile)"
        )
        check(
          load(pool.makeClass(_: String)),
          "ClassPool.java",
          "public CtClass makeClass(String classname)"
        )
        check(
          load(pool.makeClass(_: javassist.bytecode.ClassFile, _: Boolean)),
          "ClassPool.java",
          "public CtClass makeClass(ClassFile classfile, boolean ifNotFrozen)"
        )
      }
      'implementedBySubclass{
        val opt: Option[Int] = Option(1)
        check(
          load(opt.get),
          "Option.scala",
          "def get = "
        )
      }
      'implementedBySuperclass{
        // The file has changed names since earlier versions...
        if (TestUtils.scala2_12){

          val list: List[Int] = List(1, 2, 3)
          check(
            load(list.toString),
            "SeqLike.scala",
            "override def toString = super[IterableLike].toString"
          )
        }
      }

    }
    'staticMethod{
      // Can't use Java Std Lib methods because SBT screws up classloaders in test suite
      check212(
        load(com.github.javaparser.JavaParser.parseBlock _),
        "JavaParser.java",
        "public static BlockStmt parseBlock"
      )
    }

    'fuzz{
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

      'List{


        'head     - check212(load(List().head), "IterableLike.scala", "def head")
        'apply    - check212(load(List().apply _), "LinearSeqOptimized.scala", "def apply")
        'take     - check212(load(List().take _), "List.scala", "override def take")
        'drop     - check212(load(List().drop _), "List.scala", "override def drop")
        'slice    - check212(load(List().slice _), "List.scala", "override def slice")
        'iterator - check212(load(List().iterator _), "LinearSeqLike.scala", "def iterator")
        'hashCode - check212(
          load(List().hashCode _),
          "LinearSeqLike.scala",
          "override def hashCode"
        )
        'reverse  - check212(load(List().reverse _), "List.scala", "def reverse")
        'isEmpty  - check212(load(List().isEmpty _), "SeqLike.scala", "def isEmpty")
        'nonEmpty - check212(load(List().nonEmpty _), "TraversableOnce.scala", "def nonEmpty")
        'orElse   - check212(load(List().orElse _), "PartialFunction.scala", "def orElse")
        'mkString - check212(load(List().mkString _), "TraversableOnce.scala", "def mkString")
        'aggregate- check212(load(List().aggregate _), "TraversableOnce.scala", "def aggregate")

        //    These result in a divering implicit expansion, even in plain Scala
  //      'min      - check212(load(List().min _), "TraversableOnce.scala", "def min")
  //      'max      - check212(load(List().max _), "TraversableOnce.scala", "def max")

        'groupBy  - check212(load(List().groupBy _), "TraversableLike.scala", "def groupBy")
        'compose  - check212(load(List().compose _), "Function1.scala", "def compose")

        'prefixLength - check212(
          load(List().prefixLength _),
          "GenSeqLike.scala",
          "def prefixLength"
        )

        'hasDefiniteSize - check212(
          load(List().hasDefiniteSize _),
          "TraversableLike.scala",
          "def hasDefiniteSize"
        )

        'productIterator - check212(
          load(List().productIterator _),
          "Product.scala",
          "def productIterator"
        )
      }
      'fastparse{
        import fastparse.all._
        'all - check212(load(fastparse.all), "StringApi.scala", "object all extends StringApi")
        'pApply - check212(load(P("hello")), "Api.scala", "def P")
        'pParse - check212(load(P("hello").parse _), "Parsing.scala", "def parse")
        'elemsWhileRaw - check212(load(ElemsWhile.raw _), "Api.scala", "def raw")
        'frameIndex - check212(
          load(fastparse.core.Frame(1, null).index),
          "Parsing.scala",
          "case class Frame"
        )
        'successValue - check212(
          load(fastparse.all.Parsed.Success(0, 0).value),
          "Parsing.scala",
          "case class Success"
        )
        'bitSet - check212(
          load(new fastparse.utils.Utils.BitSet[Char](Array[Int](), 0, 0).apply _),
          "Utils.scala",
          "def apply(c: Elem)"
        )

        'elemPredApply- check212(
          load(fastparse.parsers.Intrinsics.ElemPred.apply[Char, String] _),
          "Intrinsics.scala",
          "case class ElemPred"
        )


        'parserInput - check212(
          load(IndexedParserInput("")),
          "ParserInput.scala",
          "case class IndexedParserInput"
        )
        'parserInputData - check212(
          load(IndexedParserInput("").data),
          "ParserInput.scala",
          "case class IndexedParserInput"
        )
        'implicitlyHelper - check212(
          load(implicitly[ElemSetHelper[Char]]),
          "Predef.scala",
          "def implicitly"
        )
        'implicitlyReprGenerate - check212(
          // Type annotation necessary in 2.11 and below
          load(implicitly[ElemSetHelper[Char]].generateValues(_: Generator.Callback[Char])),
          "ElemSetHelper.scala",
          "def generateValues"
        )
      }
    }
  }
}
