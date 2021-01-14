package ammonite.unit


import utest._
import ammonite.compiler.tools.source.load
import ammonite.util.Util
import ammonite.util.Util.Location

import java.io.InputStream
object SourceTests extends TestSuite{
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
      test("thirdPartyJava"){
        check(
          load(new javassist.ClassPool()),
          "ClassPool.java",
          "public class ClassPool"
        )
      }
      test("thirdPartyScala"){
//        Not published for 2.10
//        check(
//          load(shapeless.::),
//          "hlists.scala",
//          "final case class ::"
//        )
//        check(
//          load(mainargs.TokensReader),
//          "TokensReader.scala",
//          "class TokensReader"
//        )
      }
      test("stdLibScala"){
        test("direct"){
          val is2_13_3_orLower = {
            val ver = scala.util.Properties.versionNumberString
            !ver.startsWith("2.13.") ||
              scala.util.Try(ver.stripPrefix("2.13.").toInt).toOption.exists(_ <= 3)
          }
          check(
            load(Nil),
            if (is2_13_3_orLower) "List.scala" else "package.scala",
            if (is2_13_3_orLower) "case object Nil extends List[Nothing]"
            else "val Nil = scala.collection.immutable.Nil"
          )
        }
        test("runtimeTyped"){
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

    }
    test("objectMemberInfo"){
      test("thirdPartyJava"){
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
      test("void"){
        check(
          load(Predef.println()),
          "Predef.scala",
          "def println()"
        )
      }

      test("overloaded"){
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
      test("implementedBySubclass"){
        val opt: Option[Int] = Option(1)
        check(
          load(opt.get),
          "Option.scala",
          "def get"
        )
      }
    }
  }
}
