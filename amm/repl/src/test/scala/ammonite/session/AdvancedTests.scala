package ammonite.session

import ammonite.TestUtils._
import ammonite.DualTestRepl
import ammonite.util.{Res, Util}
import utest._


object AdvancedTests extends TestSuite{
  val tests = Tests{
    println("AdvancedTests")
    val check = new DualTestRepl()
    'pprint{
      check.session(s"""
        @ Seq.fill(10)(Seq.fill(3)("Foo"))
        res0: Seq[Seq[String]] = List(
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo")
        )

        @ case class Foo(i: Int, s0: String, s1: Seq[String])
        defined class Foo

        @ Foo(1, "", Nil)
        res2: Foo = Foo(1, "", List())

        @ Foo(
        @   1234567,
        @   "I am a cow, hear me moo",
        @   Seq("I weigh twice as much as you", "and I look good on the barbecue")
        @ )
        res3: Foo = Foo(
          1234567,
          "I am a cow, hear me moo",
          List("I weigh twice as much as you", "and I look good on the barbecue")
        )
      """)
    }

    'exit{
      check.result("exit", Res.Exit())
    }
    'skip{
      check.result("", Res.Skip)
    }

    'predef{
      val check2 = new DualTestRepl{
        override def predef = (
          """
          import math.abs
          val x = 1
          val y = "2"
          """,
          None
        )
      }
      check2.session("""
        @ -x
        res0: Int = -1

        @ y
        res1: String = "2"

        @ x + y
        res2: String = "12"

        @ abs(-x)
        res3: Int = 1
      """)

    }
    'predefSettings{
      val check2 = new DualTestRepl{
        override def predef = (
          """
          interp.configureCompiler(_.settings.Xexperimental.value = true)
          """,
          None
        )
      }
      check2.session("""
        @ repl.compiler.settings.Xexperimental.value
        res0: Boolean = true
      """)

    }
    'macros{
      check.session("""
        @ import language.experimental.macros

        @ import reflect.macros.Context

        @ object Macro {
        @   def impl(c: Context): c.Expr[String] = {
        @    import c.universe._
        @    c.Expr[String](Literal(Constant("Hello!")))
        @   }
        @ }
        defined object Macro

        @ def m: String = macro Macro.impl
        defined function m

        @ m
        res4: String = "Hello!"
      """)
    }
    'typeScope{
      // Fancy type-printing isn't implemented at all in 2.10.x
      check.session("""
        @ collection.mutable.Buffer(1)
        res0: collection.mutable.Buffer[Int] = ArrayBuffer(1)

        @ import collection.mutable

        @ collection.mutable.Buffer(1)
        res2: mutable.Buffer[Int] = ArrayBuffer(1)

        @ mutable.Buffer(1)
        res3: mutable.Buffer[Int] = ArrayBuffer(1)

        @ import collection.mutable.Buffer

        @ mutable.Buffer(1)
        res5: Buffer[Int] = ArrayBuffer(1)
      """)
    }
    'customTypePrinter{
      check.session("""
        @ Array(1)
        res0: Array[Int] = Array(1)

        @ import pprint.TPrint

        @ implicit def ArrayTPrint[T: TPrint]: TPrint[Array[T]] = TPrint.lambda( c =>
        @   implicitly[TPrint[T]].render(c) +
        @   " " +
        @   c.typeColor("Array").render
        @ )

        @ Array(1)
        res3: Int Array = Array(1)
      """)
    }
    'trappedType{
      check.session("""
        @ val nope = ammonite.TestRepl.Nope(2); val n = 2
        n: Int = 2
      """)
    }
    'unwrapping{
      check.session("""
        @ {
        @   val x = 1
        @   val y = 2
        @   x + y
        @ }
        x: Int = 1
        y: Int = 2
        res0_2: Int = 3
      """)
    }
    'forceWrapping{
      check.session("""
        @ {{
        @   val x = 1
        @   val y = 2
        @   x + y
        @ }}
        res0: Int = 3
      """)
    }
    'truncation {
      // Need a way to capture stdout in tests to make these tests work
      if(false) check.session("""
        @ Seq.fill(20)(100)
        res0: Seq[Int] = List(
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
        ...

        @ show(Seq.fill(20)(100))
        res1: ammonite.pprint.Show[Seq[Int]] = List(
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100,
          100
        )

        @ show(Seq.fill(20)(100), height = 3)
        res2: ammonite.pprint.Show[Seq[Int]] = List(
          100,
          100,
        ...

        @ repl.pprinter() = repl.pprinter().copy(defaultHeight = 5)

        @ Seq.fill(20)(100)
        res4: Seq[Int] = List(
          100,
          100,
          100,
          100,
        ...
      """)
    }
    'private{
      check.session("""
        @ private val x = 1; val y = x + 1
        x: Int = 1
        y: Int = 2

        @ y
        res1: Int = 2

        @ x
        error: not found: value x

        @ {
        @ private[this] val a = 3
        @ val b = a * 4
        @ }

        @ a
        error: not found: value a

        @ b
        
      """)
    }
    'compilerPlugin - retry(3){
      if (!scala2_12) check.session("""
        @ // Compiler plugins imported without `.$plugin` are not loaded

        @ import $ivy.`org.spire-math::kind-projector:0.6.3`

        @ trait TC0[F[_]]
        defined trait TC0

        @ type TC0EitherStr = TC0[Either[String, ?]]
        error: not found: type ?

        @ // You need to use `import $ivy.$plugin`

        @ import $plugin.$ivy.`org.spire-math::kind-projector:0.6.3`

        @ trait TC[F[_]]
        defined trait TC

        @ type TCEitherStr = TC[Either[String, ?]]
        defined type TCEitherStr

        @ // Importing plugins doesn't affect the run-time classpath

        @ import $plugin.$ivy.`com.lihaoyi::scalatags:0.6.2`

        @ import scalatags.Text
        error: not found: value scalatags
      """)
    }
    'replApiUniqueness{
      // Make sure we can instantiate multiple copies of Interpreter, with each
      // one getting its own `ReplBridge`. This ensures that the various
      // Interpreters are properly encapsulated and don't interfere with each
      // other.
      val c1 = new DualTestRepl()
      val c2 = new DualTestRepl()
      c1.session("""
        @ repl.prompt() = "A"
      """)
      c2.session("""
        @ repl.prompt() = "B"
      """)
      c1.session("""
        @ assert(repl.prompt() == "A")
      """)
      c2.session("""
        @ assert(repl.prompt() == "B")
      """)
    }
    'macroParadiseWorks{
      val scalaVersion: String = scala.util.Properties.versionNumberString
      val c1: DualTestRepl = new DualTestRepl()
      c1.session(s"""
        @ interp.load.plugin.ivy("org.scalamacros" % "paradise_${scalaVersion}" % "2.1.0")
      """)
      c1.session("""
        @ val x = 1
      """)
    }
    'desugar{
      check.session("""
        @ desugar{1 + 2 max 3}
        res0: Desugared = scala.Predef.intWrapper(3).max(3)
      """)
    }
    'loadingModulesInPredef{

      val dir = os.pwd/'amm/'src/'test/'resources/'scripts/'predefWithLoad
      'loadExec {
        val c1 = new DualTestRepl() {
          override def predef = (
            os.read(dir/"PredefLoadExec.sc"),
            Some(dir/"PredefLoadExec.sc")
          )
        }
        c1.session("""
          @ val previouslyLoaded = predefDefinedValue
          previouslyLoaded: Int = 1337
        """)
      }
      'loadModule{
        val c2 = new DualTestRepl(){
          override def predef = (
            os.read(dir/"PredefLoadModule.sc"),
            Some(dir/"PredefLoadModule.sc")
          )
        }
        c2.session("""
          @ val previouslyLoaded = predefDefinedValue
          previouslyLoaded: Int = 1337
        """)
      }
      'importIvy{
        val c2 = new DualTestRepl(){
          override def predef = (
            os.read(dir/"PredefMagicImport.sc"),
            Some(dir/"PredefMagicImport.sc")
          )
        }
        c2.session("""
          @ val previouslyLoaded = predefDefinedValue
          previouslyLoaded: Int = 1337

          @ val loadedDirect = Loaded.loadedDefinedValue
          loadedDirect: Int = 1337
        """)
      }
    }
    'bytecodeForReplClasses{
      check.session("""
        @ case class Child(name: String)

        @ val cls = classOf[Child]

        @ val resName = cls.getName.replace('.', '/') + ".class"

        @ cls.getClassLoader.getResource(resName) != null
        res3: Boolean = true

        @ cls.getClassLoader.getResourceAsStream(resName) != null
        res4: Boolean = true
      """)
    }
    'customBridge{
      check.session("""
        @ val s = test.message
        s: String = "ba"
      """)
    }

    'dontRefreshCompiler {
      * - {
        check.session("""
          @ val c1 = repl.compiler

          @ val n = 2
          n: Int = 2

          @ val c2 = repl.compiler

          @ import scala.collection.mutable.ListBuffer
          import scala.collection.mutable.ListBuffer

          @ val c3 = repl.compiler

          @ assert(c1 eq c2)

          @ assert(c1 eq c3)
        """)
      }

      'preconfigured - {
        check.session("""
          @ val c0 = repl.compiler

          @ interp.preConfigureCompiler(_ => ())

          @ val c1 = repl.compiler

          @ val n = 2
          n: Int = 2

          @ val c2 = repl.compiler

          @ import scala.collection.mutable.ListBuffer
          import scala.collection.mutable.ListBuffer

          @ val c3 = repl.compiler

          @ assert(c0 ne c1)

          @ assert(c1 eq c2)

          @ assert(c1 eq c3)
        """)
      }
    }

    'loadURL - {
      val sbv = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")
      val url = "https://repo1.maven.org/maven2/" +
        s"io/argonaut/argonaut_$sbv/6.2.2/argonaut_$sbv-6.2.2.jar"
      check.session(s"""
        @ interp.load.cp(new java.net.URL("$url"))

        @ import argonaut._, Argonaut._

        @ val json = Json.obj("a" -> Json.jBool(false)).nospaces
        json: String = "{\\"a\\":false}"
      """)
    }

    'accessPressy - {
      check.session("""
        @ def typeAt(code: String, pos: Int) = {
        @   import scala.tools.nsc.interactive.Response
        @   import scala.reflect.internal.util.{BatchSourceFile, OffsetPosition}
        @   val c = repl.interactiveCompiler
        @   val f = new BatchSourceFile("<virtual>", code)
        @   val r = new Response[Unit]
        @   c.askReload(List(f), r)
        @   r.get.fold(x => x, e => throw e)
        @   val r0 = new Response[c.Tree]
        @   c.askTypeAt(new OffsetPosition(f, pos), r0)
        @   r0.get.fold(x => x, e => throw e)
        @ }
        defined function typeAt

        @ val code = "object A { val l = List }"
        code: String = "object A { val l = List }"

        @ val t = typeAt(code, code.length - 2).toString
        t: String = ?

        @ assert(t.endsWith(".List"))
      """)
    }
  }
}
