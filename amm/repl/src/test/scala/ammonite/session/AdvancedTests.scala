package ammonite.session

import ammonite.TestUtils._
import ammonite.{DualTestRepl, TestRepl}
import ammonite.util.Res
import utest._


object AdvancedTests extends TestSuite{
  val tests = Tests{
    println("AdvancedTests")
    val check = new DualTestRepl()
    test("pprint"){
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
        res2: Foo = ${Print.Foo(i = "1", s0 = "\"\"", s1 = "List()")}

        @ Foo(
        @   1234567,
        @   "I am a cow, hear me moo",
        @   Seq("I weigh twice as much as you", "and I look good on the barbecue")
        @ )
        res3: Foo = ${Print.Foo(
          i = 1234567,
          s0 = "\"I am a cow, hear me moo\"",
          s1 = "List(\"I weigh twice as much as you\", \"and I look good on the barbecue\")",
          indent = "        "
        )}
      """)
    }

    test("exit"){
      check.result("exit", Res.Exit())
    }
    test("skip"){
      check.result("", Res.Skip)
    }

    test("predef"){
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
    test("predefSettings"){
      val check2 = new DualTestRepl{
        override def predef = (
          if (scala2)
            """
            interp.configureCompiler(_.settings.Xexperimental.value = true)
            """
          else
            """
            interp.preConfigureCompiler(ctx => ctx.setSetting(ctx.settings.showPlugins, true))
            """,
          None
        )
      }
      val getSetting =
        if (check2.scala2) "repl.compiler.settings.Xexperimental.value"
        else "repl.initialContext.settings.showPlugins.value(using repl.initialContext)"
      check2.session(s"""
        @ $getSetting
        res0: Boolean = true
      """)

    }
    test("macros"){
      if (check.scala2)
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
      else
        check.session("""
          @ inline def assert1(cond: Boolean, msg: => String) =
          @   if !cond then
          @     throw new Exception(msg)
          @ //
          defined function assert1

          @ assert1(true, "error1"); val n = 2
          n: Int = 2

          @ assert1(false, "error1"); val m = 3
          error: java.lang.Exception: error1

          @ val ammStackTrace = {
          @   scala.util.Try(assert1(false, "error1"))
          @     .toEither
          @     .left
          @     .get
          @     .getStackTrace
          @     .map(_.toString)
          @     .filter(_.startsWith("ammonite.$sess."))
          @ }

          @ assert(ammStackTrace.nonEmpty)

          @ assert(ammStackTrace.forall(_.contains("cmd3.sc")))
        """)
    }
    test("typeScope"){
      // TPrint issue in Scala 3?
      val cmBufferType =
        if (check.scala2) "collection.mutable.Buffer" else "Buffer"
      val mBufferType =
        if (check.scala2) "mutable.Buffer" else "Buffer"
      check.session(s"""
        @ collection.mutable.Buffer(1)
        res0: $cmBufferType[Int] = ArrayBuffer(1)

        @ import collection.mutable

        @ collection.mutable.Buffer(1)
        res2: $mBufferType[Int] = ArrayBuffer(1)

        @ mutable.Buffer(1)
        res3: $mBufferType[Int] = ArrayBuffer(1)

        @ import collection.mutable.Buffer

        @ mutable.Buffer(1)
        res5: Buffer[Int] = ArrayBuffer(1)
      """)
    }
    test("trappedType"){
      check.session("""
        @ val nope = ammonite.Nope(2); val n = 2
        n: Int = 2
      """)
    }
    test("unwrapping"){
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
    test("forceWrapping"){
      check.session("""
        @ {{
        @   val x = 1
        @   val y = 2
        @   x + y
        @ }}
        res0: Int = 3
      """)
    }
    test("truncation"){
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
    test("private"){
      test("vals") - check.session(s"""
        @ private val x = 1; val y = x + 1
        y: Int = 2

        @ y
        res1: Int = 2

        @ x
        error: ${check.notFound("x")}

        @ {
        @ private[this] val a = 3
        @ val b = a * 4
        @ }

        @ a
        error: ${check.notFound("a")}

        @ b
        
      """)

      test("dontPrint"){
        check.session(
          """
          @ private object Foo { def get = "a" }; val s = Foo.get
          s: String = "a"

          @ private class Foo { def get = "a" }; val s = (new Foo).get
          s: String = "a"

          @ private trait Foo { def get = "a" }; val s = (new Foo {}).get
          s: String = "a"

          @ private def foo(): String = "a"; val s = foo()
          s: String = "a"

          @ private lazy val foo: String = "a"; val s = foo
          s: String = "a"

          @ private val foo: String = "a"; val s = foo
          s: String = "a"

          @ private type T = String; private def foo(): T = "a"; val s: String = foo()
          s: String = "a"
        """)
      }
    }
    test("compilerPlugin") - retry(3){
      if (check.scala2)
        check.session("""
          @ // Compiler plugins imported without `.$plugin` are not loaded

          @ import $ivy.`org.typelevel:::kind-projector:0.13.2`

          @ trait TC0[F[_]]
          defined trait TC0

          @ type TC0EitherStr = TC0[Either[String, *]]
          error: not found: type *

          @ // You need to use `import $plugin.$ivy`

          @ import $plugin.$ivy.`org.typelevel:::kind-projector:0.13.2`

          @ trait TC[F[_]]
          defined trait TC

          @ type TCEitherStr = TC[Either[String, *]]
          defined type TCEitherStr

          @ // Importing plugins doesn't affect the run-time classpath

          @ import $plugin.$ivy.`com.lihaoyi::scalatags:0.7.0`

          @ import scalatags.Text
          error: not found: value scalatags
        """)
      else
        "Disabled"
    }
    test("replApiUniqueness"){
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
    test("macro paradise or -Ymacro-annotations") {
      val init =
        if (scala2_12) {
          val scalaVer = scala.util.Properties.versionNumberString
          val paradiseVersion =
            if (scalaVer == "2.12.0" || scalaVer == "2.12.1") "2.1.0"
            else "2.1.1"
          s"""import $$plugin.$$ivy.`org.scalamacros:::paradise:$paradiseVersion`"""
        }
        else
          "interp.configureCompiler(_.settings.YmacroAnnotations.value = true)"

      if (check.scala2)
        check.session(s"""
          @ $init

          @ import $$ivy.`io.github.alexarchambault::data-class:0.2.3`

          @ import dataclass._
          import dataclass._

          @ @data class Foo(n: Int = 0)

          @ val foo = Foo()
          foo: Foo = ${Print.Foo(n = 0)}

          @ val foo2 = foo.withN(3)
          foo2: Foo = ${Print.Foo(n = 3)}

        """)
      else
        "Disabled in Scala 3"
    }
    test("desugar"){
      if (check.scala2)
        check.session("""
          @ desugar{1 + 2 max 3}
          res0: compiler.tools.Desugared = scala.Predef.intWrapper(3).max(3)
        """)
      else
        "Disabled in Scala 3"
    }
    test("loadingModulesInPredef"){

      val dir = os.pwd/'amm/'src/'test/'resources/'scripts/'predefWithLoad
      test("loadExec"){
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
      test("loadModule"){
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
      test("importIvy"){
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
    test("bytecodeForReplClasses"){
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
    test("customBridge"){
      check.session("""
        @ val s = test.message
        s: String = "ba"
      """)
    }

    test("dontRefreshCompiler"){
      test{
        // Conditional check due to https://github.com/scala/bug/issues/11564
        if (scala.util.Properties.versionNumberString != "2.13.0") check.session("""
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

      test("preconfigured"){
        // Conditional check due to https://github.com/scala/bug/issues/11564
        if (scala.util.Properties.versionNumberString != "2.13.0") check.session("""
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

    test("loadURL"){
      if (check.scala2) {
        val sbv = {
          val sv = if (check.scalaVersion.startsWith("3.")) "2.13" else check.scalaVersion
          if (sv.forall(c => c.isDigit || c == '.'))
            sv.split('.').take(2).mkString(".")
          else
            sv
        }
        val url = "https://repo1.maven.org/maven2/" +
          s"org/scalacheck/scalacheck_$sbv/1.14.0/scalacheck_$sbv-1.14.0.jar"
        check.session(s"""
          @ interp.load.cp(new java.net.URL("$url"))

          @ import org.scalacheck.Gen
          import org.scalacheck.Gen

          @ val check = Gen.choose(1, 5).sample.exists(_ <= 5)
          check: Boolean = true
        """)
      } else "Disabled in Scala 3"
    }

    test("accessPressy"){
      if (check.scala2) check.session("""
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
      """) else "N/A in Scala 3"
    }

    test("accessInMemoryClassMap"){
      check.session("""
        @ class Foo
        defined class Foo

        @ val classes = {
        @   implicitly[ammonite.repl.api.ReplAPI]
        @     .sess
        @     .frames
        @     .head
        @     .classloader
        @     .inMemoryClasses
        @ }
        classes: Map[String, Array[Byte]] = ?

        @ val name = classOf[Foo].getName
        name: String = ?

        @ val found = classes.contains(name)
        found: Boolean = true
      """)
    }

    test("given"){
      if (check.scala2) "N/A"
      else
        check.session("""
          @ class Foo
          defined class Foo

          @ given Ordering[Foo] = new Ordering[Foo] { def compare(a: Foo, b: Foo): Int = 0 }
          given_Ordering_Foo: Ordering[Foo] = <given>

          @ class Bar
          defined class Bar

          @ given (using Ordering[Foo]): Ordering[Bar] = {
          @   new Ordering[Bar] { def compare(a: Bar, b: Bar): Int = 0 }
          @ }
          defined function given_Ordering_Bar

          @ given fooOrd: Ordering[Foo] = {
          @   new Ordering[Foo] { def compare(a: Foo, b: Foo): Int = 0 }
          @ }
          fooOrd: Ordering[Foo] = <given>
        """)
    }
    test("extension-methods"){
      if (scala2) "N/A"
      else
        check.session("""
          @ extension (x: Int) def incr = x + 1
          defined extension methods

          @ 1.incr
          res1: Int = 2

          @ {
          @ extension (x: String)
          @   def ident = x
          @   def concat(other: String) = x ++ other
          @ }
          defined extension methods

          @ "test".ident
          res3: String = "test"

          @ "test".concat("test")
          res4: String = "testtest"
        """)
    }
    test("output-directory") {
      val dir = os.temp.dir(prefix = "amm-output-dir-test")
      val predef0 =
        """val n = 2
          |""".stripMargin
      val defaultCheck = new TestRepl(
        ammonite.compiler.CompilerBuilder(outputDir = Some((dir / "default").toNIO))
      ) {
        override def predef = (predef0, None)
      }
      val classBasedCheck = new TestRepl(
        ammonite.compiler.CompilerBuilder(outputDir = Some((dir / "class-based").toNIO))
      ) {
        override def predef = (predef0, None)
        override def codeWrapper = ammonite.compiler.CodeClassWrapper
      }
      val session = """
        @ val check = interp.outputDir.nonEmpty
        check: Boolean = true
      """
      defaultCheck.session(session)
      classBasedCheck.session(session)

      val expectedClassFiles = Seq(
        os.rel / "default" / "ammonite" / "predef" / "testPredef.class",
        os.rel / "default" / "ammonite" / "predef" / "testPredef$.class",
        os.rel / "default" / "ammonite" / "$sess" / "cmd0.class",
        os.rel / "default" / "ammonite" / "$sess" / "cmd0$.class",
        os.rel / "class-based" / "ammonite" / "predef" / "testPredef.class",
        os.rel / "class-based" / "ammonite" / "predef" / "testPredef$.class",
        os.rel / "class-based" / "ammonite" / "predef" / "testPredef$Helper.class",
        os.rel / "class-based" / "ammonite" / "$sess" / "cmd0.class",
        os.rel / "class-based" / "ammonite" / "$sess" / "cmd0$.class",
        os.rel / "class-based" / "ammonite" / "$sess" / "cmd0$Helper.class"
      )
      val expectedTastyFiles = Seq(
        os.rel / "default" / "ammonite" / "predef" / "testPredef.tasty",
        os.rel / "default" / "ammonite" / "$sess" / "cmd0.tasty",
        os.rel / "class-based" / "ammonite" / "predef" / "testPredef.tasty",
        os.rel / "class-based" / "ammonite" / "$sess" / "cmd0.tasty"
      )
      val expectedFiles =
        if (ammonite.compiler.CompilerBuilder.scalaVersion.startsWith("2."))
          expectedClassFiles
        else
          expectedClassFiles ++ expectedTastyFiles

      val files = os.walk(dir).filter(os.isFile(_)).map(_.relativeTo(dir))
      assert(files.sorted == expectedFiles.sorted)
    }
    test("comment and import") {
      check.session(
        """
          @ import $ivy.`org.typelevel::cats-kernel:2.6.1`

          @ {
          @   // hello
          @   import cats.kernel._
          @ }
          import cats.kernel._
        """
      )
    }
    test("class-path-hook") {
      val sbv = check.scalaBinaryVersion
      check.session(
        s"""
            @ var deps = Set.empty[String]

            @ repl.sess.frames.head.addHook {
            @   new ammonite.util.Frame.Hook {
            @     def addClasspath(additional: Seq[java.net.URL]): Unit = {
            @       deps = deps ++ additional.map(_.toString).filter(!_.endsWith("-sources.jar")).map(url => url.drop(url.lastIndexOf('/') + 1))
            @     }
            @   }
            @ }

            @ import $$ivy.`org.typelevel::cats-core:2.9.0`

            @ val firstExpectedDeps = Set(
            @   "cats-core_$sbv-2.9.0.jar",
            @   "cats-kernel_$sbv-2.9.0.jar"
            @ )

            @ val firstCheck = deps == firstExpectedDeps
            firstCheck: Boolean = true

            @ deps = Set.empty[String]

            @ import $$ivy.`org.typelevel::cats-core:2.9.0`

            @ val firstEmptyCheck = deps.isEmpty
            firstEmptyCheck: Boolean = true

            @ deps = Set.empty[String]

            @ interp.load.ivy("info.picocli" % "picocli" % "4.7.3")

            @ val secondExpectedDeps = Set(
            @   "picocli-4.7.3.jar"
            @ )

            @ val secondCheck = deps == secondExpectedDeps
            secondCheck: Boolean = true

            @ deps = Set.empty[String]

            @ interp.load.ivy("info.picocli" % "picocli" % "4.7.3")

            @ val secondEmptyCheck = deps.isEmpty
            secondEmptyCheck: Boolean = true
          """
      )
    }

    test("custom wrapper name prefix") {
      val check = new DualTestRepl {
        override def wrapperNamePrefix = Some("cell")
      }
      // Helper suffix stripped for class-based code wrapping
      check.session(
        """
          @ val clsName = getClass.getName.stripPrefix("ammonite.$sess.").stripSuffix("Helper")
          clsName: String = "cell0$"
        """
      )
    }

    test("warnings") {

      val checkWithoutWarnings = new DualTestRepl {
        override def warnings = false
      }

      checkWithoutWarnings.session(
        """
          @ @deprecated("foo", "1.2") def value(): Int = 2

          @ val n = value()
          warning:

          @ val n0 = n
          n0: Int = 2
        """
      )

      val objCheck = new TestRepl
      val clsCheck = new TestRepl {
        override def codeWrapper = ammonite.compiler.CodeClassWrapper
      }

      if (scala2) {
        objCheck.session(
          """
            @ @deprecated("foo", "1.2") def value(): Int = 2
            defined function value

            @ val n = value()
            warning: cmd1.sc:1: method value in object cmd0 is deprecated (since 1.2): foo
            val n = value()
                    ^
          """
        )
        clsCheck.session(
          """
            @ @deprecated("foo", "1.2") def value(): Int = 2
            defined function value

            @ val n = value()
            warning: cmd1.sc:1: method value in class Helper is deprecated (since 1.2): foo
            val n = value()
                    ^
          """
        )
      }
      else {
        objCheck.session(
          """
            @ @deprecated("foo", "1.2") def value(): Int = 2
            defined function value

            @ val n = value()
            warning: -- Warning: cmd1.sc:1:8 --------------------------------------------------------
            1 |val n = value()
              |        ^^^^^
              |        method value in object cmd0 is deprecated since 1.2: foo
          """
        )
        clsCheck.session(
          """
            @ @deprecated("foo", "1.2") def value(): Int = 2
            defined function value

            @ val n = value()
            warning: -- Warning: cmd1.sc:1:8 --------------------------------------------------------
            1 |val n = value()
              |        ^^^^^
              |        method value in class Helper is deprecated since 1.2: foo
          """
        )
      }
    }
  }
}
