package ammonite.interp

import ammonite.compiler.{CompilerBuilder, DefaultCodeWrapper}
import ammonite.main.Defaults
import ammonite.repl.Repl
import ammonite.runtime.{ImportHook, Storage}
import ammonite.util._
import utest._

import java.io.{ByteArrayInputStream}
import java.nio.charset.StandardCharsets

object ReplTests extends TestSuite {
  val tempDir = os.Path(
    java.nio.file.Files.createTempDirectory("ammonite-tester")
  )

  def runInRepl(code: String): (String, String) = {
    val stdoutBuilder = new StringBuilder()
    val stderrBuilder = new StringBuilder()
    val repl = new Repl(
      input = new ByteArrayInputStream(code.getBytes(StandardCharsets.UTF_8)),
      output = (b: Int) => stdoutBuilder.append(b.toChar),
      error = (b: Int) => stderrBuilder.append(b.toChar),
      storage = new Storage.Folder(tempDir),
      baseImports = ammonite.main.Defaults.replImports ++ Interpreter.predefImports,
      basePredefs = Seq(
        PredefInfo(Name("testPredef"), "", false, None)
      ),
      customPredefs = Seq(),
      wd = os.pwd,
      welcomeBanner = None,
      replCodeWrapper = DefaultCodeWrapper,
      scriptCodeWrapper = DefaultCodeWrapper,
      alreadyLoadedDependencies = Defaults.alreadyLoadedDependencies(),
      importHooks = ImportHook.defaults,
      compilerBuilder = CompilerBuilder(),
      parser = ammonite.compiler.Parsers,
      initialColors = Colors.BlackWhite,
      classPathWhitelist = ammonite.repl.Repl.getClassPathWhitelist(thin = true)
    )
    repl.initializePredef()
    repl.run()

    (stdoutBuilder.toString(), stderrBuilder.toString())
  }

  override val tests = Tests {
    println("ReplTests")

    test("Simple complete") {
      val (stdout, _) = runInRepl("prin\t")
      Seq("print", "printf", "println").foreach { str =>
        assert(stdout.contains(str))
      }
      assert(!stdout.contains("javax.print"))
    }
    test("Complete with wildcard imports") {
      // Code completion used to break after a wildcard import
      // https://github.com/com-lihaoyi/Ammonite/issues/1009
      val (stdout, _) = runInRepl("import sys.process._\nprin\t")
      Seq("print", "printf", "println").foreach { str =>
        assert(stdout.contains(str))
        // some wrong completion item which used to show up when completion was broken
        assert(!stdout.contains("javax.print"))
      }
    }
  }
}