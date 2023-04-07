package ammonite

import java.io.PrintStream

import ammonite.compiler.DefaultCodeWrapper
import ammonite.interp.Interpreter
import ammonite.main.Defaults
import ammonite.runtime.{Frame, Storage}
import ammonite.util._
import ammonite.runtime.ImportHook

import scala.language.dynamics

object TestUtils {
  def scala2_11 = ammonite.compiler.CompilerBuilder.scalaVersion.startsWith("2.11.")
  def scala2_12 = ammonite.compiler.CompilerBuilder.scalaVersion.startsWith("2.12.")
  def scala2 = ammonite.compiler.CompilerBuilder.scalaVersion.startsWith("2.")

  def createTestInterp(
    storage: Storage,
    predefImports: Imports = Imports(),
    predef: String = ""
  ) = {
    val initialClassLoader = Thread.currentThread().getContextClassLoader
    val startFrame = Frame.createInitial(initialClassLoader)
    val printStream = new PrintStream(System.out)
    val interpParams = Interpreter.Parameters(
      printer = Printer(
        printStream, new PrintStream(System.err), printStream,
        println, println, println
      ),
      storage = storage,
      wd = os.pwd,
      colors = Ref(Colors.BlackWhite),
      initialClassLoader = initialClassLoader,
      alreadyLoadedDependencies = Defaults.alreadyLoadedDependencies("amm-test-dependencies.txt"),
      importHooks = ImportHook.defaults,
      classPathWhitelist = ammonite.repl.Repl.getClassPathWhitelist(thin = true)
    )
    val interp = new Interpreter(
      ammonite.compiler.CompilerBuilder,
      () => ammonite.compiler.Parsers,

      getFrame = () => startFrame,
      createFrame = () => throw new Exception("unsupported"),
      replCodeWrapper = DefaultCodeWrapper,
      scriptCodeWrapper = DefaultCodeWrapper,
      parameters = interpParams
    )
    // Provide a custom predef so we can verify in tests that the predef gets cached
    for {
      (error, _) <- interp.initializePredef(
        Seq(),
        Seq(PredefInfo(Name("predef"), predef, false, None)),
        Seq(),
        predefImports
      )
    } {
      val (msgOpt, causeOpt) = error match {
        case r: Res.Exception => (Some(r.msg), Some(r.t))
        case r: Res.Failure => (Some(r.msg), None)
        case _ => (None, None)
      }

      throw new Exception(
        s"Error during predef initialization${msgOpt.fold("")(": " + _)}",
        causeOpt.orNull
      )
    }

    interp
  }

  object Print extends Dynamic {
    def applyDynamicNamed(className: String)(args: (String, Any)*): String = {
      var indent = ""
      val fields = args.flatMap {
        case ("indent", value: String) => indent = value; Nil
        case (name, value) =>
          val repr =
            if (scala2_12) s"$value"
            else s"$name = $value"
          Seq(repr)
      }
      if (indent.isEmpty())
        s"$className(${fields.mkString(", ")})"
      else
        s"$className(\n${fields.map(indent + "  " + _).mkString(",\n")}\n$indent)"
    }
  }
}
