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
  def scala2_11 = scala.util.Properties.versionNumberString.startsWith("2.11")
  def scala2_12 = scala.util.Properties.versionNumberString.startsWith("2.12")

  def createTestInterp(
    storage: Storage,
    predefImports: Imports = Imports(),
    predef: String = ""
  ) = {
    val initialClassLoader = Thread.currentThread().getContextClassLoader
    val startFrame = Frame.createInitial(initialClassLoader)
    val printStream = new PrintStream(System.out)
    val interp = new Interpreter(

      printer = Printer(
        printStream, new PrintStream(System.err), printStream,
        println, println, println
      ),
      storage = storage,
      wd = os.pwd,
      colors = Ref(Colors.BlackWhite),
      getFrame = () => startFrame,
      createFrame = () => throw new Exception("unsupported"),
      initialClassLoader = initialClassLoader,
      replCodeWrapper = DefaultCodeWrapper,
      scriptCodeWrapper = DefaultCodeWrapper,
      alreadyLoadedDependencies = Defaults.alreadyLoadedDependencies("amm-test-dependencies.txt"),
      importHooks = ImportHook.defaults,
      classPathWhitelist = ammonite.repl.Repl.getClassPathWhitelist(thin = true)
    )
    // Provide a custom predef so we can verify in tests that the predef gets cached
    interp.initializePredef(
      Seq(),
      Seq(PredefInfo(Name("predef"), predef, false, None)),
      Seq(),
      predefImports
    )
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
