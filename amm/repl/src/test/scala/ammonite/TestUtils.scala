package ammonite

import java.io.PrintStream

import ammonite.compiler.{CompilerLifecycleManager, ObjectCodeWrapper, Preprocessor}
import ammonite.compiler.iface.Imports
import ammonite.interp.Interpreter
import ammonite.main.Defaults
import ammonite.runtime.{Frame, Storage}
import ammonite.util._
import ammonite.runtime.ImportHook

object TestUtils {
  def scala2_11 = scala.util.Properties.versionNumberString.startsWith("2.11")
  def scala2_12 = scala.util.Properties.versionNumberString.startsWith("2.12")

  def createTestInterp(
    storage: Storage,
    predefImports: Imports = new Imports,
    predef: String = ""
  ) = {
    val initialClassLoader = Thread.currentThread().getContextClassLoader
    val startFrame = Frame.createInitial(initialClassLoader)
    val printStream = new PrintStream(System.out)
    val interp = new Interpreter(
      new CompilerLifecycleManager,
      printer = Printer(
        printStream, new PrintStream(System.err), printStream,
        println, println, println
      ),
      storage = storage,
      wd = os.pwd,
      getFrame = () => startFrame,
      createFrame = () => throw new Exception("unsupported"),
      initialClassLoader = initialClassLoader,
      replCodeWrapper = ObjectCodeWrapper,
      scriptCodeWrapper = ObjectCodeWrapper,
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
}
