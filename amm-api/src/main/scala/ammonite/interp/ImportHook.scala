package ammonite.interp

import java.io.File

import acyclic.file
import ammonite.ops.{read, _}
import ammonite.util._

/**
  * An extensible hook into the Ammonite REPL's import system; allows the end
  * user to hook into `import $foo.bar.{baz, qux => qua}` syntax, and in
  * response load jars or process source files before the "current" compilation
  * unit is run. Can be used to load script files, ivy dependencies, jars, or
  * files from the web.
  */
trait ImportHook{
  def handle(source: ImportHook.Source,
             tree: ImportTree,
             interp: ImportHook.InterpreterInterface): Res[Seq[ImportHook.Result]]
}

object ImportHook{

  /**
    * The minimal interface that is exposed to the import hooks from the
    * Interpreter. Open for extension, if someone needs more stuff, but by
    * default this is what is available.
    */
  trait InterpreterInterface{
    def eval: Evaluator
    def processModule(source: ImportHook.Source,
                      code: String,
                      wrapperName: Name,
                      pkgName: Seq[Name],
                      autoImport: Boolean): Res[Imports]
    def reInit(): Unit
    def wd: Path
    def loadIvy(coordinates: (String, String, String), verbose: Boolean = true): Set[File]
  }

  /**
    * The result of processing an [[ImportHook]]. Can be either a source-file
    * to evaluate, or additional files/folders/jars to put on the classpath
    */
  sealed trait Result
  object Result{
    case class Source(code: String,
                      wrapper: Name,
                      pkg: Seq[Name],
                      source: ImportHook.Source,
                      imports: Imports,
                      exec: Boolean) extends Result
    case class ClassPath(file: Path, plugin: Boolean) extends Result
  }

  /**
    * Where a script can "come from". Used to resolve relative $file imports
    * relative to the importing script.
    */
  sealed trait Source
  object Source{
    case class File(path: Path) extends Source
    case class URL(path: String) extends Source
  }
}