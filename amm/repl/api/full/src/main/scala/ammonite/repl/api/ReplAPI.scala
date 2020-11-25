package ammonite.repl.api

import java.nio.file.Path

import ammonite.compiler.iface.Imports
import ammonite.util.{Colors => _, _}

import scala.reflect.runtime.universe.{Bind => _, _}




trait ReplAPI {



  /**
   * Read/writable prompt for the shell. Use this to change the
   * REPL prompt at any time!
   */
  def prompt: String
  def prompt_=(prompt: => String): Unit

  /**
   * The front-end REPL used to take user input. Modifiable!
   */
  val frontEnd: Ref[FrontEnd]

  /**
    * The last exception that was thrown in the REPL; `null` if nothing has
    * yet been thrown. Useful if you want additional information from the
    * thrown exception than the printed stack trace (e.g. many exceptions have
    * additional metadata attached) or if you want to show the stack trace
    * on an exception that doesn't normally print it (e.g. seeing the stack
    * when a Ctrl-C interrupt happened) via `lastException.printStackTrace`.
    */
  def lastException: Throwable

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

  /**
   * Access the compiler to do crazy things if you really want to!
   */
  def compiler: scala.tools.nsc.Global

  /**
    * Access the presentation compiler to do even crazier things if you really want to!
    */
  def interactiveCompiler: scala.tools.nsc.interactive.Global

  /**
   * Shows all imports added that bring values into scope for the commands a
   * user runs; *includes* imports from the built-in predef and user predef files
   */
  def fullImports: Imports

  /**
   * Shows the imports added to scope by the commands a user has entered so far;
   * *excludes* imports from the built-in predef and user predef files
   */
  def imports: Imports

  /**
    * If class wrapping is enabled, this lists the names of the previous commands
    * that the current commands actually references (as told by the scalac).
    *
    * E.g. in a session like
    * ```
    * {@literal @} val n = 2
    * n: Int = 2
    *
    * {@literal @} val p = 1
    * p: Int = 1
    *
    * {@literal @} n + p
    * res2: Int = 3
    * ```
    * this would have returned an empty list if called from the same line as `val n = 2`
    * or `val p = 1`. This would have returned `Seq("cmd0", "cmd1")` if called
    * from the same line as `n + p`, as both `cmd0`, that defines `n`, and `cmd1`, that
    * defines `p`, are referenced from this line.
    */
  def usedEarlierDefinitions: Seq[String]

  /**
   * Current width of the terminal
   */
  def width: Int
  /**
   * Current height of the terminal
   */
  def height: Int

  /**
    * Functions that can be used to manipulate the current REPL session:
    * check-pointing progress, reverting to earlier checkpoints, or deleting
    * checkpoints by name.
    *
    * Frames get pushed on a stack; by default, a saved frame is
    * accessible simply by calling `load`. If you provide a name
    * when `save`ing a checkpoint, it can later be `load`ed directly
    * by providing the same name to `load`
    *
    * Un-named checkpoints are garbage collected, together with their
    * classloader and associated data, when they are no longer accessible
    * due to `restore`. Named checkpoints are kept forever; call `delete`
    * on them if you really want them to go away.
    */
  def sess: Session

  def load: ReplLoad

  // internal
  def printer: Printer
  def fullRawHistory: Array[String]
  def rawHistory: Array[String]
  def replArgs: IndexedSeq[Bind[_]]

  def getColors: Colors
  def setColors(colors: Colors): Unit

  private var data0: Object = null
  def data = data0
  def setData(data: Object) = {
    data0 = data
  }
}