package ammonite.repl.api

import ammonite.util._

trait ReplAPI extends ReplAPIScalaVersionSpecific {

  /**
   * Read/writable prompt for the shell. Use this to change the
   * REPL prompt at any time!
   */
  val prompt: Ref[String]

  /**
   * The front-end REPL used to take user input. Modifiable!
   */
  val frontEnd: Ref[FrontEnd]

  /**
   * Display help text if you don't know how to use the REPL
   */
  def help: String

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
   * History of commands that have been entered into the shell, including
   * previous sessions
   */
  def fullHistory: History

  /**
   * History of commands that have been entered into the shell during the
   * current session
   */
  def history: History

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

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
   * @ val n = 2
   * n: Int = 2
   *
   * @ val p = 1
   * p: Int = 1
   *
   * @ n + p
   * res2: Int = 3
   * ```
   * this would have returned an empty list if called from the same line as `val n = 2`
   * or `val p = 1`. This would have returned `Seq("cmd0", "cmd1")` if called
   * from the same line as `n + p`, as both `cmd0`, that defines `n`, and `cmd1`, that
   * defines `p`, are referenced from this line.
   */
  def usedEarlierDefinitions: Seq[String]

  /**
   * Controls how things are pretty-printed in the REPL. Feel free
   * to shadow this with your own definition to change how things look
   */
  implicit def tprintColorsImplicit: pprint.TPrintColors

  implicit def codeColorsImplicit: CodeColors

  def pprinter: Ref[pprint.PPrinter]

  /**
   * Current width of the terminal
   */
  def width: Int

  /**
   * Current height of the terminal
   */
  def height: Int

  def show(t: Any): Unit

  /**
   * Lets you configure the pretty-printing of a value. By default, it simply
   * disables truncation and prints the entire thing, but you can set other
   * parameters as well if you want.
   */
  def show(t: Any, width: Integer = null, height: Integer = null, indent: Integer = null): Unit

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

  def clipboard: Clipboard

  def _compilerManager: ammonite.compiler.iface.CompilerLifecycleManager
}

trait ReplLoad {

  /**
   * Loads a command into the REPL and
   * evaluates them one after another
   */
  def apply(line: String): Unit

  /**
   * Loads and executes the scriptfile on the specified path.
   * Compilation units separated by `@\n` are evaluated sequentially.
   * If an error happens it prints an error message to the console.
   */
  def exec(path: os.Path): Unit
}

trait Session {

  /**
   * The current stack of frames
   */
  def frames: List[Frame]

  /**
   * Checkpoints your current work, placing all future work into its own
   * frames. If a name is provided, it can be used to quickly recover
   * that checkpoint later.
   */
  def save(name: String = ""): Unit

  /**
   * Discards the last frames, effectively reverting your session to
   * the last `save`-ed checkpoint. If a name is provided, it instead reverts
   * your session to the checkpoint with that name.
   */
  def load(name: String = ""): SessionChanged

  /**
   * Resets you to the last save point. If you pass in `num`, it resets
   * you to that many savepoints since the last one.
   */
  def pop(num: Int = 1): SessionChanged

  /**
   * Deletes a named checkpoint, allowing it to be garbage collected if it
   * is no longer accessible.
   */
  def delete(name: String): Unit
}

trait Clipboard {

  /**
   * Reads contents from the system clipboard.
   * @return System clipboard contents if they are readable as `String`,
   *         empty string otherwise.
   */
  def read: String

  /**
   * Sets the contents of the system clipboard.
   *
   * @param data New contents for the clipboard.
   */
  def write(data: geny.Writable): Unit
}
