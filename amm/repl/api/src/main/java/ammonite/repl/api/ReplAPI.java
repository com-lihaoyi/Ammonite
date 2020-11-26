package ammonite.repl.api;

import java.util.function.Supplier;

import ammonite.compiler.iface.Imports;

public abstract class ReplAPI {

  public abstract String getPrompt();

  public abstract FrontEnd getFrontEnd();

  /**
    * The last exception that was thrown in the REPL; `null` if nothing has
    * yet been thrown. Useful if you want additional information from the
    * thrown exception than the printed stack trace (e.g. many exceptions have
    * additional metadata attached) or if you want to show the stack trace
    * on an exception that doesn't normally print it (e.g. seeing the stack
    * when a Ctrl-C interrupt happened) via `lastException.printStackTrace`.
    */
  public abstract Throwable lastException();

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  public abstract void newCompiler();

  /**
   * Shows all imports added that bring values into scope for the commands a
   * user runs; *includes* imports from the built-in predef and user predef files
   */
  public abstract Imports fullImports();

  /**
   * Shows the imports added to scope by the commands a user has entered so far;
   * *excludes* imports from the built-in predef and user predef files
   */
  public abstract Imports imports();

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
  public abstract String[] usedEarlierDefinitions();

  /**
   * Current width of the terminal
   */
  public abstract int width();
  /**
   * Current height of the terminal
   */
  public abstract int height();

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
  public abstract Session sess();

  public abstract ReplLoad replLoad();

  // internal
  public abstract ammonite.compiler.iface.Logger printer();
  public abstract Object[] replArgs();
  public abstract String[] fullRawHistory();
  public abstract String[] rawHistory();

  public abstract Colors getColors();
  public abstract void setColors(Colors colors);
  public abstract void setPrompt(Supplier<String> prompt);
  public abstract void setFrontEnd(FrontEnd frontEnd);

  private Object data0 = null;
  public final Object data() {
    return data0;
  }
  public final void setData(Object data) {
    this.data0 = data;
  }
}
