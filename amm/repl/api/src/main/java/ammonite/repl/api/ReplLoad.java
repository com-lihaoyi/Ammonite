package ammonite.repl.api;

import java.nio.file.Path;

public interface ReplLoad {
  /**
    * Loads a command into the REPL and
    * evaluates them one after another
    */
  void apply(String line);

  /**
    * Loads and executes the scriptfile on the specified path.
    * Compilation units separated by `@\n` are evaluated sequentially.
    * If an error happens it prints an error message to the console.
    */
  void exec(Path path);
}
