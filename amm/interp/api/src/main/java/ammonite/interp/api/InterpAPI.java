package ammonite.interp.api;

import java.nio.file.Path;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.List;

import coursierapi.Fetch;
import coursierapi.Repository;


public abstract class InterpAPI {
  /**
    * When running a script in `--watch` mode, re-run the main script if this
    * file changes. By default, this happens for all script files, but you can
    * call this to watch arbitrary files your script may depend on
    */
  public abstract void watch(Path p);

  /**
   * A generalization of [[watch]], allows watching arbitrary values and not
   * just the contents of file paths.
   */
  public abstract <T> void addWatchValue(Supplier<T> v);

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  public abstract InterpLoad load();

  // Internal
  public abstract List<Repository> repositoriesList();

  // Internal
  public abstract List<Function<Fetch, Fetch>> resolutionHooksList();

  // Internal
  public abstract List<Function<Object, Object>> beforeExitHooksList();
}
