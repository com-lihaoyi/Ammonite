package ammonite.compiler.iface;

import java.util.function.Function;

/**
  * Responsible for all scala-source-code-munging that happens within the
  * Ammonite REPL.
  *
  * Performs several tasks:
  *
  * - Takes top-level Scala expressions and assigns them to `res{1, 2, 3, ...}`
  *   values so they can be accessed later in the REPL
  *
  * - Wraps the code snippet with an wrapper `object` since Scala doesn't allow
  *   top-level expressions
  *
  * - Mangles imports from our [[ammonite.util.ImportData]] data structure into a source
  *   String
  *
  * - Combines all of these into a complete compilation unit ready to feed into
  *   the Scala compiler
  */
public abstract class Preprocessor {

  public abstract Output transformOrNull(
    String[] stmts,
    String resultIndex,
    String leadingSpaces,
    CodeSource codeSource,
    String indexedWrapperName, // Name
    Imports imports,
    Function<String, String> printerTemplate,
    String extraCode,
    boolean skipEmpty,
    boolean markScript,
    CodeWrapper codeWrapper) throws PreprocessorError;

  public static class PreprocessorError extends Exception {
    public PreprocessorError(String message) {
      super(message);
    }
  }

  public static final class Output {
    private final String code;
    private final int prefixCharLength;
    private final int userCodeNestingLevel;

    public Output(
      String code,
      int prefixCharLength,
      int userCodeNestingLevel
    ) {
      this.code = code;
      this.prefixCharLength = prefixCharLength;
      this.userCodeNestingLevel = userCodeNestingLevel;
    }

    public String code() {
      return code;
    }
    public int prefixCharLength() {
      return prefixCharLength;
    }
    public int userCodeNestingLevel() {
      return userCodeNestingLevel;
    }
  }

}
