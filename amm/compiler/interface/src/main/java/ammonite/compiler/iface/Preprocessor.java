package ammonite.compiler.iface;

import java.util.function.Function;

public abstract class Preprocessor {

  public abstract Output transform(
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
