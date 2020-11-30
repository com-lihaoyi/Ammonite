package ammonite.compiler.iface;

import java.util.Map;

public abstract class Parser {

  public abstract ParsedImportHooks importHooks(
    CodeSource source,
    Map.Entry<Integer, String>[] statements
  );

  public abstract ScriptBlock[] scriptBlocks(
    String rawCode,
    String fileName
  ) throws ScriptSplittingError; // Either[String, IndexedSeq[(String, Seq[String])]]

  public abstract ScriptBlock[] scriptBlocksWithStartIndices(
    String rawCode,
    String fileName
  ) throws ScriptSplittingError; // Either[Parsed.Failure, IndexedSeq[(Int, String, Seq[(Int, String)])]]

  public static final class ParsedImportHooks {
    private final String[] hookStatements;
    private final ImportTree[] importTrees;

    public ParsedImportHooks(
      String[] hookStatements,
      ImportTree[] importTrees
    ) {
      this.hookStatements = hookStatements;
      this.importTrees = importTrees;
    }

    public String[] hookStatements() {
      return hookStatements;
    }
    public ImportTree[] importTrees() {
      return importTrees;
    }

    // TODO equals, hashCode, toString
  }

  public static final class ScriptBlock {
    private final int startIndex;
    private final String ncomment;
    private final Map.Entry<Integer, String>[] codeWithStartIndices;

    public int startIndex() {
      return startIndex;
    }
    public String ncomment() {
      return ncomment;
    }
    public Map.Entry<Integer, String>[] codeWithStartIndices() {
      return codeWithStartIndices;
    }

    public ScriptBlock(
      String ncomment,
      Map.Entry<Integer, String>[] codeWithStartIndices
    ) {
      this.startIndex = 0;
      this.ncomment = ncomment;
      this.codeWithStartIndices = codeWithStartIndices;
    }

    public ScriptBlock(
      int startIndex,
      String ncomment,
      Map.Entry<Integer, String>[] codeWithStartIndices
    ) {
      this.startIndex = startIndex;
      this.ncomment = ncomment;
      this.codeWithStartIndices = codeWithStartIndices;
    }

    // TODO equals, hashCode, toString
  }

  public static class ScriptSplittingError extends Exception {
    private final int index;
    private final String expected;
    public ScriptSplittingError(String message) {
      super(message);
      this.index = -1;
      this.expected = null;
    }
    public ScriptSplittingError(
      String message,
      int index,
      String expected
    ) {
      super(message);
      this.index = index;
      this.expected = expected;
    }

    public int index() {
      return index;
    }
    public String expected() {
      return expected;
    }
  }
}
