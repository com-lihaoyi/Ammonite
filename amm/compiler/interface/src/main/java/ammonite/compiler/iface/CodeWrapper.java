package ammonite.compiler.iface;

import java.util.Map;

public abstract class CodeWrapper {
  public abstract String[] rawWrapperPath();
  public abstract Map.Entry<String, Map.Entry<String, Integer>> wrap(
    String code,
    CodeSource source,
    Imports imports,
    String printCode,
    String indexedWrapper, // Name
    String extraCode
  );
}
