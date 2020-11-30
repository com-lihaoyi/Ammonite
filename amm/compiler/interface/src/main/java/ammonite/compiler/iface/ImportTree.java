package ammonite.compiler.iface;

import java.util.ArrayList;
import java.util.Map;

public final class ImportTree {

  private final String[] prefix;
  private final Map.Entry<String, String>[] mappingsOrNull; // Option[Seq[(String, Option[String])]]
  private final int start;
  private final int end;
  private String[] strippedPrefix = null;

  public String[] prefix() {
    return prefix;
  }
  public Map.Entry<String, String>[] mappingsOrNull() {
    return mappingsOrNull;
  }
  public int start() {
    return start;
  }
  public int end() {
    return end;
  }

  public ImportTree updateBounds(int start, int end) {
    return new ImportTree(prefix, mappingsOrNull, start, end);
  }

  public ImportTree withPrefix(String[] newPrefix) {
    return new ImportTree(newPrefix, mappingsOrNull, start, end);
  }

  public String[] strippedPrefix() {
    if (strippedPrefix == null) {
      ArrayList<String> builder = new ArrayList<>();
      for (String elem : prefix) {
        if (!elem.startsWith("$"))
          break;
        builder.add(elem.substring(1));
      }
      strippedPrefix = builder.toArray(new String[builder.size()]);
    }
    return strippedPrefix;
  }

  public ImportTree(
    String[] prefix,
    Map.Entry<String, String>[] mappingsOrNull, // Option[Seq[(String, Option[String])]]
    int start,
    int end
  ) {
    this.prefix = prefix;
    this.mappingsOrNull = mappingsOrNull;
    this.start = start;
    this.end = end;
  }

  // TODO equals, hashCode, toString
}
