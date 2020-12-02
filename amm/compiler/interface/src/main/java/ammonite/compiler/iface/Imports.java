package ammonite.compiler.iface;

import java.util.Arrays;

/**
  * Represents the imports that occur before a piece of user code in the
  * Ammonite REPL. It's basically a `Seq[ImportData]`, except we really want
  * it to be always in a "canonical" form without shadowed/duplicate imports.
  *
  * Thus we only expose an `apply` method which performs this de-duplication,
  * and a `++` operator that combines two sets of imports while performing
  * de-duplication.
  */
public final class Imports {

  private final Data[] data;

  public Imports(Data[] data) {
    this.data = data;
  }

  public Imports() {
    this.data = new Data[] {};
  }

  public Data[] data() {
    return this.data;
  }


  public static final String Type = "Type";
  public static final String Term = "Term";
  public static final String TermType = "TermType";

  public static final class Data {

    private final String from;
    private final String to;
    private final String[] rawPrefix;
    private final String type;

    public Data(
      String from,
      String to,
      String[] rawPrefix,
      String type
    ) {
      this.from = from;
      this.to = to;
      this.rawPrefix = rawPrefix;
      this.type = type;
    }

    public String from() {
      return this.from;
    }
    public String to() {
      return this.to;
    }
    public String[] rawPrefix() {
      return this.rawPrefix;
    }
    public String type() {
      return this.type;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj)
        return true;
      if (obj instanceof Data) {
        Data other = (Data) obj;
        return this.from.equals(other.from) &&
            this.to.equals(other.to) &&
            Arrays.equals(this.rawPrefix, other.rawPrefix) &&
            this.type.equals(other.type);
      }
      return false;
    }

    @Override
    public int hashCode() {
      int code = 17 + "Import.Data".hashCode();
      code = 37 * code + from.hashCode();
      code = 37 * code + to.hashCode();
      code = 37 * code + Arrays.hashCode(rawPrefix);
      code = 37 * code + type.hashCode();
      return 37 * code;
    }

    @Override
    public String toString() {
      StringBuilder b = new StringBuilder("Import.Data(from=");
      b.append(from);
      b.append(", to=");
      b.append(to);
      b.append(", prefix=[");
      boolean isFirst = true;
      for (String elem : rawPrefix) {
          if (isFirst) {
              isFirst = false;
          } else {
              b.append(", ");
          }

          b.append(elem);
      }
      b.append("], type=");
      b.append(type);
      b.append(")");
      return b.toString();
    }
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj instanceof Imports) {
      Imports other = (Imports) obj;
      return Arrays.equals(this.data, other.data);
    }
    return false;
  }

  @Override
  public int hashCode() {
    int code = 17 + "Import".hashCode();
    code = 37 * code + Arrays.hashCode(data);
    return 37 * code;
  }

  public String asString() {
    StringBuilder b = new StringBuilder("Import(data=[");
    boolean isFirst = true;
    for (Data elem : data) {
        if (isFirst) {
            isFirst = false;
        } else {
            b.append(", ");
        }

        b.append(elem);
    }
    b.append("])");
    return b.toString();
  }

  @Override
  public String toString() {
    throw new RuntimeException("nope");
  }
}
