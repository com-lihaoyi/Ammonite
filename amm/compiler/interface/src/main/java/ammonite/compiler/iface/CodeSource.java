package ammonite.compiler.iface;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.Objects;

public final class CodeSource {

  private final String wrapper;
  private final String[] flexiblePkg;
  private final String[] rawPkgRoot;
  private final Path pathOrNull;

  public CodeSource(
    String wrapper,
    String[] flexiblePkg,
    String[] rawPkgRoot,
    Path pathOrNull
  ) {
    this.wrapper = wrapper;
    this.flexiblePkg = flexiblePkg;
    this.rawPkgRoot = rawPkgRoot;
    this.pathOrNull = pathOrNull;
  }

  public String wrapper() {
    return this.wrapper;
  }
  public String[] flexiblePkg() {
    return this.flexiblePkg;
  }
  public String[] rawPkgRoot() {
    return this.rawPkgRoot;
  }
  public Path pathOrNull() {
    return this.pathOrNull;
  }

 @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj instanceof CodeSource) {
      CodeSource other = (CodeSource) obj;
      return this.wrapper.equals(other.wrapper) &&
          Arrays.equals(this.flexiblePkg, other.flexiblePkg) &&
          Arrays.equals(this.rawPkgRoot, other.rawPkgRoot) &&
          Objects.equals(this.pathOrNull, other.pathOrNull);
    }
    return false;
  }

  @Override
  public int hashCode() {
    int code = 17 + "CodeSource".hashCode();
    code = 37 * code + wrapper.hashCode();
    code = 37 * code + Arrays.hashCode(flexiblePkg);
    code = 37 * code + Arrays.hashCode(rawPkgRoot);
    code = 37 * code + Objects.hashCode(pathOrNull);
    return 37 * code;
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder("CodeSource(wrapper=");
    b.append(wrapper);
    b.append(", flexiblePkg=[");
    boolean isFirst = true;
    for (String elem : flexiblePkg) {
        if (isFirst) {
            isFirst = false;
        } else {
            b.append(", ");
        }

        b.append(elem);
    }
    b.append("], rawPkgRoot=[");
    isFirst = true;
    for (String elem : rawPkgRoot) {
        if (isFirst) {
            isFirst = false;
        } else {
            b.append(", ");
        }

        b.append(elem);
    }
    b.append("], path=");
    b.append(pathOrNull);
    b.append(")");
    return b.toString();
  }
}
