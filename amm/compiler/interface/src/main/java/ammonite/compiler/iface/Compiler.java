package ammonite.compiler.iface;

import java.util.Arrays;
import java.util.Map;

public abstract class Compiler {

  public abstract Output compileOrNull(
    byte[] src,
    Logger printer,
    int importsLen,
    int userCodeNestingLevel,
    String fileName
  );

  public abstract Preprocessor preprocessor(String fileName, boolean markGeneratedSections);
  public final Preprocessor preprocessor(String fileName) {
    return preprocessor(fileName, false);
  }

  public abstract Object objCompiler();

  public static final class Output {

    private final Map.Entry<String, byte[]>[] classFiles;
    private final Imports imports;
    private final String[] usedEarlierDefinitionsOrNull;

    public Output(
      Map.Entry<String, byte[]>[] classFiles,
      Imports imports,
      String[] usedEarlierDefinitionsOrNull
    ) {
      this.classFiles = classFiles;
      this.imports = imports;
      this.usedEarlierDefinitionsOrNull = usedEarlierDefinitionsOrNull;
    }

    public Map.Entry<String, byte[]>[] classFiles() {
      return classFiles;
    }
    public Imports imports() {
      return imports;
    }
    public String[] usedEarlierDefinitionsOrNull() {
      return usedEarlierDefinitionsOrNull;
    }

   @Override
    public boolean equals(Object obj) {
      if (this == obj)
        return true;
      if (obj instanceof Output) {
        Output other = (Output) obj;
        return this.classFiles.equals(other.classFiles) &&
            this.imports.equals(other.imports) &&
            Arrays.equals(this.usedEarlierDefinitionsOrNull, other.usedEarlierDefinitionsOrNull);
      }
      return false;
    }

    @Override
    public int hashCode() {
      int code = 17 + "Compiler.Output".hashCode();
      code = 37 * code + classFiles.hashCode();
      code = 37 * code + imports.hashCode();
      code = 37 * code + Arrays.hashCode(usedEarlierDefinitionsOrNull);
      return 37 * code;
    }

    @Override
    public String toString() {
      StringBuilder b = new StringBuilder("Compiler.Output(classFiles=[");
      boolean isFirst = true;
      for (Map.Entry<String, byte[]> elem : classFiles) {
          if (isFirst) {
              isFirst = false;
          } else {
              b.append(", ");
          }
          b.append(elem.getKey());
      }
      b.append("], imports=");
      b.append(imports.toString());
      b.append(", usedEarlierDefinitionsOrNull=");
      if (usedEarlierDefinitionsOrNull == null) {
        b.append("null");
      } else {
        b.append("[");
        isFirst = true;
        for (String elem : usedEarlierDefinitionsOrNull) {
            if (isFirst) {
                isFirst = false;
            } else {
                b.append(", ");
            }
            b.append(elem);
        }
        b.append("]");
      }
      b.append(")");
      return b.toString();
    }
  }

}
