package ammonite.compiler.iface;

public interface Logger {
  void printInfo(String message);
  void printWarning(String message);
  void printError(String message);
}
