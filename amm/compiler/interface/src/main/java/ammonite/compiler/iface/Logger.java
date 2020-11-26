package ammonite.compiler.iface;

import java.io.PrintStream;

public interface Logger {
  PrintStream outStream();
  PrintStream errStream();
  PrintStream resultStream();
  void printInfo(String message);
  void printWarning(String message);
  void printError(String message);
}
