package ammonite.compiler.iface;

import java.net.URL;
import java.util.function.Consumer;
import java.util.Map;

public abstract class CompilerBuilder {

  public abstract Compiler create(
    URL[] initialClassPath,
    URL[] classPath,
    Map.Entry<String, byte[]>[] dynamicClassPath,
    ClassLoader evalClassLoader,
    ClassLoader pluginClassLoader,
    Consumer<Message> reporter,
    String[] settings,
    String[][] classPathWhiteList,
    boolean lineNumberModifier
  );

  public abstract String userScalaVersion();

  public static final class Message {
    private final String severity;
    private final int start;
    private final int end;
    private final String message;

    public String severity() {
      return severity;
    }
    public int start() {
      return start;
    }
    public int end() {
      return end;
    }
    public String message() {
      return message;
    }

    public Message(
      String severity,
      int start,
      int end,
      String message
    ) {
      this.severity = severity;
      this.start = start;
      this.end = end;
      this.message = message;
    }
  }

}
