package ammonite.compiler.iface;

import java.nio.file.Path;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.Map;

public abstract class CompilerLifecycleManager {
  public abstract void setRTCacheDir(Path dir);
  public abstract void setHeadFrame(Supplier<Frame> headFrame);
  public abstract void setDependencyCompleter(Supplier<Function<String, Map.Entry<Integer, String[]>>> dependencyCompleter);
  public abstract void setClassPathWhiteList(String[][] whiteList);
  public abstract void setInitialClassLoader(ClassLoader initialClassLoader);

  public abstract Compiler compiler();
  public abstract int compilationCount();

  public abstract Preprocessor preprocessor(String fileName);

  public abstract String userScalaVersion();

  public abstract void init();
  public abstract void forceInit();

  public abstract Map.Entry<Integer, Map.Entry<String[], String[]>> completions(int offset, String previousImports, String snippet);

  public abstract Compiler.Output compileClassOrNull(
    Preprocessor.Output processed,
    Logger printer,
    String fileName);

  public abstract void addToClasspath(Map.Entry<String, byte[]>[] classFiles);

  public abstract Object objPressy();
  public abstract void shutdownPressy();
}
