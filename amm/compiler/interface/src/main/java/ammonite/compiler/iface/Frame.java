package ammonite.compiler.iface;

import java.net.URL;

public abstract class Frame {
  public abstract ReplClassLoader classloader();
  public abstract ReplClassLoader pluginClassloader();

  public abstract URL[] classpath();
  public abstract int version();
}
