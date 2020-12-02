package ammonite.compiler.iface;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;

public abstract class ReplClassLoader extends URLClassLoader {

  public ReplClassLoader(URL[] urls, ClassLoader parent) {
    super(urls, parent);
  }

  public abstract Map<String, byte[]> inMemoryClassesMap();
}
