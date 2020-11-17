package ammonite.interp.api;

import java.nio.file.Path;

import coursierapi.Dependency;

public interface LoadJar {

  /**
    * Load a `.jar` from a URL into your JVM classpath
    */
  void cp(java.net.URL jar);
  /**
   * Load one or more `.jar` files or directories into your JVM classpath
   */
  void cp(Path... jars);
  /**
   * Load a library from its maven/ivy coordinates
   */
  void ivy(Dependency... coordinates);
}
