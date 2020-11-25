package ammonite.repl.api;

import java.net.URL;

public interface SessionChanged {
  String[] rawRemovedImports(); // Set[scala.Symbol]
  String[] rawAddedImports(); // Set[scala.Symbol]
  URL[] rawRemovedJars(); // Set[java.net.URL]
  URL[] rawAddedJars(); // Set[java.net.URL]
}
