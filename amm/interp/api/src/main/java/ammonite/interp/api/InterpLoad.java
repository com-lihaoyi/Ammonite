package ammonite.interp.api;

import java.nio.file.Path;

public interface InterpLoad extends LoadJar {

  void module(Path path);

  LoadJar plugin();

}
