package ammonite.repl.api;

public interface Colors {

  public static class Default implements Colors {

  }

  public static Default DEFAULT = new Default();

  public static class BlackWhite implements Colors {

  }

  public static BlackWhite BLACKWHITE = new BlackWhite();

}
