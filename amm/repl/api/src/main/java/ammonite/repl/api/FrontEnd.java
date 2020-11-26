package ammonite.repl.api;

public abstract class FrontEnd {

  public FrontEnd() {

  }

  public static class Ammonite extends FrontEnd {
    public Ammonite() {

    }
  }

  public static Ammonite Ammonite() {
    return new Ammonite();
  }

  public static class JLineUnix extends FrontEnd {
    public JLineUnix() {

    }
  }

  public static JLineUnix JLineUnix() {
    return new JLineUnix();
  }

  public static class JLineWindows extends FrontEnd {
    public JLineWindows() {

    }
  }

  public static JLineWindows JLineWindows() {
    return new JLineWindows();
  }

}
