package ammonite.interp.api;

public class AmmoniteExit extends Throwable {
  private final Object value0;
  public AmmoniteExit(Object value) {
    super();
    this.value0 = value;
  }
  public Object value() {
    return value0;
  }
}
