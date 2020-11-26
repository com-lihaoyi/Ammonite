package ammonite.interp.api;

public abstract class ScriptInvoker {

  public abstract Result invoke(Object parser, String scriptName, String[] scriptArgs);
  public abstract String helpText(Object parser, int totalWidth, boolean docsOnNewLine);


  public static abstract class Result {

    public static class Success<T> extends Result {

      private final T value;

      public Success(T value) {
        this.value = value;
      }

      public T value() {
        return value;
      }
    }

    public static class Failure extends Result {

      private final String message;

      public Failure(String message) {
        this.message = message;
      }

      public String message() {
        return this.message;
      }

    }

    public static class ExceptionThrown extends Result {

      private final Throwable exception;

      public ExceptionThrown(Throwable exception) {
        this.exception = exception;
      }

      public Throwable exception() {
        return this.exception;
      }

    }

  }

}
