package ammonite.sshd.util

import acyclic.file
import java.io.{InputStream, OutputStream, PrintStream}

/**
 * Container for staging environment important for Ammonite repl to run correctly.
 * @param thread a thread where execution takes place. Important for restoring contextClassLoader
 * @param contextClassLoader thread's context class loader. Ammonite repl uses that to load classes
 * @param systemIn
 * @param systemOut
 * @param systemErr
 */
case class Environment(thread: Thread,
                       contextClassLoader: ClassLoader,
                       systemIn: InputStream,
                       systemOut: PrintStream,
                       systemErr: PrintStream
)

object Environment {
  def apply(classLoader: ClassLoader, in: InputStream, out: PrintStream): Environment =
    apply(Thread.currentThread(), classLoader, in, out, out)

  def apply(classLoader: ClassLoader, in: InputStream, out: OutputStream): Environment =
    apply(classLoader, in, new PrintStream(out))

  /**
   * Runs your code with supplied environment installed.
   * After execution of supplied code block will restore original environment
   */
  def withEnvironment(env: Environment)(code: ⇒ Any): Any = {
    val oldClassLoader = env.thread.getContextClassLoader
    try {
      env.thread.setContextClassLoader(env.contextClassLoader)
      Console.withIn(env.systemIn) {
        Console.withOut(env.systemOut) {
          Console.withErr(env.systemErr) {
            code
          }
        }
      }
    } finally {
      env.thread.setContextClassLoader(oldClassLoader)
    }
  }

}
