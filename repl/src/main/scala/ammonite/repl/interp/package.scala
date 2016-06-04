package ammonite.repl

/**
  * What actually lets us compile and execute code in the Ammonite REPL; deals
  * with the Scala compiler, preprocessing the strings, JVM classloaders, etc.
  */
package object interp