package ammonite.repl.eval
import acyclic.file

/**
 * Loads the jars that make up the classpath of the scala-js-fiddle
 * compiler and re-shapes it into the correct structure to satisfy
 * scala-compile and scalajs-tools
 */
object Classpath {
  /**
   * In memory cache of all the jars used in the compiler. This takes up some
   * memory but is better than reaching all over the filesystem every time we
   * want to do something.
   */

  val deps = Seq(
    "/Users/haoyi/.ivy2/cache/com.lihaoyi/ammonite_2.11/jars/ammonite_2.11-0.1.4.jar",
    "/Users/haoyi/Dropbox (Personal)/Workspace/ammonite/repl/target/scala-2.11/classes",
    "/Users/haoyi/Dropbox (Personal)/Workspace/ammonite/core/target/scala-2.11/classes",
    "/Users/haoyi/.ivy2/cache/org.scala-lang/scala-compiler/jars/scala-compiler-2.11.4.jar",
    "/Users/haoyi/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.4.jar",
    "/Users/haoyi/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.4.jar",
    "/Users/haoyi/.ivy2/cache/org.scala-lang.modules/scala-parser-combinators_2.11/bundles/scala-parser-combinators_2.11-1.0.3.jar",
    "/Users/haoyi/.ivy2/cache/com.lihaoyi/utest_2.11/jars/utest_2.11-0.2.4.jar",
    "/Users/haoyi/.ivy2/cache/com.lihaoyi/utest-runner_2.11/jars/utest-runner_2.11-0.2.4.jar",
    "/Users/haoyi/.ivy2/cache/org.scala-sbt/test-interface/jars/test-interface-1.0.jar",
    "/Users/haoyi/.ivy2/cache/jline/jline/jars/jline-2.12.jar",
    "/Users/haoyi/.ivy2/cache/com.lihaoyi/acyclic_2.11/jars/acyclic_2.11-0.1.2.jar"
  ) ++ System.getProperty("sun.boot.class.path").split(":")

//  deps.foreach(println)
//  deps.filter(x => !new java.io.File(x).exists()).foreach(println)
//  assert(nonexistent.length == 0, "\n"+nonexistent.mkString("\n"))
  val (jarDeps, dirDeps) =
    deps.map(new java.io.File(_))
        .filter(_.exists)
        .partition(_.toString.endsWith(".jar"))
}