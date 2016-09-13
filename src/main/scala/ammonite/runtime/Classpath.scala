// package ammonite.runtime

// import java.io.File
// import java.util.zip.ZipFile

// import scala.util.control.NonFatal

// /**
//   * Loads the jars that make up the classpath of the scala-js-fiddle
//   * compiler and re-shapes it into the correct structure to satisfy
//   * scala-compile and scalajs-tools
//   */
// object Classpath {
//   // private val traceClasspathIssues = sys.props.get("ammonite.trace-classpath").exists(_.toLowerCase == "true")

//   // private def canBeOpenedAsJar(file: File): Boolean =
//   //   try {
//   //     val zf = new ZipFile(file)
//   //     zf.close()
//   //     true
//   //   } catch {
//   //     case NonFatal(e) =>
//   //       traceClasspathProblem(
//   //         s"Classpath element '${file.getAbsolutePath}' " +
//   //           s"could not be opened as jar file because of $e"
//   //       )
//   //       false
//   //   }
//   // private def traceClasspathProblem(msg: String): Unit = if (traceClasspathIssues) println(msg)
// }
