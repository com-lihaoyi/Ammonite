// /**
//   * Miscellaneous rubbish that isn't big enough to warrant a separate file
//   */
// package ammonite.util

// import java.security.MessageDigest
// import ammonite.ops._

// import ammonite.kernel.kernel.ClassFiles

// object Util {

//   val upPathSegment = "^"
//   def pathToPackageWrapper(path: Path, wd: Path): (Seq[Name], Name) = {
//     val pkg = {
//       val base = Seq("$file")
//       val relPath = (path / up).relativeTo(wd)
//       val ups = Seq.fill(relPath.ups)(upPathSegment)
//       val rest = relPath.segments
//       (base ++ ups ++ rest).map(Name(_))
//     }
//     val wrapper = path.last.lastIndexOf('.') match {
//       case -1 => path.last
//       case i => path.last.take(i)
//     }
//     (pkg, Name(wrapper))
//   }

//   def md5Hash(data: Iterator[Array[Byte]]) = {
//     val digest = MessageDigest.getInstance("MD5")
//     data.foreach(digest.update)
//     digest.digest()
//   }

//   val windowsPlatform = System.getProperty("os.name").startsWith("Windows")
//   val newLine = System.lineSeparator()
//   // Type aliases for common things

//   type CacheDetails = (String, String)
//   //                   Wrapper HashVal
//   type IvyMap = Map[(String, String, String, String), Set[String]]
//   type CacheOutput =
//     (Seq[(String, String)], Seq[ClassFiles], Imports, Seq[ImportTree])
//   type CompileCache = (ClassFiles, Imports)

//   def transpose[A](xs: List[List[A]]): List[List[A]] = {
//     @scala.annotation.tailrec
//     def transpose(xs: List[List[A]], result: List[List[A]]): List[List[A]] = {
//       xs.filter(_.nonEmpty) match {
//         case Nil => result
//         case ys: List[List[A]] =>
//           transpose(ys.map(_.tail), ys.map(_.head) :: result)
//       }
//     }

//     transpose(xs, Nil).reverse
//   }
// }
