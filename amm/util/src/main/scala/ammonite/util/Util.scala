/**
 * Miscellaneous rubbish that isn't big enough to warrant a separate file
 */
package ammonite.util

import java.security.MessageDigest

object Util {
  val javaPrefixes = Set("java", "jdk", "javax")
  def lookupWhiteList(whitelist: Set[Seq[String]], tokens: Seq[String]): Boolean = {
    if (whitelist.isEmpty) true
    else {
      tokens.foreach(s => assert(!s.contains('/'), s))
      javaPrefixes.contains(tokens.head) || whitelist(tokens)
    }
  }
  def withContextClassloader[T](contextClassloader: ClassLoader)(t: => T) = {
    val oldClassloader = Thread.currentThread().getContextClassLoader
    try {
      Thread.currentThread().setContextClassLoader(contextClassloader)
      t
    } finally {
      Thread.currentThread().setContextClassLoader(oldClassloader)
    }
  }
  val upPathSegment = "^"
  def pathToPackageWrapper(flexiblePkgName0: Seq[Name], relPath0: os.RelPath): (Seq[Name], Name) = {
    var flexiblePkgName = flexiblePkgName0
    var relPath = relPath0 / os.up
    val fileName = relPath0.last
    while (
      flexiblePkgName.length > 1 &&
      flexiblePkgName.last.encoded != upPathSegment &&
      relPath.ups > 0
    ) {
      flexiblePkgName = flexiblePkgName.dropRight(1)
      relPath = os.RelPath(relPath.segments, relPath.ups - 1)
    }
    val pkg = {
      val ups = Seq.fill(relPath.ups)(upPathSegment)
      val rest = relPath.segments
      flexiblePkgName ++ (ups ++ rest).map(Name(_))
    }
    val wrapper = fileName.lastIndexOf('.') match {
      case -1 => fileName
      case i => fileName.take(i)
    }

    (pkg, Name(wrapper))
  }

  def md5Hash(data: Iterator[Array[Byte]]) = {
    val digest = MessageDigest.getInstance("MD5")
    data.foreach(digest.update)
    digest.digest()
  }

  // normalizes strings to have new line of the OS program is being run on
  // irrespective of the OS on which script was written
  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)

  val windowsPlatform = System.getProperty("os.name").startsWith("Windows")
  val java9OrAbove = !System.getProperty("java.specification.version").startsWith("1.")
  val newLine = System.lineSeparator()
  // Type aliases for common things

  /**
   * Refers to a wrapper object compiled with a specific set of source code
   * and enclosing environment (encapsulated in the hashVal). This lets us
   * unambiguously look up the correct version of a class, or invalidate and
   * recompile it if the source code or environment changes
   */
  case class VersionedWrapperId(wrapperPath: String, tag: Tag)

  type ClassFiles = Vector[(String, Array[Byte])]

  /**
   * Information about where a particular block of code came from; [[path]]
   * is optional because some code snippets are synthetic, which means any
   * filename is entirely synthetic and $file imports do not work in them.
   * However, there are many snippets of code, e.g. repl commands and such,
   * which have a "fake" [[path]] because we want to allow $file imports
   * relative to some path or working-directory
   */
  case class CodeSource(
      wrapperName: Name,
      flexiblePkgName: Seq[Name],
      pkgRoot: Seq[Name],
      path: Option[os.Path]
  ) {
    // All code Ammonite compiles must be rooted in some package within
    // the `ammonite` top-level package
    assert(pkgRoot.head == Name("ammonite"))
    def pkgName = pkgRoot ++ flexiblePkgName
    def fullName = pkgName :+ wrapperName

    def fileName = path.fold(filePathPrefix.last + ".sc")(_.last)
    def jvmPathPrefix = Util.encodeJvmPath(fullName)
    def filePathPrefix = Util.encodeFilePath(fullName)
    def printablePath = path match {
      case Some(x) => x.toString
      case None => "(synthetic)/" + filePathPrefix.mkString("/") + ".sc"
    }
  }

  def encodeFilePath(path: Seq[Name]) = path.map(_.encoded)
  def encodeScalaSourcePath(path: Seq[Name]) = path.map(_.backticked).mkString(".")
  def encodeJvmPath(path: Seq[Name]) = path.map(_.encoded).mkString(".")

  def transpose[A](xs: List[List[A]]): List[List[A]] = {
    @scala.annotation.tailrec
    def transpose(xs: List[List[A]], result: List[List[A]]): List[List[A]] = {
      xs.filter(_.nonEmpty) match {
        case Nil => result
        case ys: List[List[A]] => transpose(ys.map(_.tail), ys.map(_.head) :: result)
      }
    }

    transpose(xs, Nil).reverse
  }

  case class Location(fileName: String, lineNum: Int, fileContent: String)

  def javaMajorVersion: Int = {
    val prop = System.getProperty("java.version")
    val prop0 =
      if (prop.startsWith("1.")) prop.stripPrefix("1.")
      else prop
    val idx = prop0.indexOf('.')
    val version =
      if (idx < 0) prop0
      else prop0.take(idx)
    version.toInt
  }

  /**
   * Detects if the console is interactive; lets us make console-friendly output
   * (e.g. ansi color codes) if it is, and script-friendly output (no ansi codes)
   * if it's not
   *
   * https://stackoverflow.com/a/1403817/871202
   */
  def isInteractive() = System.console() != null
}
