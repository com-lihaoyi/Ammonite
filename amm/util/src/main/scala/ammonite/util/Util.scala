/**
  * Miscellaneous rubbish that isn't big enough to warrant a separate file
  */
package ammonite.util

import java.security.MessageDigest
import acyclic.file
import ammonite.ops._


object Util{

  val upPathSegment = "^"
  def pathToPackageWrapper(path: Path, wd: Path): (Seq[Name], Name) = {
    val pkg = {
      val base = Seq("$file")
      val relPath = (path/up).relativeTo(wd)
      val ups = Seq.fill(relPath.ups)(upPathSegment)
      val rest = relPath.segments
      (base ++ ups ++ rest).map(Name(_))
    }
    val wrapper = path.last.lastIndexOf('.') match{
      case -1 => path.last
      case i => path.last.take(i)
    }
    (pkg, Name(wrapper))
  }
  def md5Hash(data: Iterator[Array[Byte]]) = {
    val digest = MessageDigest.getInstance("MD5")
    data.foreach(digest.update)
    digest.digest()
  }

  //normalizes strings to have new line of the OS program is being run on
  //irrespective of the OS on which script was written
  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)


  val windowsPlatform = System.getProperty("os.name").startsWith("Windows")
  val newLine = System.lineSeparator()
  // Type aliases for common things

  /**
    * Refers to a wrapper object compiled with a specific set of source code
    * and enclosing environment (encapsulated in the hashVal). This lets us
    * unambiguously look up the correct version of a class, or invalidate and
    * recompile it if the source code or environment changes
    */
  case class VersionedWrapperId(wrapperPath: String, versionHash: String)



  type IvyMap = Map[(String, Seq[(String, String, String)]), Set[String]]
  type ClassFiles = Vector[(String, Array[Byte])]



  /**
    * The serialized output of running a script, including both metadata and the classfile binaries
    */
  case class ScriptOutput(processed: ScriptOutput.Metadata, classFiles: Seq[ClassFiles])
  object ScriptOutput{
    /**
      * Metadata extracted from the compilation of a single block, without the classfiles
      * but with enough information to fetch the classfiles form disk and evaluate the
      * block without compiling/parsing it
      */
    case class BlockMetadata(id: VersionedWrapperId, importHookTrees: Seq[ImportTree])
    case class Metadata(finalImports: Imports, blockInfo: Seq[BlockMetadata])
  }
  type CompileCache = (ClassFiles, Imports)

  /**
    * Information about where a particular block of code came from; [[source]]
    * is optional because some code snippets are synthetic, which means any
    * filename is entirely synthetic and $file imports do not work in them.
    * However, there are many snippets of code, e.g. repl commands and such,
    * which have a "fake" [[source]] because we want to allow $file imports
    * relative to some path or working-directory
    */
  case class CodeSource(wrapperName: Name,
                        pkgName: Seq[Name],
                        source: Option[Path]){
    def fullName = pkgName :+ wrapperName

    def jvmPathPrefix = Util.encodeJvmPath(fullName)
    def filePathPrefix = Util.encodeFilePath(fullName)
    def printablePath = source match{
      case Some(x) => x.toString
      case None => "<synthetic>/" + filePathPrefix + ".sc"
    }
  }

  def encodeFilePath(path: Seq[Name]) = path.map(_.encoded)
  def encodeScalaSourcePath(path: Seq[Name]) = path.map(_.backticked).mkString(".")
  def encodeJvmPath(path: Seq[Name]) = path.map(_.encoded).mkString(".")


  def transpose[A](xs: List[List[A]]): List[List[A]] = {
    @scala.annotation.tailrec
    def transpose(xs: List[List[A]], result: List[List[A]]): List[List[A]] = {
      xs.filter(_.nonEmpty) match {
        case Nil    =>  result
        case ys: List[List[A]] => transpose(ys.map(_.tail), ys.map(_.head) :: result)
      }
    }

    transpose(xs, Nil).reverse
  }
}