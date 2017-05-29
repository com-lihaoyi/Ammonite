/**
  * Miscellaneous rubbish that isn't big enough to warrant a separate file
  */
package ammonite.util

import java.security.MessageDigest

import ammonite.ops._


object Util{

  val upPathSegment = "^"
  def pathToPackageWrapper(flexiblePkgName0: Seq[Name],
                           relPath0: RelPath): (Seq[Name], Name) = {
    var flexiblePkgName = flexiblePkgName0
    var relPath = relPath0/up
    val fileName = relPath0.last
    while(
      flexiblePkgName.length > 1 &&
      flexiblePkgName.last.encoded != upPathSegment &&
      relPath.ups > 0
    ){
      flexiblePkgName = flexiblePkgName.dropRight(1)
      relPath = relPath.copy(ups = relPath.ups - 1)
    }
    val pkg = {
      val ups = Seq.fill(relPath.ups)(upPathSegment)
      val rest = relPath.segments
      flexiblePkgName ++ (ups ++ rest).map(Name(_))
    }
    val wrapper = fileName.lastIndexOf('.') match{
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
    * Information about where a particular block of code came from; [[path]]
    * is optional because some code snippets are synthetic, which means any
    * filename is entirely synthetic and $file imports do not work in them.
    * However, there are many snippets of code, e.g. repl commands and such,
    * which have a "fake" [[path]] because we want to allow $file imports
    * relative to some path or working-directory
    */
  case class CodeSource(wrapperName: Name,
                        flexiblePkgName: Seq[Name],
                        pkgRoot: Seq[Name],
                        path: Option[Path]){
    def pkgName = pkgRoot ++ flexiblePkgName
    def fullName = pkgName :+ wrapperName

    def jvmPathPrefix = Util.encodeJvmPath(fullName)
    def filePathPrefix = Util.encodeFilePath(fullName)
    def printablePath = path match{
      case Some(x) => x.toString
      case None => "<synthetic>/" + filePathPrefix.mkString("/") + ".sc"
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