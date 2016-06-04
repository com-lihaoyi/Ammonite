package ammonite.repl.interp

import java.net.{URL, URLClassLoader}
import java.nio.file
import java.security.MessageDigest

import ammonite.ops._
import ammonite.repl.{ImportData, Util}
import scala.collection.mutable

object Frame{
  /**
    * Figure out which imports will get stomped over by future imports
    * before they get used, and just ignore those.
    */
  def mergeImports(importss: Seq[ImportData]*) = {
    // We iterate over the combined reversed imports, keeping track of the
    // things that will-be-stomped-over-in-the-non-reversed-world in a map.
    // If an import's target destination will get stomped over we ignore it
    //
    // At the end of the day we re-reverse the trimmed list and return it.
    val importData = importss.flatten
    val stompedTypes = mutable.Set.empty[String]
    val stompedTerms = mutable.Set.empty[String]
    val out = mutable.Buffer.empty[ImportData]
    for(data <- importData.reverseIterator){
      val stomped = data.importType match{
        case ImportData.Term => Seq(stompedTerms)
        case ImportData.Type => Seq(stompedTypes)
        case ImportData.TermType => Seq(stompedTerms, stompedTypes)
      }
      if (!stomped.exists(_(data.toName))){
        out.append(data)
        stomped.foreach(_.add(data.toName))
        stompedTerms.remove(data.prefix)
      }
    }
    out.reverse
  }
}


/**
  * Represents a single "frame" of the `sess.save`/`sess.load` stack/tree.
  *
  * Exposes `imports` and `classpath` as readable but only writable
  * in particular ways: `imports` can only be updated via `mergeImports`,
  * while `classpath` can only be added to.
  */
class Frame(val classloader: SpecialClassLoader,
            val pluginClassloader: SpecialClassLoader,
            private[this] var imports0: Seq[ImportData],
            private[this] var classpath0: Seq[java.io.File]){
  def imports = imports0
  def classpath = classpath0
  def addImports(additional: Seq[ImportData]) = {
    imports0 = Frame.mergeImports(imports0, additional)
  }
  def addClasspath(additional: Seq[java.io.File]) = {
    classpath0 = classpath0 ++ additional
  }
}

object SpecialClassLoader{
  val simpleNameRegex = "[a-zA-Z0-9_]+".r

  /**
    * Stats all loose class-files in the current classpath that could
    * conceivably be part of some package, i.e. their directory path
    * doesn't contain any non-package-identifier segments, and aggregates
    * their names and mtimes as a "signature" of the current classpath
    */
  def initialClasspathHash(classloader: ClassLoader): Array[Byte] = {
    val allClassloaders = {
      val all = mutable.Buffer.empty[ClassLoader]
      var current = classloader
      while(current != null){
        all.append(current)
        current = current.getParent
      }
      all
    }

    def findMtimes(d: file.Path): Seq[(Path, Long)] = {
      def skipSuspicious(path: Path) = {
        simpleNameRegex.findPrefixOf(path.last) == Some(path.last)
      }
      ls.rec(skip = skipSuspicious)! Path(d) | (x => (x, x.mtime.toMillis))
    }

    val classpathFolders =
      allClassloaders.collect{case cl: java.net.URLClassLoader => cl.getURLs}
                     .flatten
                     .filter(_.getProtocol == "file")
                     .map(_.toURI)
                     .map(java.nio.file.Paths.get)
                     .filter(java.nio.file.Files.isDirectory(_))

    val classFileMtimes = classpathFolders.flatMap(f => findMtimes(f))

    val hashes = classFileMtimes.map{ case (name, mtime) =>
      Util.md5Hash(Iterator(
        name.toString.getBytes,
        (0 until 64 by 8).iterator.map(mtime >> _).map(_.toByte).toArray
      ))
    }
    Util.md5Hash(hashes.iterator)
  }
}
/**
  * Classloader used to implement the jar-downloading
  * command-evaluating logic in Ammonite.
  *
  * http://stackoverflow.com/questions/3544614/how-is-the-control-flow-to-findclass-of
  */
class SpecialClassLoader(parent: ClassLoader, parentHash: Array[Byte])
  extends URLClassLoader(Array(), parent){

  /**
    * Files which have been compiled, stored so that our special
    * classloader can get at them.
    */
  val newFileDict = mutable.Map.empty[String, Array[Byte]]
  def findClassPublic(name: String) = findClass(name)
  val specialLocalClasses = Set(
    "ammonite.repl.frontend.ReplBridge",
    "ammonite.repl.frontend.ReplBridge$"
  )
  override def findClass(name: String): Class[_] = {
    pprint.log(name)
    def loadedFromBytes =
      for(bytes <- newFileDict.get(name))
        yield defineClass(name, bytes, 0, bytes.length)

    def special =
      if (!specialLocalClasses(name)) None
      else{
        import ammonite.ops._
        //          println("Custom finding class! " + name)
        val bytes = read.bytes(
          this.getResourceAsStream(name.replace('.', '/') + ".class")
        )

        Some(defineClass(name, bytes, 0, bytes.length))
      }

    Option(this.findLoadedClass(name))
      .orElse(loadedFromBytes)
      .orElse(special)
      .getOrElse(super.findClass(name))
  }
  def add(url: URL) = {
    _classpathHash = Util.md5Hash(Iterator(_classpathHash, jarHash(url)))
    addURL(url)
  }

  private def jarHash(url: URL) = {
    val digest = MessageDigest.getInstance("MD5")
    val is = url.openStream
    try {
      val byteChunk = new Array[Byte](8192)
      while({
        val n = is.read(byteChunk)
        if (n <= 0) false
        else {
          digest.update(byteChunk, 0, n)
          true
        }
      })()
    } finally {
      if (is != null) is.close()
    }

    digest.digest()
  }

  def initialClasspathHash = parentHash
  private[this] var _classpathHash = initialClasspathHash
  def classpathHash: Array[Byte] = _classpathHash
  def allJars: Seq[URL] = {
    this.getURLs ++ ( parent match{
      case t: SpecialClassLoader => t.allJars
      case _ => Nil
    })
  }
}
