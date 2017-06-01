package ammonite.runtime

import java.nio.file.FileAlreadyExistsException

import ammonite.ops._
import ammonite.util.{ImportTree, Imports, StableRef, Tag}
import ammonite.util.Util._

import scala.util.Try
import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}
import scala.reflect.NameTransformer.encode


/**
 * Trait for the interface of common persistent storage. This handles history
 * and persistent caches. Right now it is not threadsafe nor does it handle
 * the mutual exclusion of files between processes. Mutexes should be added
 * to be able to run multiple Ammonite processes on the same system.
 */
trait Storage{
  def loadPredef: (String, Option[Path])
  def loadSharedPredef: (String, Option[Path])
  val fullHistory: StableRef[History]
  val ivyCache: StableRef[Storage.IvyMap]
  def compileCacheSave(path: String, tag: Tag, data: CompileCache): Unit
  def compileCacheLoad(path: String, tag: Tag): Option[CompileCache]
  def classFilesListSave(filePathPrefix: RelPath,
                         perBlockMetadata: Seq[ScriptOutput.BlockMetadata],
                         tag: Tag): Unit
  def classFilesListLoad(filePathPrefix: RelPath, cacheTag: Tag): Option[ScriptOutput]
  def getSessionId: Long

}

object Storage{
  type IvyMap = Map[(String, Seq[coursier.Dependency]), Set[String]]
  private def loadIfTagMatches(loadedTag: Tag,
                               cacheTag: Tag,
                               classFilesList: Seq[ScriptOutput.BlockMetadata],
                               compileCacheLoad: (String, Tag) => Option[CompileCache]) = {
    if (loadedTag != cacheTag) None
    else{
      val res =
        for(blockMeta <- classFilesList)
        yield compileCacheLoad(blockMeta.id.wrapperPath, blockMeta.id.tag)

      if (res.exists(_.isEmpty)) None
      else Some(ScriptOutput(ScriptOutput.Metadata(classFilesList), res.flatten.map(_._1)))
    }

  }
  case class InMemory() extends Storage{
    var predef = ""
    var sharedPredef = ""
    def loadPredef = (predef, None)
    def loadSharedPredef = (sharedPredef, None)
    def getSessionId = 0L
    var _history = new History(Vector())
    val fullHistory = new StableRef[History]{
      def apply() = _history
      def update(h: History): Unit = _history = h
    }

    var _ivyCache: IvyMap = Map.empty
    val ivyCache = new StableRef[IvyMap]{
      def apply() = _ivyCache
      def update(value: IvyMap): Unit = _ivyCache = value
    }

    var compileCache: mutable.Map[String, (Tag, CompileCache)] = mutable.Map.empty
    val classFilesListcache = {
      mutable.Map.empty[String, (Tag, Seq[ScriptOutput.BlockMetadata])]
    }
    def compileCacheSave(path: String, tag: Tag, data: CompileCache): Unit = {
      compileCache(path) = (tag, data)
    }
    def compileCacheLoad(path: String, tag: Tag) = {
      for {
        (loadedTag, data) <- compileCache.get(path)
        if loadedTag == tag
      } yield data
    }

    def classFilesListSave(filePathPrefix: RelPath,
                           perBlockMetadata: Seq[ScriptOutput.BlockMetadata],
                           tag: Tag): Unit = {

      classFilesListcache(filePathPrefix.toString) = (tag, perBlockMetadata.reverse)
    }

    def classFilesListLoad(filePathPrefix: RelPath,
                           cacheTag: Tag): Option[ScriptOutput] = {

      classFilesListcache.get(filePathPrefix.toString) match{
        case None => None
        case Some((loadedTag, classFilesList)) =>
          loadIfTagMatches(loadedTag, cacheTag, classFilesList, compileCacheLoad)
      }
    }
  }



  class Folder(val dir: Path, isRepl: Boolean = true) extends Storage{
    def predef = if (isRepl) dir/"predef.sc" else dir/"predefScript.sc"
    def predefShared = dir/"predefShared.sc"
    // Each version puts its cache in a separate folder, to bust caches
    // on every version bump; otherwise binary-incompatible changes to
    // ReplAPI/Preprocessor/ammonite-ops will cause scripts to fail after
    // someone upgrades Ammonite.
    val cacheDir = dir/'cache/ammonite.Constants.version
    val compileCacheDir = cacheDir/'compile
    val classFilesOrder = "classFilesOrder.json"
    val ivyCacheFile = cacheDir/"ivycache.json"
    val metadataFile = "metadata.json"
    val sessionFile  = dir/"session"

    def getSessionId() = {
      try read(sessionFile).toLong
      catch{case e: Throwable =>
        val randomId = math.abs(util.Random.nextLong)
        write.over(sessionFile, randomId.toString)
        randomId
      }
    }
    val historyFile = dir/'history
    val fullHistory = new StableRef[History]{
      def apply(): History = {
        try{
          new History(upickle.default.read[Vector[String]](read(historyFile)))
        }catch{case e: Exception =>
          new History(Vector())
        }
      }

      def update(t: History): Unit = {
        write.over(historyFile, upickle.default.write(t.toVector, indent = 4))
      }
    }

    def compileCacheSave(path: String, tag: Tag, data: CompileCache): Unit = {
      val (classFiles, imports) = data
      val tagCacheDir = compileCacheDir/path.split('.').map(encode)/tag.combined

      if(!exists(tagCacheDir)){
        mkdir(tagCacheDir)
        val metadata = upickle.default.write((tag, imports), indent = 4)
        write(tagCacheDir/metadataFile, metadata)
        classFiles.foreach{ case (name, bytes) =>
          write(tagCacheDir/s"$name.class", bytes)
        }

      }
    }

    def classFilesListSave(filePathPrefix: RelPath,
                           perBlockMetadata: Seq[ScriptOutput.BlockMetadata],
                           tag: Tag): Unit = {

      val codeCacheDir = cacheDir/'scriptCaches/filePathPrefix/tag.combined
      if (!exists(codeCacheDir)){
        mkdir(codeCacheDir)
        try {
          write(
            codeCacheDir/classFilesOrder,
            upickle.default.write((tag, perBlockMetadata.reverse), indent = 4)
          )
        } catch {
          case _: FileAlreadyExistsException => // ignore
          case t: Throwable => throw t
        }
      }
    }

    def readJson[T: upickle.default.Reader](path: Path): Option[T] = {
      try {
        val fileData = {ammonite.ops.read(path)}
        val parsed = {upickle.default.read(fileData)}
        Some(parsed)
      }
      catch{ case e: Throwable => None }
    }

    def classFilesListLoad(filePathPrefix: RelPath,
                           cacheTag: Tag): Option[ScriptOutput] = {

      val codeCacheDir = cacheDir/'scriptCaches/filePathPrefix/cacheTag.combined
      if(!exists(codeCacheDir)) None
      else {

        val metadataJson = readJson[(Tag, Seq[ScriptOutput.BlockMetadata])](
          codeCacheDir/classFilesOrder
        )

        metadataJson match{
          case Some(metadata) =>
            val (loadedTag, classFilesList) = metadata


            loadIfTagMatches(loadedTag, cacheTag, classFilesList, compileCacheLoad)

          case _ => None
        }
      }
    }

    def compileCacheLoad(path: String, tag: Tag): Option[CompileCache] = {
      val tagCacheDir = compileCacheDir/path.split('.').map(encode)/tag.combined
      if(!exists(tagCacheDir)) None
      else for{
        (loadedTag, metadata) <- readJson[(Tag, Imports)](tagCacheDir/metadataFile)

        if tag == loadedTag
        classFiles <- loadClassFiles(tagCacheDir)
      } yield {
        (classFiles, metadata)
      }
    }

    def loadClassFiles(cacheDir: Path): Option[ClassFiles] = {
      val classFiles = ls(cacheDir).filter(_.ext == "class").toVector
      Try{
        val data = classFiles.map{ case file =>
          val className = (file relativeTo cacheDir).toString.stripSuffix(".class")
          (className, read.bytes(file))
        }
        data
      }.toOption
    }


    val ivyCache = new StableRef[IvyMap]{
      def apply() = {
        val json =
          try read(ivyCacheFile)
          catch{ case e: java.nio.file.NoSuchFileException => "[]" }

        try upickle.default.read[IvyMap](json)
        catch{ case e: Exception => Map.empty }
      }
      def update(map: IvyMap) = {
        write.over(ivyCacheFile, upickle.default.write(map, indent = 4))
      }
    }

    def loadPredef = {
      try {
        (read(predef), Some(predef))
      }
      catch { case e: java.nio.file.NoSuchFileException => ("", None) }
    }
    def loadSharedPredef = {
      try {
        (read(predefShared), Some(predefShared))
      }
      catch {case e: java.nio.file.NoSuchFileException => ("", None)}
    }
  }
}

class History(s: Vector[String])
extends IndexedSeq[String]
with IndexedSeqLike[String, History] {
  def length: Int = s.length
  def apply(idx: Int): String = s.apply(idx)
  override def newBuilder = History.builder
}

object History{
  def builder = new mutable.Builder[String, History] {
    val buffer = mutable.Buffer.empty[String]
    def +=(elem: String): this.type = {buffer += elem; this}

    def result(): History = new History(buffer.toVector)

    def clear(): Unit = buffer.clear()
  }
  implicit def cbf = new CanBuildFrom[History, String, History]{
    def apply(from: History) = builder
    def apply() = builder
  }
  implicit def toHistory(s: Seq[String]): History = new History(s.toVector)
}

