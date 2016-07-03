package ammonite.repl

import acyclic.file
import ammonite.ops._
import Parsers.ImportTree
import ammonite.repl.Util.{IvyMap, CompileCache, ClassFiles, CacheOutput}
import org.apache.ivy.plugins.resolver.RepositoryResolver

import scala.util.Try
import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, IndexedSeqLike}


/**
 * Trait for the interface of common persistent storage. This handles history
 * and persistent caches. Right now it is not threadsafe nor does it handle
 * the mutual exclusion of files between processes. Mutexes should be added
 * to be able to run multiple Ammonite processes on the same system.
 */ 
trait Storage{
  def loadPredef: String
  val fullHistory: StableRef[History]
  val ivyCache: StableRef[IvyMap]
  def compileCacheSave(path: String, tag: String, data: CompileCache): Unit
  def compileCacheLoad(path: String, tag: String): Option[CompileCache]
  def classFilesListSave(pkg: String,
                         wrapper: String,
                         dataList: Seq[(String, String)],
                         imports: Imports,
                         tag: String,
                         importTreesList: Seq[ImportTree]): Unit
  def classFilesListLoad(pkg: String,
                         wrapper: String,
                         cacheTag: String): Option[CacheOutput]
}

object Storage{
  case class InMemory() extends Storage{
    var predef = ""
    def loadPredef = predef

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

    var compileCache: mutable.Map[String, (String, CompileCache)] = mutable.Map.empty
    val classFilesListcache = {
      mutable.Map.empty[String, (String, Seq[(String, String)], Imports, Seq[ImportTree])]
    }
    def compileCacheSave(path: String, tag: String, data: CompileCache): Unit = {
      compileCache(path) = (tag, data)
    }
    def compileCacheLoad(path: String, tag: String) = {
      for {
        (loadedTag, data) <- compileCache.get(path)
        if loadedTag == tag
      } yield data
    }

    def classFilesListSave(pkg: String,
                           wrapper: String,
                           dataList: Seq[(String, String)],
                           imports: Imports,
                           tag: String,
                           importTreesList: Seq[ImportTree]): Unit = {
      val dir = pkg + "." + wrapper
      classFilesListcache(dir) = (tag, dataList.reverse, imports, importTreesList)
    }

    def classFilesListLoad(pkg: String,
                           wrapper: String,
                           cacheTag: String): Option[CacheOutput] = {
      val dir = pkg + "." + wrapper
      classFilesListcache.get(dir) match{
        case None => None
        case Some((loadedTag, classFilesList, imports, importTreesList)) =>
          if (loadedTag == cacheTag) {
            val res = {
              for((path, tag) <- classFilesList) yield {
                compileCacheLoad(path, tag)
              }
            }.flatten
            Some((classFilesList.unzip._1, res.unzip._1, imports, importTreesList))
          }
          else None
      }
    }
  }


  class Folder(val dir: Path) extends Storage{
    val predef = dir/"predef.scala"
    // Each version puts its cache in a separate folder, to bust caches
    // on every version bump; otherwise binary-incompatible changes to
    // ReplAPI/Preprocessor/ammonite-ops will cause scripts to fail after
    // someone upgrades Ammonite.
    val cacheDir = dir/'cache/ammonite.Constants.version
    val compileCacheDir = cacheDir/'compile
    val classFilesOrder = "classFilesOrder.json"
    val ivyCacheFile = cacheDir/"ivycache.json"
    val metadataFile = "metadata.json"
    val historyFile = dir/'history
    val fullHistory = new StableRef[History]{
      def apply(): History = try{
        new History(upickle.default.read[Vector[String]](read(historyFile)))
      }catch{case e: Exception =>
        new History(Vector())
      }

      def update(t: History): Unit = {
        write.over(historyFile, upickle.default.write(t.toVector, indent = 4))
      }
    }

    def compileCacheSave(path: String, tag: String, data: CompileCache): Unit = {
      val (classFiles, imports) = data
      val tagCacheDir = compileCacheDir/path.replace("/", "$div").replace(":", "$colon")
      if(!exists(tagCacheDir)){
        mkdir(tagCacheDir)
        val metadata = upickle.default.write((tag, imports), indent = 4)
        write(tagCacheDir/metadataFile, metadata)
        classFiles.foreach{ case (name, bytes) =>
          write(tagCacheDir/s"$name.class", bytes)
        }
      }
    }

    def classFilesListSave(pkg: String,
                           wrapper: String,
                           dataList: Seq[(String, String)],
                           imports: Imports,
                           tag: String,
                           importTreesList: Seq[ImportTree]): Unit = {
      val dir = pkg.replace("/", "$div") + "." + wrapper.replace("/", "$div")
      val codeCacheDir = compileCacheDir/'scriptCaches/dir
      if(!exists(codeCacheDir)) {
        mkdir(codeCacheDir)
        write(
          codeCacheDir/classFilesOrder,
          upickle.default.write((tag, dataList.reverse), indent = 4)
        )
        write(codeCacheDir/"imports.list", upickle.default.write(imports))
        write(codeCacheDir/"importTrees.json", upickle.default.write(importTreesList, indent = 4))
      }
    }

    def classFilesListLoad(pkg: String,
                           wrapper: String,
                           cacheTag: String): Option[CacheOutput] = {

      val dir = pkg.replace("/", "$div") + "." + wrapper.replace("/", "$div")
      val codeCacheDir = compileCacheDir/'scriptCaches/dir
      if(!exists(codeCacheDir)) None
      else {
        val metadataJson = Try {
          read(codeCacheDir/classFilesOrder)
        }.toOption
        val impFile = Try {
          read(codeCacheDir/"imports.list")
        }.toOption
        val impTrees = Try{
          read(codeCacheDir/"importTrees.json")
        }.toOption
        (metadataJson, impFile, impTrees) match{
          case (Some(metadata), Some(imp), Some(importTrees)) =>
            val (loadedTag, classFilesList) =
              upickle.default.read[(String, Seq[(String, String)])](metadata)
            if (cacheTag == loadedTag){
              val res = {
                for((path, tag) <- classFilesList) yield {
                  compileCacheLoad(path, tag)
                }
              }.flatten
              val imports = upickle.default.read[Imports](imp)
              val impTreeList = upickle.default.read[Seq[ImportTree]](importTrees)
              Some((classFilesList.unzip._1, res.unzip._1, imports, impTreeList))
            }
            else None
          case _ => None
        }
      }
    }

    def compileCacheLoad(path: String, tag: String): Option[CompileCache] = {
      val tagCacheDir = compileCacheDir/path.replace("/", "$div").replace(":", "$colon")
      if(!exists(tagCacheDir)) None
      else for{
        metadataJson <- Try{read(tagCacheDir/metadataFile)}.toOption
        (loadedTag, metadata) <- Try{
          upickle.default.read[(String, Imports)](metadataJson)
        }.toOption
        if tag == loadedTag
        classFiles <- loadClassFiles(tagCacheDir)
      } yield {
        (classFiles, metadata)
      }
    }

    def loadClassFiles(cacheDir: Path): Option[ClassFiles] = {
      val classFiles = ls(cacheDir).filter(_.ext == "class")
      Try{
        val data = classFiles.map{ case file =>
          val className = (file relativeTo cacheDir).toString.stripSuffix(".class")
          (className, read.bytes(file))
        }
        data
      }.toOption.map(_.toSeq)
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

    def loadPredef = try{
      read(predef)
    } catch {
      case e: java.nio.file.NoSuchFileException => ""
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

  import pprint._
  implicit val historyPPrint: pprint.PPrint[History] = pprint.PPrint(
    new pprint.PPrinter[History]{
      def render0(t: History, c: pprint.Config) = {
        t.iterator.flatMap(Iterator("\n", _))
      }
    }
  )

}

