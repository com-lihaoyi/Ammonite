package ammonite.repl

import acyclic.file
import java.io.{File, FileInputStream, IOException, FileWriter}
import ammonite.ops._
import ammonite.repl.Util.{IvyMap, CompileCache, ClassFiles}

import scala.util.Try
import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate, CanBuildFrom, SeqFactory}
import scala.collection.{IterableLike, mutable, IndexedSeqLike}
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions.asScalaBuffer


/**Trait for the interface of common persistent storage. 
 * This handles history and persistent caches.
 * Right now it is not threadsafe nor does it handle the mutual exclusion of files between processes. 
 * Mutexes should be added to be able to run multiple Ammonite processes on the same system.
 */ 
trait Storage{
  def loadPredef: String
  val fullHistory: StableRef[History]
  val ivyCache: StableRef[IvyMap]
  def compileCacheSave(tag: String, data: CompileCache): Unit
  def compileCacheLoad(tag: String): Option[CompileCache]
}

object Storage{

  def apply(dir: Path): Storage = new Storage{
    val predef = dir/"predef.scala"
    val cacheDir = dir/'cache
    val compileCacheDir = cacheDir/'compile
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
        write.over(historyFile, upickle.default.write(t.toVector))
      }
    }

    def compileCacheSave(tag: String, data: CompileCache): Unit = {
      val (classFiles, imports) = data
      val tagCacheDir = compileCacheDir/tag
      if(!exists(tagCacheDir)){
        mkdir(tagCacheDir)
        val metadata = upickle.default.write(imports)
        write(tagCacheDir/metadataFile, metadata)
        classFiles.foreach{ case (name, bytes) =>
          write(tagCacheDir/s"$name.class", bytes)
        }
      }
    }

    def compileCacheLoad(tag: String): Option[CompileCache] = {
      val tagCacheDir = compileCacheDir/tag
      if(!exists(tagCacheDir)) None
      else for{
        metadataJson <- Try{read(tagCacheDir/metadataFile)}.toOption
        metadata <- Try{upickle.default.read[Seq[ImportData]](metadataJson)}.toOption
        classFiles <- loadClassFiles(tagCacheDir)
      } yield {
        (classFiles, metadata)
      }
    }

    def loadClassFiles(cacheDir: Path): Option[ClassFiles] = {
      val classFiles = ls(cacheDir).filter(_.ext == "class")
      Try{
        val data = classFiles.map{ case file =>
          val className = (file - cacheDir).toString.stripSuffix(".class")
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
        write.over(ivyCacheFile, upickle.default.write(map))
      }
    }

    def loadPredef = try{
      read(predef)
    } catch {
      case e: java.io.FileNotFoundException => ""
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

