package ammonite.repl

import acyclic.file
import java.io.{File, FileInputStream, IOException, FileWriter}
import ammonite.ops._
import ammonite.repl.Util.{IvyMap, CompileCache, ClassFiles}
import org.yaml.snakeyaml.Yaml
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
  val history: StableRef[History]
  val ivyCache: StableRef[IvyMap]
  def compileCacheSave(tag: String, data: CompileCache): Unit
  def compileCacheLoad(tag: String): Option[CompileCache]
}

object Storage{

  def apply(dir: Path): Storage = new Storage{
  

    val history = new StableRef[History]{
      def apply(): History = {
        val yaml = new Yaml
        try{
          val list = yaml.load(new FileInputStream(dir + "/history"))
          list match {
            case a: java.util.List[String] => new History(a.toVector)
            case _ => new History(Vector())
          }
        } catch {
          case e: IOException => new History(Vector())
        }
      }

      def update(t: History): Unit = {
        val yaml = new Yaml
        val fw = new FileWriter(dir + "/history")
        yaml.dump(t.toArray, fw)
      }
    }

    def compileCacheSave(tag: String, data: CompileCache): Unit = {
      val (classFiles, imports) = data
      val cacheDir = dir/'compileCache/tag
      if(!exists(cacheDir)){
        mkdir(cacheDir)
        val metadata = upickle.default.write(imports)
        write(cacheDir/"metadata.json", metadata)
        classFiles.foreach{ case (name, bytes) =>
          write(cacheDir/s"$name.class", bytes)
        }
      }
    }

    def compileCacheLoad(tag: String): Option[CompileCache] = {
      val cacheDir = dir/'compileCache/tag
      if(!exists(cacheDir)) None
      else for{
        metadataJson <- Try{read(cacheDir/"metadata.json")}.toOption
        metadata <- Try{upickle.default.read[Seq[ImportData]](metadataJson)}.toOption
        classFiles <- loadClassFiles(cacheDir)
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
        val json = try{
          read(dir/"ivycache.json")
        }catch{
          case e: java.io.FileNotFoundException => "[]"
        }

        try{
          upickle.default.read[IvyMap](json)
        }catch{ case e: Exception =>
          Map.empty
        }
      }
      def update(map: IvyMap) = {
        write(dir/"ivycache.json", upickle.default.write(map))
      }
    }

    def loadPredef = try{
      read(dir/"predef.scala")
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
        val lines = if(c.height > 0) c.height else t.length
        val seq = "\n" +: t.dropRight(lines).flatMap{ code => Seq("@ ", code, "\n") }
        if(t.length > lines) ("\n..." +: seq).iterator
        else seq.iterator
      }
    }
  )

}

