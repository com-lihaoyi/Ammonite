package ammonite.repl

import acyclic.file
import java.io.{File, FileInputStream, IOException, FileWriter}
import ammonite.repl.Util.{IvyMap, CompileCache}
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

  def apply(dir: File): Storage = new Storage{
  
    if(dir.exists){
      if(!dir.isDirectory){
        dir.delete()
        dir.mkdir()
      }
    } else {
      dir.mkdir()
    }

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
      val cacheDir = new File(dir + s"/compileCache/$tag")
      if(!cacheDir.exists){
        cacheDir.mkdirs()
        val metadata = upickle.default.write(imports)
        writeFile(cacheDir + "/metadata.json", metadata.getBytes)
        classFiles.foreach{ case (name, bytes) =>
          writeFile(cacheDir + s"/$name.class", bytes)
        }
      }
    }

    def compileCacheLoad(tag: String): Option[CompileCache] = {
      val cacheDir = new File(dir + s"/compileCache/$tag")
      if(!cacheDir.exists) None
      else for{
        metadataJson <- Try{
          io.Source.fromFile(cacheDir + "/metadata.json").mkString
        }.toOption
        metadata <- Try{
          upickle.default.read[Seq[ImportData]](metadataJson)
        }.toOption
        classFiles <- loadClassFiles(cacheDir)
      } yield (classFiles, metadata)
    }

    def loadClassFiles(cacheDir: File): Option[Traversable[(String, Array[Byte])]] = {
      val classFiles = cacheDir.listFiles().filter(_.getName.endsWith(".class"))
      Try{
        val data = classFiles.map{ file =>
          val className = file.getName.replaceAll("\\.class$","")
          (className, readFile(file))
        }
        data
      }.toOption.map(_.toSeq)
    }

    val ivyCache = new StableRef[IvyMap]{
      def apply() = {
        val json = try{
          new String(readFile(dir + "/ivycache.json"))
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
        writeFile(dir + "/ivycache.json", upickle.default.write(map).getBytes)
      }
    }

    def loadPredef = try{
      new String(readFile(dir + "/predef.scala"))
    } catch {
      case e: java.io.FileNotFoundException => ""
    }

    def writeFile(filename: String, data: Array[Byte]): Unit = {
      val fos = new java.io.FileOutputStream(filename)
      fos.write(data)
      fos.flush()
    }

    def readFile(filename: String): Array[Byte] = {
      val file = new File(filename)
      readFile(file)
    }

    def readFile(file: File): Array[Byte] = {
      val fis = new java.io.FileInputStream(file)
      val bytes = new Array[Byte](file.length.toInt)
      var c = 0
      while(c < bytes.length) c += fis.read(bytes, c, bytes.length-c)
      bytes
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

