package ammonite.repl

import acyclic.file
import java.io.{File, FileInputStream, IOException, FileWriter}
import org.yaml.snakeyaml.Yaml
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions.asScalaBuffer

object Storage{
  val dir = new java.io.File(System.getProperty("user.home") + "/.ammonite")    
  if(dir.exists){
    if(!dir.isDirectory){
      dir.delete()
      dir.mkdir()
    }
  } else {
    dir.mkdir()
  }

  lazy val yaml = new Yaml

  def loadHistory: History = {
    try{
      val res = new History
      val list = yaml.synchronized{
        yaml.load(new FileInputStream(dir + "/history"))
      }
      list match {
        case a: java.util.List[String] => res ++= a
        case _ =>
      }
      res
    } catch {
      case e: IOException => new History
    }
  }

  def saveHistory(h: History): Unit = {
    val fw = new FileWriter(dir + "/history")
    yaml.synchronized{
      yaml.dump(h.toArray, fw)
    }
  }

}

class History extends ArrayBuffer[String]{
  def last(lines: Int) = {
    drop(length - lines)
  }
}


