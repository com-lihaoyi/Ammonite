package ammonite.repl.api

import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}


class History(val array: Array[String])
extends IndexedSeq[String]
with IndexedSeqLike[String, History] {
  def length: Int = array.length
  def apply(idx: Int): String = array.apply(idx)
  override def newBuilder = History.builder
}

object History{
  def builder = new mutable.Builder[String, History] {
    val buffer = mutable.Buffer.empty[String]
    def +=(elem: String): this.type = {buffer += elem; this}

    def result(): History = new History(buffer.toArray)

    def clear(): Unit = buffer.clear()
  }
  implicit def cbf = new CanBuildFrom[History, String, History]{
    def apply(from: History) = builder
    def apply() = builder
  }
  implicit def toHistory(s: Seq[String]): History = new History(s.toArray)
}

