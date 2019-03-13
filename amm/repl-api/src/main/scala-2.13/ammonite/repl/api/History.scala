package ammonite.repl.api

import scala.collection.generic.IsSeq
import scala.collection.{Iterable, SeqOps, mutable}


class History(s: Vector[String])
  extends IndexedSeq[String] {
  def length: Int = s.length
  def apply(idx: Int): String = s.apply(idx)

  override protected def newSpecificBuilder: mutable.Builder[String, History] =
    History.builder
}

object History {
  def builder = new mutable.Builder[String, History] {
    val buffer = mutable.Buffer.empty[String]
    def addOne(elem: String): this.type = {buffer += elem; this}

    def result(): History = new History(buffer.toVector)

    def clear(): Unit = buffer.clear()
  }
  implicit def toHistory(s: Seq[String]): History = new History(s.toVector)
}

