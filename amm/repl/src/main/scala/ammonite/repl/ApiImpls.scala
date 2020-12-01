package ammonite.repl

import ammonite.repl.api.Session
import ammonite.runtime._
import ammonite.util.Util._
import ammonite.util._

import scala.collection.mutable

class SessionApiImpl(frames0: => StableRef[List[Frame]]) extends Session{
  def frames: Array[ammonite.compiler.iface.Frame] = frames0().toArray
  val namedFrames = mutable.Map.empty[String, List[Frame]]

  def childFrame(parent: Frame) =
    Frame.childFrame(parent)

  def save(name: String = "") = {
    if (name != "") namedFrames(name) = frames0()
    // freezing the frame will trigger the creation of a new one later on,
    // so that the saved one won't change any more
    frames0().head.freeze()
  }

  def pop(num: Int = 1) = {
    var next = frames0()
    for(i <- 0 until num){
      if (next.tail != Nil) next = next.tail
    }
    val out = SessionChanged.delta(frames0().head, next.head)
    // freezing the current frame, so that the result of the current command,
    // that tangles with sessions, isn't added to it
    next.head.freeze()
    frames0() = next
    out
  }
  
  def load(name: String = "") = {
    val next = if (name == "") frames0().tail else namedFrames(name)
    val out = SessionChanged.delta(frames0().head, next.head)
    // freezing the current frame, so that the result of the current command,
    // that tangles with sessions, isn't added to it
    next.head.freeze()
    frames0() = next
    out
  }

  def delete(name: String) = {
    namedFrames.remove(name)
  }
}
