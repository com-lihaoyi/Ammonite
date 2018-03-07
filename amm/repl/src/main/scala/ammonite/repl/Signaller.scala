package ammonite.repl

import sun.misc.{Signal, SignalHandler}

object Signaller {
  val handlers = scala.collection.mutable.Map[Signal, List[SignalHandler]]()
}

/**
 * Lets you turn on signal handling within a certain block,
 * attaching a callback to the handler and then turning it
 * properly off again when the block exits. Does sketchy
 * `unsafe` stuff because that's the only way you can make
 * it properly reset when you're finished.
 */
case class Signaller(sigStr: String)(f: => Unit) extends Scoped{
  import Signaller._

  def apply[T](t: => T): T = {
    val handler = new SignalHandler () {
      def handle(sig: Signal) = f
    }

    val sig = new Signal(sigStr)

    handlers(sig) = sun.misc.Signal.handle(sig, handler) :: handlers.getOrElse(sig, List())

    try t
    finally{
      val head::tail = handlers(sig)
      handlers(sig) = tail
      sun.misc.Signal.handle(sig, head)
    }
  }
}

/**
 * Converts something with a scoped `apply` method into
 * something which can be similarly used in a for-comprehension
 */
trait Scoped{
  def apply[T](t: => T): T
  def foreach[T](t: Unit => T): T = apply(t(()))
  def flatMap[T, M[_]](t: Unit => M[T]): M[T] = apply(t(()))
  def map[T](t: Unit => T): T = apply(t(()))
}