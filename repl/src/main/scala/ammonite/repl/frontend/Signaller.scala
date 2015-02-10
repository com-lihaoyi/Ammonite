package ammonite.repl.frontend

/**
 * Lets you turn on signal handling within a certain block,
 * attaching a callback to the handler and then turning it
 * properly off again when the block exits. Does sketchy
 * `unsafe` stuff because that's the only way you can make
 * it properly reset when you're finished.
 */
case class Signaller(sigStr: String)(f: => Unit) extends Scoped{
  import sun.misc.{Signal, SignalHandler}
  var oldSigInt = List.empty[SignalHandler]
  def handlers = {
    val handlersField = classOf[Signal].getDeclaredField("handlers")
    handlersField.setAccessible(true)
    handlersField.get(null)
      .asInstanceOf[java.util.Hashtable[Signal, SignalHandler]]
  }

  def apply[T](t: => T): T = {

    val handler = new SignalHandler () {
      def handle(sig: Signal) = f
    }
    val sig = new Signal(sigStr)
    oldSigInt = handlers.get(sig) :: oldSigInt
    sun.misc.Signal.handle(sig, handler)
    try t
    finally{
      handlers.put(sig, oldSigInt.head)
      oldSigInt = oldSigInt.tail
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