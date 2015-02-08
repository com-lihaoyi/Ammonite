package ammonite.sh

import acyclic.file
case class Signaller(sigStr: String)(f: => Unit) extends Scoped{
  import sun.misc.{Signal, SignalHandler}
  var oldSigInt = List.empty[SignalHandler]
  def handlers = {
    val handlersField = classOf[Signal].getDeclaredField("handlers")
    handlersField.setAccessible(true)
    handlersField.get(null)
      .asInstanceOf[java.util.Hashtable[Signal, SignalHandler]]
  }
  val x = util.Random.nextInt()
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

trait Scoped{
  def apply[T](t: => T): T
  def foreach[T](t: Unit => T): T = apply(t(()))
  def flatMap[T, M[_]](t: Unit => M[T]): M[T] = apply(t(()))
  def map[T](t: Unit => T): T = apply(t(()))
}