package ammonite.sh2

import acyclic.file
object Signaller{
  import sun.misc.{Signal, SignalHandler}
  var oldSigInt = List.empty[SignalHandler]
  def handlers = {
    val handlersField = classOf[Signal].getDeclaredField("handlers")
    handlersField.setAccessible(true)
    handlersField.get(null)
      .asInstanceOf[java.util.Hashtable[Signal, SignalHandler]]
  }

  def apply[T](sigStr: String, f: => Unit)(t: => T): T = {
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

