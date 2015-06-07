package ammonite

package object pprint {
  import acyclic.pkg
  def pprintln[T: PPrint] = (t: T) => {
    PPrint(t).foreach(print)
    println()
  }
  def pprintln[T: PPrint](t: T) = {
    PPrint(t).foreach(print)
    println()
  }
}
