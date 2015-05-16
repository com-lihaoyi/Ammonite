package ammonite.pprint

/**
 * Created by haoyi on 5/15/15.
 */
object Main {
  def main(args: Array[String]): Unit = {
    import ammonite.pprint.Config.Defaults._
    PPrinter.SeqRepr[Int, Iterable]
  }
}
