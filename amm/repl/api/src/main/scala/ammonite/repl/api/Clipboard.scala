package ammonite.repl.api

import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}

trait Clipboard{
  /**
    * Reads contents from the system clipboard.
    * @return System clipboard contents if they are readable as `String`,
    *         empty string otherwise.
    */
  def read: String
  /**
    * Sets the contents of the system clipboard.
    *
    * @param data New contents for the clipboard.
    */
  def write(data: Clipboard.Writable): Unit
}
object Clipboard {

  class Writable(val writeableData: geny.Generator[Array[Byte]])

  object Writable extends LowPri{
    implicit def WritableString(s: String) = new Writable(
      geny.Generator(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    )
    implicit def WritableBytes(a: Array[Byte]): Writable = new Writable(geny.Generator(a))

  }
  trait LowPri{

    implicit def WritableGenerator[M[_], T](a: M[T])
                                           (implicit f: T => Writable,
                                            i: M[T] => geny.Generator[T]) = {
      new Writable(
        i(a).flatMap(f(_).writeableData)
      )
    }
  }

  val clipboardImpl: Clipboard = new Clipboard {

    private lazy val systemClipboard =
      Toolkit.getDefaultToolkit.getSystemClipboard

    override def read: String =
      Option(systemClipboard.getContents(null)) collect {
        case data if data.isDataFlavorSupported(DataFlavor.stringFlavor) =>
          data.getTransferData(DataFlavor.stringFlavor)
      } match {
        case Some(str: String) => str
        case _ => ""
      }

    override def write(data: Clipboard.Writable): Unit = {
      val newContents = new StringSelection(
        data.writeableData.map(new String(_)).mkString
      )
      systemClipboard.setContents(newContents, newContents)
    }
  }
}
