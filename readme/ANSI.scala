package readme

import scala.collection.mutable
import scalatags.Text.all._

object ANSI {
 
  // http://flatuicolors.com/
  val red = "#c0392b"
  val green = "#27ae60"
  val yellow = "#f39c12"
  val blue = "#2980b9"
  val magenta = "#8e44ad"
  val cyan = "#16a085"
  val black = "#000"
  val white = "#fff"

  val foregrounds = Map[fansi.Attr, String](
    fansi.Color.Black -> black,
    fansi.Color.Red -> red,
    fansi.Color.Green-> green,
    fansi.Color.Yellow-> yellow,
    fansi.Color.Blue -> blue,
    fansi.Color.Magenta-> magenta,
    fansi.Color.Cyan -> cyan,
    fansi.Color.White -> white
  )
  val backgrounds = Map[fansi.Attr, String](
    fansi.Back.Black -> black,
    fansi.Back.Red -> red,
    fansi.Back.Green-> green,
    fansi.Back.Yellow-> yellow,
    fansi.Back.Blue -> blue,
    fansi.Back.Magenta-> magenta,
    fansi.Back.Cyan -> cyan,
    fansi.Back.White -> white
  )
  def ansiToHtml(ansiInput: String): Frag = {
    val wrapped = mutable.Buffer.empty[scalatags.Text.Frag]
    val parsed = fansi.Str(ansiInput, errorMode = fansi.ErrorMode.Strip)
    val chars = parsed.getChars
    val colors = parsed.getColors

    var i = 0
    var previousColor = 0L
    val snippetBuffer = new mutable.StringBuilder()

    def createSnippet() = {
      val foreground = fansi.Color.lookupAttr(previousColor & fansi.Color.mask)
      val background = fansi.Back.lookupAttr(previousColor & fansi.Back.mask)
      val snippet = snippetBuffer.toString
      snippetBuffer.clear()
      wrapped.append(span(
        foregrounds.get(foreground).map(color := _),
        backgrounds.get(background).map(backgroundColor := _),
        snippet
      ))
    }

    while(i < parsed.length){
      if (colors(i) != previousColor && snippetBuffer.nonEmpty) createSnippet()
      previousColor = colors(i)
      snippetBuffer += chars(i)
      i += 1
    }
    createSnippet()
    wrapped.toVector
  }
}
