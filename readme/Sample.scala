package readme

import java.io.{InputStreamReader, BufferedReader}

import ammonite.ops._
import scala.collection.mutable
import scalatags.Text.all._
import scalatags.Text.all._
import collection.mutable

object Sample{
  println("Initializing Sample")


  def curlCommand(curlUrl: String) =
    s"$$ sudo curl -L -o /usr/local/bin/amm " +
    curlUrl +
    " && chmod +x /usr/local/bin/amm && amm"
  val replCurl = curlCommand(ammonite.Constants.curlUrl)
  val unstableCurl = curlCommand(ammonite.Constants.unstableCurlUrl)
  val filesystemCurl =
    "$ mkdir ~/.ammonite && curl -L -o ~/.ammonite/predef.sc https://git.io/vo4wx"
  val cacheVersion = 6
  def cached(key: Any)(calc: => String) = {
    val path = cwd/'target/'cache/(key.hashCode + cacheVersion).toString
    try read! path
    catch{ case e =>
      val newValue = calc
      write.over(path, newValue)
      newValue
    }
  }


  def ammSample(ammoniteCode: String) = {
    val scalaVersion = scala.util.Properties.versionNumberString
    val ammVersion = ammonite.Constants.version
    val executableName = s"ammonite-$ammVersion-$scalaVersion"
    val ammExec = "amm/target/scala-2.11/" + executableName
    val predef = "shell/src/main/resources/ammonite/shell/example-predef-bare.sc"
    val out = exec(Seq(ammExec, "--predef-file", predef), s"${ammoniteCode.trim}\nexit\n")
//    val out = read! wd/'target/'cache/"-1080873603"
    val lines = out.lines.toSeq.drop(3).dropRight(2).mkString("\n")
//    println("ammSample " + lines)
//    val ammOutput = lines.split("\u001b")


    raw(ansiToHtml(lines).render.replaceAll("\r\n|\n\r", "\n"))
  }

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
    var previousColor = 0
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


  def exec(command: Seq[String], input: String): String = cached(("exec", command, input)){

    val pb = new ProcessBuilder(command:_*)
    pb.redirectErrorStream(true)
    val p = pb.start()

    p.getOutputStream.write(input.getBytes)
    p.getOutputStream.flush()

    val buffer = new Array[Byte](32768)
    val bytes =
      Iterator.continually{
        println("reading ")
        val res = p.getInputStream.read(buffer)
        println(res)
        println(new String(buffer.take(res)))
        if(res == -1) None else Some(buffer.take(res))
      }
      .takeWhile(_.isDefined)
      .toArray
      .flatten
      .flatten
    p.waitFor()
    new String(bytes)
  }
  def compare(bashCode: String, ammoniteCode: String) = {
    val out = {
      val output = exec(Seq("bash", "-i"), s"\n${bashCode.trim}\nexit\n")
      output.dropWhile(_ != '$').drop(1)
        .lines
        .toVector
        .dropRight(2)
        .mkString("\n")
        .flatMap(s => Seq[Frag](span(color := magenta, "bash$"), span(color := black, s)))
        .drop(1)
    }

    div(
      pre(out),
      pre(ammSample(ammoniteCode))
    )
  }
}
