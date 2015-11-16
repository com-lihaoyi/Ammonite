package readme

import java.io.{InputStreamReader, BufferedReader}

import ammonite.ops._
import scala.collection.mutable
import scalatags.Text.all._

object Sample{
  val replCurl = "$ curl -L -o amm http://git.io/vnnBX; chmod +x amm; ./amm"
  val filesystemCurl =
    "$ mkdir ~/.ammonite; curl -L -o ~/.ammonite/predef.scala http://git.io/vnnBy"
  val cacheVersion = 5
  def cached(key: Any)(calc: => String) = {
    val path = cwd/'target/'cache/(key.hashCode + cacheVersion).toString
    try read! path
    catch{ case e =>
      val newValue = calc
      write.over(path, newValue)
      newValue
    }
  }

  val ansiRegex = "\u001B\\[[;\\d]*."
  def ammSample(ammoniteCode: String) = {
    val scalaVersion = scala.util.Properties.versionNumberString
    val ammVersion = ammonite.Constants.version
    val executableName = s"ammonite-repl-$ammVersion-$scalaVersion"
    val ammExec = "repl/target/scala-2.11/" + executableName
    val predef = "shell/src/main/resources/ammonite/shell/example-predef-bare.scala"
    val out = exec(Seq(ammExec, "--predef-file", predef), s"${ammoniteCode.trim}\nexit\n")
    val lines = out.lines.toSeq.drop(3).dropRight(2).mkString("\n")
//    println("ammSample " + lines)
    val ammOutput = lines.split("\u001b")

    val red = "#c0392b"
    val green = "#27ae60"
    val yellow = "#f39c12"
    val blue = "#2980b9"
    val magenta = "#8e44ad"
    val cyan = "#16a085"
    val black = "#000"
    val white = "#fff"
    val colors = Map(
      "[30m" -> span(color:=black),
      "[31m" -> span(color:=red),
      "[32m" -> span(color:=green),
      "[33m" -> span(color:=yellow),
      "[34m" -> span(color:=blue),
      "[35m" -> span(color:=magenta),
      "[36m" -> span(color:=cyan),
      "[37m" -> span(color:=white)
    )
    val wrapped = for(snippet <- ammOutput) yield {
      colors.find(snippet startsWith _._1) match{
        case None => ("\u001B"+snippet).replaceAll(ansiRegex, ""): Frag
        case Some((ansiCode, wrapper)) => wrapper(snippet.drop(ansiCode.length))
      }
    }


    write.over(cwd/"temp.html", wrapped.render.replaceAll("\r\n|\n\r", "\n"))
    raw(wrapped.render.replaceAll("\r\n|\n\r", "\n"))
  }
  def exec(command: Seq[String], input: String): String = cached(("exec", command, input)){
    println("EXEC " + command + "\t" + input)
    val pb = new ProcessBuilder(command:_*)
    pb.redirectErrorStream(true)
    val p = pb.start()

    p.getOutputStream.write(input.getBytes)
    p.getOutputStream.flush()

    val buffer = new Array[Byte](4096)
    val bytes =
      Iterator.continually{
        println("reading ")
        val res = p.getInputStream.read(buffer)
        println(res)
        println(new String(buffer.take(res)))
        if(res == -1) None else Some(buffer.take(res))
      }
      .takeWhile(_ != None)
      .toArray
      .flatten
      .flatten
    p.waitFor()
    new String(bytes)
  }
  def compare(bashCode: String, ammoniteCode: String) = {
    val out = {
      val output = exec(Seq("bash", "-i"), s"\n${bashCode.trim}\nexit\n")
      output.lines
        .drop(2)
        .toVector
        .dropRight(2)
        .mkString("\n")
        .split("bash-3\\.2\\$")
        .flatMap(s => Seq[Frag](span(color:="#9b59b6", "bash$"), s))
        .drop(1)
    }

    div(
      pre(out),
      pre(ammSample(ammoniteCode))
    )
  }
}