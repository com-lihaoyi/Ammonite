package readme

import java.io.{InputStreamReader, BufferedReader}

import ammonite.ops._
import scala.collection.mutable
import scalatags.Text.all._
object Sample{
  val cacheVersion = math.random
  def cached(key: Any)(calc: => scalatags.Text.Frag) = {
    val path = cwd/'target/'cache/(key.hashCode + cacheVersion).toString
    try {
      raw(read! path)
    } catch{ case e =>
      val newValue = calc.render
      write.over(path, newValue)
      raw(newValue)
    }
  }

  val ansiRegex = "\u001B\\[[;\\d]*."
  def ammSample(ammoniteCode: String) = cached(("ammoniteCode", ammoniteCode)){
    println("ammSample\n" + ammoniteCode)
    val ammExec = "repl/target/scala-2.11/ammonite-repl-0.4.7-SNAPSHOT-2.11.7"
    val predef = "shell/src/main/resources/example-predef.scala"

    val out = exec(Seq(ammExec, "--predef-file", predef), s"${ammoniteCode.trim}\nexit\n")
    val lines = out.lines.toSeq.drop(3).dropRight(2).mkString("\n")
//    println("ammSample " + lines)
    val ammOutput = lines.split("\u001b")
    println(ammOutput.toSeq)
    val colors = Map(
      "[30m" -> span(color:="#000"),
      "[31m" -> span(color:="#c0392b"),
      "[32m" -> span(color:="#2ecc71"),
      "[33m" -> span(color:="#f1c40f"),
      "[34m" -> span(color:="#3498db"),
      "[35m" -> span(color:="#9b59b6"),
      "[36m" -> span(color:="#1abc9c"),
      "[37m" -> span(color:="#ecf0f1")
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
  def exec(command: Seq[String], input: String): String = {
    val pb = new ProcessBuilder(command:_*)
    pb.redirectErrorStream(true)
    val p = pb.start()
    p.getOutputStream.write(input.getBytes)
    p.getOutputStream.flush()
    val buffer = new Array[Byte](4096)
    val bytes =
      Iterator.continually{
        val res = p.getInputStream.read(buffer)
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
    println("compare!")
    val out = cached(("bashCode", bashCode)){
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

    println("RUN")
    println("bashCode " + bashCode)
    println("out " + out)
    div(
      u(b("Bash")),
      pre(out),
      u(b("Scala")),
      pre(ammSample(ammoniteCode))
    )
  }
}