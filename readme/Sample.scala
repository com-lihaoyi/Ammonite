package readme

import java.io.{BufferedReader, ByteArrayOutputStream, InputStreamReader}

import ammonite.ops._
import scalatags.Text.all._

object Sample{
  println("Initializing Sample")

  val loading = attr("loading")
  val loadingLazy = loading := "lazy"

  def curlCommand(curlUrl: String) =
    s"""$$ sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L """ +
    curlUrl +
    ") > /usr/local/bin/amm && chmod +x /usr/local/bin/amm' && amm"
  val replCurl = curlCommand(ammonite.Constants.curlUrl)
  val unstableCurl = curlCommand(ammonite.Constants.unstableCurlUrl)
  val cygwinSed = """$ sed -i '0,/"\$0"/{s/"\$0"/`cygpath -w "\$0"`/}' /usr/local/bin/amm"""
  val filesystemCurl =
    "$ mkdir -p ~/.ammonite && curl -L -o ~/.ammonite/predef.sc https://git.io/vHaKQ"
  
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
    val predef = "shell/src/main/resources/ammonite/shell/example-predef-bare.sc"
    val out = exec(
      Seq(
        sys.env.getOrElse("AMMONITE_ASSEMBLY", "amm"),
        "--color", "true",
        "--no-remote-logging",
        "--no-home-predef",
        "--predef", predef
      ),
      s"${ammoniteCode.trim}\nexit\n",
      args = Map("JAVA_OPTS" -> "-Xmx600m")
    )

    val lines = Predef.augmentString(out).lines.toSeq.drop(4).dropRight(2).mkString("\n")
    val rawHtmlString = ANSI.ansiToHtml(lines).render.replaceAll("\r\n|\n\r", "\n")
    raw(rawHtmlString)
  }



  def exec(command: Seq[String],
           input: String,
           args: Map[String, String] = Map.empty): String = cached(("exec", command, input, args)){

    println("====================EXECUTING====================")
    println(command)
    println(input)
    println(args)

    try {
      val p = os.proc(command)
          .call(cwd = os.pwd, env = args, stdin = input)
      val resultString = p.out.string

      println(s"====================RESULT====================")
      println(resultString)
      println("========================================")
      resultString
    } catch {
      case e: os.SubprocessException =>
        throw new RuntimeException("Non-zero exit value for subprocess: " + e.result.exitCode)
    }
  }

  def compare(bashCode: String, ammoniteCode: String) = {
    val out = {
      val customPrompt = "__bash__"
      val output = exec(
        Seq("bash", "-i"),
        s"PS1=$customPrompt\n${bashCode.trim}\nexit\n"
      )
      for(chunk <- output.split("\n" + customPrompt, -1).drop(1).dropRight(1)) yield{
        Seq[Frag](span(color := ANSI.magenta, "\nbash$ "), chunk)
      }
    }

    div(
      pre(out),
      pre(ammSample(ammoniteCode))
    )
  }
}
