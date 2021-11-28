package readme

import java.io.{BufferedReader, ByteArrayOutputStream, InputStreamReader}

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
    val path = os.pwd/'target/'cache/(key.hashCode + cacheVersion).toString
    try os.read(path)
    catch { case e : Throwable =>
      val newValue = calc
      os.write.over(path, newValue, createFolders=true)
      newValue
    }
  }

  def ammSample(ammoniteCode: String) = {
    val scalaVersion = scala.util.Properties.versionNumberString
    val ammVersion = ammonite.Constants.version
    val out = exec(
      Seq(
        sys.env.getOrElse("AMMONITE_ASSEMBLY", "amm"),
        "--color", "true",
        "--no-remote-logging",
        "--no-home-predef",
      ),
      s"${ammoniteCode.trim}\nexit\n",
      args = Map("JAVA_OPTS" -> "-Xmx600m")
    )

    // drop(1) to remove "Welcome to the Ammonite Repl..."
    // dropRight(3) to remove "\nexit\n"
    val lines = Predef.augmentString(out).lines.toSeq.drop(1).dropRight(3).mkString("\n")
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

      println("====================RESULT====================")
      println(resultString)
      println("==============================================")
      resultString
    } catch {
      case e: os.SubprocessException =>
        throw new RuntimeException("Non-zero exit value for subprocess: " + e.result.exitCode)
    }
  }

  def compare(bashCode: String, ammoniteCode: String) = {
    val customPrompt = "__bash__"
    val out = exec(
      Seq("bash", "-i"),
      s"PS1=$customPrompt\n${bashCode.trim}\nexit\n"
    )

    val bashCodeFormatted = Seq[Frag](span(color := ANSI.magenta, "bash$ "), bashCode.trim)
    val bashOutput = Predef.augmentString(out).lines.toSeq.mkString("\n")

    div(
      pre(bashCodeFormatted),
      pre(bashOutput),
      pre(ammSample(ammoniteCode))
    )
  }
}
