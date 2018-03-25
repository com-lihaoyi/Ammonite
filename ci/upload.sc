#!/usr/bin/env amm
import ammonite.ops._
import scalaj.http._

@main
def apply(uploadedFile: Path,
          tagName: String,
          uploadName: String,
          authKey: String): String = {
  println("upload.apply")
  println(uploadedFile)
  println(tagName)
  println(uploadName)
  println(authKey)
  val body = Http("https://api.github.com/repos/lihaoyi/Ammonite/releases/tags/" + tagName)
    .header("Authorization", "token " + authKey)
    .asString.body

  val parsed = upickle.json.read(body)

  println(body)

  val snapshotReleaseId = parsed("id").num.toInt


  val uploadUrl =
    s"https://uploads.github.com/repos/lihaoyi/Ammonite/releases/" +
      s"$snapshotReleaseId/assets?name=$uploadName"

  val res = Http(uploadUrl)
    .header("Content-Type", "application/octet-stream")
    .header("Authorization", "token " + authKey)
    .timeout(connTimeoutMs = 5000, readTimeoutMs = 60000)
    .postData(read.bytes! uploadedFile)
    .asString

  println(res.body)
  val longUrl = upickle.json.read(res.body)("browser_download_url").toString

  longUrl
}