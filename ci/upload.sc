#!/usr/bin/env amm
import ammonite.ops._
import scalaj.http._

def apply(tagName: String, uploadName: String, authKey: String): String = {
  val parsed = upickle.json.read(
    Http("https://api.github.com/repos/lihaoyi/Ammonite/releases").asString.body
  )

  val snapshotReleaseId =
    parsed.arr
      .find(_("tag_name").str == tagName)
      .get("id")
      .num.toInt


  val uploadUrl =
    s"https://uploads.github.com/repos/lihaoyi/Ammonite/releases/" +
      s"$snapshotReleaseId/assets?name=$uploadName"

  val res = Http(uploadUrl)
    .header("Content-Type", "application/octet-stream")
    .header("Authorization", "token " + authKey)
    .postData(read! cwd/"favicon.png")
    .asString

  val longUrl = upickle.json.read(res.body)("browser_download_url").str

  println("Long Url " + longUrl)

  val shortUrl = Http("https://git.io")
    .postForm(Seq("url" -> longUrl))
    .asString
    .headers("Location")
    .head

  println("Short Url " + shortUrl)
  shortUrl
}