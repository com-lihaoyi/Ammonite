#!/usr/bin/env amm
import ammonite.ops._

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
  val body = requests.get(
    "https://api.github.com/repos/lihaoyi/Ammonite/releases/tags/" + tagName,
    headers = Seq("Authorization" -> s"token $authKey"),
  )

  val parsed = ujson.read(body.text)

  println(body)

  val snapshotReleaseId = parsed("id").num.toInt


  val uploadUrl =
    s"https://uploads.github.com/repos/lihaoyi/Ammonite/releases/" +
      s"$snapshotReleaseId/assets?name=$uploadName"

  val res = requests.post(
    uploadUrl,
    headers = Seq(
      "Content-Type" -> "application/octet-stream",
      "Authorization" -> s"token $authKey"
    ),
    connectTimeout = 5000, 
    readTimeout = 60000,
    data = os.read.bytes(uploadedFile)
  )

  println(res.text)
  val longUrl = ujson.read(res.text)("browser_download_url").toString

  longUrl
}
