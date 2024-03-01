#!/usr/bin/env amm

@mainargs.main
def apply(
    uploadedFile: os.Path,
    tagName: String,
    uploadName: String,
    authKey: String,
    ghOrg: String,
    ghRepo: String
): String = {
  println("upload.apply")
  println(uploadedFile)
  println(tagName)
  println(uploadName)
  println(authKey)
  println(ghOrg)
  println(ghRepo)
  val body = requests.get(
    s"https://api.github.com/repos/${ghOrg}/${ghRepo}/releases/tags/" + tagName,
    headers = Seq("Authorization" -> s"token $authKey")
  )

  val parsed = ujson.read(body.text)

  println(body)

  val snapshotReleaseId = parsed("id").num.toInt

  val uploadUrl =
    s"https://uploads.github.com/repos/${ghOrg}/${ghRepo}/releases/" +
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
  ).text

  println(res)

  val longUrl = ujson.read(res)("browser_download_url").str

  longUrl
}
