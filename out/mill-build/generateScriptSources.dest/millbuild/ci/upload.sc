package millbuild.ci

import _root_.mill.runner.MillBuildRootModule

@scala.annotation.nowarn
object MiscInfo_upload {
  implicit lazy val millBuildRootModuleInfo: _root_.mill.runner.MillBuildRootModule.Info = _root_.mill.runner.MillBuildRootModule.Info(
    Vector("/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/nashorn.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/localedata.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/jaccess.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/cldrdata.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/sunec.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/dnsns.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/sunpkcs11.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/sunjce_provider.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/ext/zipfs.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/resources.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/rt.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/jsse.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/jce.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/charsets.jar", "/usr/lib/jvm/temurin-8-jdk-amd64/jre/lib/jfr.jar", "/home/runner/work/Ammonite/Ammonite/out/mill-launcher/0.11.12.jar").map(_root_.os.Path(_)),
    _root_.os.Path("/home/runner/work/Ammonite/Ammonite/ci"),
    _root_.os.Path("/home/runner/work/Ammonite/Ammonite"),
  )
  implicit lazy val millBaseModuleInfo: _root_.mill.main.RootModule.Info = _root_.mill.main.RootModule.Info(
    millBuildRootModuleInfo.projectRoot,
    _root_.mill.define.Discover[upload]
  )
}
import MiscInfo_upload.{millBuildRootModuleInfo, millBaseModuleInfo}
object upload extends upload
class upload extends _root_.mill.main.RootModule.Foreign(Some(_root_.mill.define.Segments.labels("foreign-modules", "ci", "upload"))) {

//MILL_ORIGINAL_FILE_PATH=/home/runner/work/Ammonite/Ammonite/ci/upload.sc
//MILL_USER_CODE_START_MARKER

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

}