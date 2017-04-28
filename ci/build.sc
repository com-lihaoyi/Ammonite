import ammonite.ops._
import ammonite.ops.ImplicitWd._
import $file.upload
println("START TIME " + new java.util.Date().toString)

val isMasterCommit =
  sys.env.get("TRAVIS_PULL_REQUEST") == Some("false") &&
  (sys.env.get("TRAVIS_BRANCH") == Some("master") || sys.env("TRAVIS_TAG") != "")


val allVersions = Seq(
  "2.10.4", "2.10.5", "2.10.6",
  "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7", "2.11.8", "2.11.11",
  "2.12.0", "2.12.1", "2.12.2"
)

val latestVersions = Set("2.10.6", "2.11.11", "2.12.2")

val buildVersion =
  if (sys.env("TRAVIS_TAG") == "") s"COMMIT-${getGitHash()}"
  else sys.env("TRAVIS_TAG")


def getGitHash() = %%("git", "rev-parse", "--short", "HEAD").out.trim

def binVersion(v: String) = v.take(v.lastIndexOf("."))

def updateConstants(version: String = buildVersion,
                    unstableVersion: String = "<fill-me-in-in-Constants.scala>",
                    curlUrl: String = "<fill-me-in-in-Constants.scala>",
                    unstableCurlUrl: String = "<fill-me-in-in-Constants.scala>",
                    oldCurlUrls: Seq[(String, String)] = Nil,
                    oldUnstableCurlUrls: Seq[(String, String)] = Nil) = {
  val versionTxt = s"""
    package ammonite
    object Constants{
      val version = "$version"
      val unstableVersion = "$unstableVersion"
      val curlUrl = "$curlUrl"
      val unstableCurlUrl = "$unstableCurlUrl"
      val oldCurlUrls = Seq[(String, String)](
        ${oldCurlUrls.map{case (name, value) => s""" "$name" -> "$value" """}.mkString(",\n")}
      )
      val oldUnstableCurlUrls = Seq[(String, String)](
        ${oldUnstableCurlUrls.map{case (name, value) => s""" "$name" -> "$value" """}.mkString(",\n")}
      )
    }
  """
  println("Writing Constants.scala")
  rm! cwd/'project/"Constants.scala"
  write(cwd/'project/"Constants.scala", versionTxt)
  println(read! cwd/'project/"Constants.scala")
}

def publishSigned() = {
  val creds = s"""
    (credentials in ThisBuild) += Credentials("Sonatype Nexus Repository Manager",
        "oss.sonatype.org",
        "${sys.env("SONATYPE_DEPLOY_USER")}",
        "${sys.env("SONATYPE_DEPLOY_PASSWORD")}"
    )
    pgpPassphrase := Some("${sys.env("SONATYPE_PGP_PASSWORD")}".toArray)
    pgpSecretRing := file("secring.asc")
    pgpPublicRing := file("pubring.asc")
    sonatypeProfileName := "com.lihaoyi"
  """
  write(cwd/"sonatype.sbt", creds)
  write(cwd/"secring.asc", sys.env("SONATYPE_PGP_KEY_CONTENTS").replace("\\n", "\n"))
  write(cwd/"pubring.asc", sys.env("SONATYPE_PGP_PUB_KEY_CONTENTS").replace("\\n", "\n"))


  for (version <- allVersions) {
    %sbt("++" + version, "published/publishSigned")
  }
  %sbt("sonatypeReleaseAll")
}



def publishDocs() = {

  val publishDocs = sys.env("DEPLOY_KEY").replace("\\n", "\n")
  write(cwd / 'deploy_key, publishDocs)

  val gitHash = getGitHash()

  val travisTag = sys.env("TRAVIS_TAG")

  val latestTaggedVersion = %%('git, 'describe, "--abbrev=0", "--tags").out.trim

  val (stableKey, unstableKey, oldStableKeys, oldUnstableKeys) =
    if (travisTag != ""){
      (
        s"$latestTaggedVersion/2.12-$latestTaggedVersion",
        s"$latestTaggedVersion/2.12-$latestTaggedVersion",
        for(v <- Seq("2.10", "2.11"))
        yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
        for(v <- Seq("2.10", "2.11"))
        yield s"$latestTaggedVersion/$v-$latestTaggedVersion"
      )
    }else{
      (
        s"$latestTaggedVersion/2.12-$latestTaggedVersion",
        s"snapshot-commit-uploads/2.12-$gitHash",
        for(v <- Seq("2.10", "2.11"))
        yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
        for(v <- Seq("2.10", "2.11"))
        yield s"snapshot-commit-uploads/$v-$gitHash"
      )
    }
  println("(stableKey, unstableKey)")
  println((stableKey, unstableKey))
  updateConstants(
    latestTaggedVersion,
    buildVersion,
    upload.shorten(s"https://github.com/lihaoyi/Ammonite/releases/download/$stableKey"),
    upload.shorten(s"https://github.com/lihaoyi/Ammonite/releases/download/$unstableKey"),
    for(k <- oldStableKeys)
    yield (k, upload.shorten(s"https://github.com/lihaoyi/Ammonite/releases/download/$k")),
    for(k <- oldUnstableKeys)
    yield (k, upload.shorten(s"https://github.com/lihaoyi/Ammonite/releases/download/$k"))
  )

  %sbt "readme/compile"
  %sbt "readme/run"

  %("ci/deploy_master_docs.sh")
}
@main
def executable() = {
  if (isMasterCommit){
    updateConstants()
    val travisTag = sys.env("TRAVIS_TAG")
    val gitHash = getGitHash()

    println("MASTER COMMIT: Creating a release")
    import upickle.Js
    if (travisTag != ""){
      scalaj.http.Http("https://api.github.com/repos/lihaoyi/Ammonite/releases")
        .postData(
          upickle.json.write(
            Js.Obj(
              "tag_name" -> Js.Str(travisTag),
              "name" -> Js.Str(travisTag),
              "body" -> Js.Str("http://www.lihaoyi.com/Ammonite/#" + travisTag)
            )
          )
        )
        .header("Authorization", "token " + sys.env("AMMONITE_BOT_AUTH_TOKEN"))
        .asString
    }

    for (version <- latestVersions) {

      println("MASTER COMMIT: Publishing Executable for Scala " + version)
      //Prepare executable
      %sbt("++" + version, "amm/test:assembly")
      val bv = binVersion(version)
      val shortUrl = if (travisTag != ""){
        val short = upload(
          cwd/'amm/'target/s"scala-$bv"/'amm,
          travisTag,
          s"$bv-$travisTag",
          sys.env("AMMONITE_BOT_AUTH_TOKEN")
        )
        short
      }else{
        val short = upload(
          cwd/'amm/'target/s"scala-$bv"/'amm,
          "snapshot-commit-uploads",
          s"$bv-$gitHash",
          sys.env("AMMONITE_BOT_AUTH_TOKEN")
        )
        short
      }
    }
  }else{
    println("MISC COMMIT: generating executable but not publishing")
    for (version <- latestVersions) {
      %sbt("++" + version, "published/test:compile")
      %sbt("++" + version, "integration/test:compile")
      %sbt("++" + version, "amm/test:assembly")
    }
  }
}

@main
def docs() = {
  // Disable doc auto-publishing for now, as the recent modularization means we
  // need to make significant changes to the readme and that'll time.
  if (isMasterCommit){
    println("MASTER COMMIT: Updating version and publishing to Github Pages")

    publishDocs()
  }else{
    println("MISC COMMIT: Building readme for verification")
    %sbt "readme/compile"
    %sbt "readme/run"
  }
}

@main
def artifacts() = {
  if (isMasterCommit){
    println("MASTER COMMIT: Updating version and publishing to Maven Central")
    updateConstants()
    publishSigned()
  }else{
    println("MISC COMMIT: Compiling all Scala code across versions for verification")
    for (version <- allVersions) {
      %sbt("++" + version, "published/package")
      %sbt("++" + version, "published/packageSrc")
    }
  }

}

@main
def test(testCommand: String) = {
  %sbt("++" + sys.env("TRAVIS_SCALA_VERSION"), "published/compile")
  %sbt("++" + sys.env("TRAVIS_SCALA_VERSION"), testCommand)
}
