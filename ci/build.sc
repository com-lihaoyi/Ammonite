#!/usr/bin/env amm
import ammonite.ops._
import ammonite.ops.ImplicitWd._
import $file.upload
println("START TIME " + new java.util.Date().toString)

val isMasterCommit =
  sys.env.get("TRAVIS_PULL_REQUEST") == Some("false") &&
  (sys.env.get("TRAVIS_BRANCH") == Some("master") || sys.env("TRAVIS_TAG") != "")


val allVersions = Seq(
  "2.10.4", "2.10.5", "2.10.6",
  "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7", "2.11.8"
)

val buildVersion =
  if (sys.env("TRAVIS_TAG") == "") s"COMMIT-${getGitHash()}"
  else sys.env("TRAVIS_TAG")


def getGitHash() = %%("git", "rev-parse", "--short", "HEAD").out.trim


def updateConstants(version: String = buildVersion,
                    unstableVersion: String = "<fill-me-in-in-Constants.scala>",
                    curlUrl: String = "<fill-me-in-in-Constants.scala>",
                    unstableCurlUrl: String = "<fill-me-in-in-Constants.scala>") = {
  val versionTxt = s"""
    package ammonite
    object Constants{
      val version = "$version"
      val unstableVersion = "$unstableVersion"
      val curlUrl = "$curlUrl"
      val unstableCurlUrl = "$unstableCurlUrl"
    }
  """
  rm! cwd/'project/"Constants.scala"
  write(cwd/'project/"Constants.scala", versionTxt)
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
    if (Set("2.10.5", "2.11.8").contains(version)) {
      %sbt("++" + version, "published/publishSigned")
    }else {
      %sbt("++" + version, "amm/publishSigned", "sshd/publishSigned")
    }
  }
  %sbt("sonatypeReleaseAll")
}



def publishDocs() = {

  val publishDocs = sys.env("DEPLOY_KEY").replace("\\n", "\n")
  write(cwd / 'deploy_key, publishDocs)

  val travisTag = sys.env("TRAVIS_TAG")

  val latestTaggedVersion = %%('git, 'describe, "--abbrev=0", "--tags").out.trim

  val (stableKey, unstableKey) =
    if (travisTag != ""){
      (
        s"$latestTaggedVersion/$latestTaggedVersion",
        s"$latestTaggedVersion/$latestTaggedVersion"
      )
    }else{
      (
        s"$latestTaggedVersion/$latestTaggedVersion",
        s"snapshot-commit-uploads/$travisTag"
      )
    }
  updateConstants(
    latestTaggedVersion,
    buildVersion,
    upload.shorten(s"https://github.com/lihaoyi/Ammonite/releases/$stableKey"),
    upload.shorten(s"https://github.com/lihaoyi/Ammonite/releases/$unstableKey")
  )

  %sbt "readme/run"

  %("ci/deploy_master_docs.sh")
}
@main
def executable() = {
  if (isMasterCommit){
    println("MASTER COMMIT: Publishing Executable")
    //Prepare executable
    updateConstants()
    %sbt "amm/test:assembly"

    val travisTag = sys.env("TRAVIS_TAG")
    val gitHash = getGitHash()
    val shortUrl = if (travisTag != ""){
      import upickle.Js
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

      val short = upload(
        cwd/'amm/'target/"scala-2.11"/'amm,
        travisTag,
        travisTag,
        sys.env("AMMONITE_BOT_AUTH_TOKEN")
      )
      short
    }else{
      val short = upload(
        cwd/'amm/'target/"scala-2.11"/'amm,
        "snapshot-commit-uploads",
        gitHash,
        sys.env("AMMONITE_BOT_AUTH_TOKEN")
      )
      short
    }
  }else{
    println("MISC COMMIT: Not Publishing Executable")
  }
}

@main
def docs() = {
  if (isMasterCommit){
    println("MASTER COMMIT: Updating version and publishing to Github Pages")

    publishDocs()
  }else{
    println("MISC COMMIT: Building readme for verification")
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
      %sbt("++" + version, "published/compile")
    }
  }

}

@main
def test(testCommand: String) = {
  %sbt("++" + sys.env("TRAVIS_SCALA_VERSION"), "published/compile")
  %sbt("++" + sys.env("TRAVIS_SCALA_VERSION"), testCommand)
}
