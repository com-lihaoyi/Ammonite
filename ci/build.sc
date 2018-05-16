import ammonite.ops._
import ammonite.ops.ImplicitWd._
import $file.upload
println("START TIME " + new java.util.Date().toString)

val isMasterCommit =
  sys.env.get("TRAVIS_PULL_REQUEST") == Some("false") &&
  (sys.env.get("TRAVIS_BRANCH") == Some("master") || sys.env("TRAVIS_TAG") != "")

val latestTaggedVersion = %%('git, 'describe, "--abbrev=0", "--tags").out.trim

val commitsSinceTaggedVersion = {
  %%('git, "rev-list", 'master, "--count").out.trim.toInt -
  %%('git, "rev-list", latestTaggedVersion, "--count").out.trim.toInt
}

val allVersions = Seq(
  "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7", "2.11.8", "2.11.12",
  "2.12.0", "2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.6"
)

val latestMajorVersions = Set("2.11.12", "2.12.6")

val (buildVersion, unstable) = sys.env.get("TRAVIS_TAG") match{
  case Some(v) if v != "" => (v, false)
  case _ =>  (s"$latestTaggedVersion-$commitsSinceTaggedVersion-${getGitHash()}", true)
}

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
  rm! pwd/'project/"Constants.scala"
  write(pwd/'project/"Constants.scala", versionTxt)
  println(read! pwd/'project/"Constants.scala")
}


def publishDocs() = {

  val publishDocs = sys.env("DEPLOY_KEY").replace("\\n", "\n")
  write(pwd / 'deploy_key, publishDocs)



  val (stableKey, unstableKey, oldStableKeys, oldUnstableKeys) =
    if (!unstable){
      (
        s"$latestTaggedVersion/2.12-$latestTaggedVersion",
        s"$latestTaggedVersion/2.12-$latestTaggedVersion",
        for(v <- Seq("2.11"))
        yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
        for(v <- Seq("2.11"))
        yield s"$latestTaggedVersion/$v-$latestTaggedVersion"
      )
    }else{
      (
        s"$latestTaggedVersion/2.12-$latestTaggedVersion",
        s"$latestTaggedVersion/2.12-$buildVersion",
        for(v <- Seq("2.11"))
        yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
        for(v <- Seq("2.11"))
        yield s"$latestTaggedVersion/$v-$buildVersion"
      )
    }
  println("(stableKey, unstableKey)")
  println((stableKey, unstableKey))
  updateConstants(
    latestTaggedVersion,
    buildVersion,
    s"https://github.com/lihaoyi/Ammonite/releases/download/$stableKey",
    s"https://github.com/lihaoyi/Ammonite/releases/download/$unstableKey",
    for(k <- oldStableKeys)
    yield (k, s"https://github.com/lihaoyi/Ammonite/releases/download/$k"),
    for(k <- oldUnstableKeys)
    yield (k, s"https://github.com/lihaoyi/Ammonite/releases/download/$k")
  )

  %sbt "readme/compile"
  %sbt "readme/run"

  %("ci/deploy_master_docs.sh")
}

@main
def publishExecutable(ammoniteVersion: String,
                      publishKey: String,
                      unstable: Boolean) = {


  updateConstants(ammoniteVersion)

  println("MASTER COMMIT: Creating a release")
  import ujson.Js
  if (!unstable){
    scalaj.http.Http("https://api.github.com/repos/lihaoyi/Ammonite/releases")
      .postData(
        ujson.write(
          Js.Obj(
            "tag_name" -> ammoniteVersion,
            "name" -> ammoniteVersion,
            "body" -> s"http://www.lihaoyi.com/Ammonite/#$ammoniteVersion"
          )
        )
      )
      .header("Authorization", "token " + publishKey)
      .asString
  }

  for (version <- latestMajorVersions) {
    println("MASTER COMMIT: Publishing Executable for Scala " + version)
    //Prepare executable

    val assemblyMetadata = ujson.read(%%("mill", "show", s"amm[$version].assembly").out.string)
    val bv = binVersion(version)
    upload(
      Path(assemblyMetadata("path").str),
      latestTaggedVersion,
      s"$bv-$ammoniteVersion",
      publishKey
    )
  }
}
@main
def executable() = {
  if (isMasterCommit){
    publishExecutable(
      ammoniteVersion = buildVersion,
      publishKey = sys.env("AMMONITE_BOT_AUTH_TOKEN"),
      unstable
    )
  }else{
    println("MISC COMMIT: generating executable but not publishing")
    for (version <- latestMajorVersions) %("mill", s"amm[$version].assembly")
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
def sonatypeReleaseAll() = {
  if (isMasterCommit) {
    write(pwd/"gpg_key", sys.env("SONATYPE_PGP_KEY_CONTENTS").replace("\\n", "\n"))
    %("gpg", "--import", "gpg_key")
    rm(pwd/"gpg_key")

    %(
      "mill", "mill.scalalib.PublishModule/publishAll",
      sys.env("SONATYPE_DEPLOY_USER") + ":" + sys.env("SONATYPE_DEPLOY_PASSWORD"),
      sys.env("SONATYPE_PGP_PASSWORD"),
      "foo.publishArtifacts",
      "--release", "true"
    )
  }
}
