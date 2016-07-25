#!/usr/bin/env amm
// HttpApi.sc
import scalaj.http._
/**
  * Github URL shortener script using Scalaj-HTTP and the git.io API.
  */
@main
def shorten(longUrl: String) = {
  val res = Http("https://git.io")
    .postForm(Seq("url" -> longUrl))
    .asString
    .headers("Location")
    .head
  println(res)
  res
}

/**
  * Github URL shortener script using Scalaj-HTTP and the git.io API.
  */
@main
def listReleases(project: String) = {
  val json = upickle.json.read(
    Http(s"https://api.github.com/repos/$project/releases")
      .asString
      .body
  )
  val releaseNames = for{
    item <- json.arr
    name <- item.obj.get("name")
  } yield name.str
  println(releaseNames.mkString(","))
}