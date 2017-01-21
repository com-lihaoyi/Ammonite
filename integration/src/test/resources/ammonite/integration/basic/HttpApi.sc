#!/usr/bin/env amm
// HttpApi.sc
import scalaj.http._

@main
def addPost(title: String, body: String) = {
  val res = upickle.json.read(
    Http("http://jsonplaceholder.typicode.com/posts")
      .postForm(Seq("title"  -> title,
                    "body"   -> body,
                    "userId" -> "1"))
      .asString
      .body
  ).obj.get("id").map(_.num.toInt).getOrElse(0)
  println(res)
  res
}

@main
def comments(postId: Int) = {
  val json = upickle.json.read(
    Http(s"http://jsonplaceholder.typicode.com/comments?postId=$postId")
      .asString
      .body
  )
  val names = for{
    item <- json.arr
    name <- item.obj.get("name")
  } yield name.str
  println(names.mkString(","))
}
