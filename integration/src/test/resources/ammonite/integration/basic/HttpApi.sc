#!/usr/bin/env amm
// HttpApi.sc

@main
def addPost(title: String, body: String) = {
  ujson.read(
    requests.get(
      "http://jsonplaceholder.typicode.com/posts",
      data = Seq(
        "title"  -> title,
        "body"   -> body,
        "userId" -> "1"
      )
    ).text()
  ).obj.get("id").map(_.num.toInt).getOrElse(0)


}

@main
def comments(postId: Int) = {
  val json = ujson.read(
    requests
      .get(s"http://jsonplaceholder.typicode.com/comments?postId=$postId")
      .text()
  )
  val names = for{
    item <- json.arr
    name <- item.obj.get("name")
  } yield name.str
  names.mkString(",")
}
