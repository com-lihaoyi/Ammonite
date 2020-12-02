#!/usr/bin/env amm
// HttpApi.sc

lazy val jsonPlaceHolderBase =
  Option(System.getenv("JSONPLACEHOLDER")).getOrElse {
    "http://jsonplaceholder.typicode.com"
  }

@main
def addPost(title: String, body: String) = {
  ujson.read(
    requests.post(
      s"$jsonPlaceHolderBase/posts",
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
      .get(s"$jsonPlaceHolderBase/comments?postId=$postId")
      .text()
  )
  val names = for{
    item <- json.arr
    name <- item.obj.get("name")
  } yield name.str
  names.mkString(",")
}
