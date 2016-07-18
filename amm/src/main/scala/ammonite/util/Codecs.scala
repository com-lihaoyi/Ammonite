package ammonite.util

object Codecs {

  /**
    * Read/write [[Name]]s as unboxed strings, in order to save verbosity
    * in the JSON cache files as well as improving performance of
    * reading/writing since we read/write [[Name]]s a *lot*.
    */
  implicit val nameRW: upickle.default.ReadWriter[Name] = upickle.default.ReadWriter[Name](
    name => upickle.Js.Str(name.raw),
    {case upickle.Js.Str(raw) => Name(raw)}
  )

}