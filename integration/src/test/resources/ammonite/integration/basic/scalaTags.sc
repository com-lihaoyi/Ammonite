interp.load.ivy("com.lihaoyi" %% "scalatags" % "0.7.0")
@
import scalatags.Text.all._
val res = a("omg", href:="www.google.com").render

println(res)
