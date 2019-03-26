interp.load.ivy("com.lihaoyi" %% "scalatags" % "0.6.8")
@
import scalatags.Text.all._
val res = a("omg", href:="www.google.com").render

println(res)
