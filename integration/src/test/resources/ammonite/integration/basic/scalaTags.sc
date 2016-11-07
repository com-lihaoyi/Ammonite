interp.load.ivy("com.lihaoyi" %% "scalatags" % "0.6.2")
@
import scalatags.Text.all._
val res = a("omg", href:="www.google.com").render

println(res)
