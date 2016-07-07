import ammonite.ops._
load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")
@
import scalatags.Text.all._
val res = a("omg", href:="www.google.com").render
