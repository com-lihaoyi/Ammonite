val dep =
  if (interp.scalaVersion.value.startsWith("3."))
    "com.lihaoyi" % "scalatags_2.13" % "0.7.0"
  else
    "com.lihaoyi" %% "scalatags" % "0.7.0"
interp.load.ivy(dep)
@
import scalatags.Text.all._
val res = a("omg", href:="www.google.com").render
