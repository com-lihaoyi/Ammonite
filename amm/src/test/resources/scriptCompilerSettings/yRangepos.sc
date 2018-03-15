val scalacOptions = List(
  "-Yrangepos:true"
)
interp.preConfigureCompiler(_.processArguments(scalacOptions, true))

@

case class ABC(a : Int, b : String)
println(ABC(1, "2"))

