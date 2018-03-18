val scalacOptions = List(
  "-Yrangepos:true"
)
interp.preConfigureCompiler(_.processArguments(scalacOptions, true))

@

case class ABC(a : Int, b : String)
val a = ABC(1, 1) // compiler error, the test checks the line at which it throws

