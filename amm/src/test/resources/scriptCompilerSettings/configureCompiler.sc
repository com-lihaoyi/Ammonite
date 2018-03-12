val scalacOptions = List(
  "-Yrangepos:true"
)
interp.configureCompiler(_.settings.processArguments(scalacOptions, true))

// Forcing the re-initialisation of the compiler.
// compiler.useOffsetPosition should remain true because it's initialised
// eagerly
@

