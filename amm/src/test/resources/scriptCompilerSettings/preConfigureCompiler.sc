val scalacOptions = List(
  "-Yrangepos:true"
)
interp.preConfigureCompiler(_.processArguments(scalacOptions, true))

// Forcing the re-initialisation of the compiler.
// compiler.useOffsetPosition should now be false because the configuration of
// the settings will have occurred before the new compiler become instantiated
@

