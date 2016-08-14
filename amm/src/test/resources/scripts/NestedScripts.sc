println("script start")
import ammonite.ops._
interp.load.module(cwd/'amm/'src/'test/'resources/'scripts/"LimitImports.sc")
@
println("module should be loaded")
@
val asd2 = asd
