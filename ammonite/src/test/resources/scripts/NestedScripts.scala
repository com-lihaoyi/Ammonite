println("script start")
import ammonite.ops._
load.module(cwd/'ammonite/'src/'test/'resources/'scripts/"LimitImports.scala")
@
println("module should be loaded")
@
val asd2 = asd
