println("script start")
import ammonite.ops._
load.module(cwd/'repl/'src/'test/'resources/'scripts/"LimitImports.scala")
@
println("module should be loaded")
@
val asd2 = asd
