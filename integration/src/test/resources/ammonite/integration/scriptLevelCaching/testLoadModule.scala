// Test case to check whether it can load modules from cache and use the imports successfully

import ammonite.ops._
println("Script starts!!")
val scriptDir = cwd/'integration/'src/'test/'resources/'ammonite/'integration/'scriptLevelCaching
load.module(scriptDir/"scriptToBeLoaded.scala")

@

println(x)
