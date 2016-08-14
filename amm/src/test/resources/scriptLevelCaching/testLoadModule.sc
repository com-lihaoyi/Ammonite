// Test case to check whether it can load modules from cache and use the imports successfully

import ammonite.ops._
println("Script starts!!")
val scriptDir = cwd/'amm/'src/'test/'resources/'scriptLevelCaching
interp.load.module(scriptDir/"scriptToBeLoaded.sc")

@

println(x)
