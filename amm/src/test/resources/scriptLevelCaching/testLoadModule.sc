// Test case to check whether it can load modules from cache and use the imports successfully

println("Script starts!!")
val scriptDir = os.pwd / "amm" / "src" / "test" / "resources" / "scriptLevelCaching"
interp.load.module(scriptDir / "scriptToBeLoaded.sc")

@
println(x)
