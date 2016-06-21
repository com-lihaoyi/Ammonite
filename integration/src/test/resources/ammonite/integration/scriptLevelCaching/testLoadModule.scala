// Test case to check whether it can load modules from cache and use the imports successfully

import ammonite.ops._
println("Script starts!!")
val scriptDir = 'integration/'src/'test/'resources/'ammonite/'integration/'scriptLevelCaching
load.module(Path("/home/abhishek/Amm_Master")/scriptDir/"scriptToBeLoaded.scala")

@

//println("\n\n\n\n\n****&*&*&*&*&*& DOiing GOOD!! &*&*&*&\n\n\n\n")
//@
println("OKKKKKKKKKKKKKKKKKKKKKKKKKK  ")
println(x)
