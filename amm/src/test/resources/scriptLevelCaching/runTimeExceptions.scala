import ammonite.ops._
println(5/read(cwd/'amm/'target/'test/'resources/'scriptLevelCaching/"num.value").trim.toInt)
rm(cwd/'amm/'target/'test/'resources/'scriptLevelCaching/"num.value")
write(cwd/'amm/'target/'test/'resources/'scriptLevelCaching/"num.value", "0")
