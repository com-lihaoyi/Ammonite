import ammonite.ops._
println(5/read(pwd/'amm/'target/'test/'resources/'scriptLevelCaching/"num.value").trim.toInt)
rm(pwd/'amm/'target/'test/'resources/'scriptLevelCaching/"num.value")
write(pwd/'amm/'target/'test/'resources/'scriptLevelCaching/"num.value", "0")
