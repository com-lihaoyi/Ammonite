import ammonite.ops._
println(5/read(pwd/'target/'test/'resources/'scriptLevelCaching/"num.value").trim.toInt)
rm(pwd/'target/'test/'resources/'scriptLevelCaching/"num.value")
write(pwd/'target/'test/'resources/'scriptLevelCaching/"num.value", "0")
