import ammonite.ops._
println(5/read(cwd/'ammonite/'target/'test/'resources/'scriptLevelCaching/"num.value").trim.toInt)
rm(cwd/'ammonite/'target/'test/'resources/'scriptLevelCaching/"num.value")
write(cwd/'ammonite/'target/'test/'resources/'scriptLevelCaching/"num.value", "0")
