import ammonite.ops._
println(5/read(cwd/'repl/'target/'test/'resources/'scriptLevelCaching/"num.value").trim.toInt)
rm(cwd/'repl/'target/'test/'resources/'scriptLevelCaching/"num.value")
write(cwd/'repl/'target/'test/'resources/'scriptLevelCaching/"num.value", "0")
