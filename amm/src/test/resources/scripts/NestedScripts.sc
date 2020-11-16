println("script start")
interp.load.module((os.pwd/'amm/'src/'test/'resources/'scripts/"LimitImports.sc").toNIO)
@
println("module should be loaded")
@
val asd2 = asd
