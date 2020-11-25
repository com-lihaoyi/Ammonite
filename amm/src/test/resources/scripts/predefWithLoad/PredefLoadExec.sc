import $ivy.`com.lihaoyi::os-lib:0.7.1`
repl.load.exec(
  (os.pwd/'amm/'src/'test/'resources/'scripts/'predefWithLoad/"Loaded.sc").toNIO
)

@

val predefDefinedValue = loadedDefinedValue
