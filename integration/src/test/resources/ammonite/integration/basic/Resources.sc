interp.load.ivy("org.apache.jackrabbit" % "oak-core" % "1.3.16")

@
val path =
  os.resource / "org" / "apache" / "jackrabbit" / "oak" / "plugins" / "blob" / "blobstore.properties"
println(os.read(path).length) // Should work
