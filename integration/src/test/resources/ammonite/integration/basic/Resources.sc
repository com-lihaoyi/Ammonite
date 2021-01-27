import ammonite.ops._
interp.load.ivy("org.apache.jackrabbit" % "oak-core" % "1.3.16")
@
val path = resource/"org"/"apache"/"jackrabbit"/"oak"/"plugins"/"blob"/"blobstore.properties"
println(read(path).length) // Should work