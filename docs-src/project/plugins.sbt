
resolvers += Resolver.typesafeRepo("releases")
resolvers += Resolver.sonatypeRepo("public")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1")
addSbtPlugin("com.typesafe.sbt" % "sbt-web" % "1.4.3")
addSbtPlugin("ba.sake" % "sbt-hepek" % "0.1.2")
