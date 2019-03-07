import coursier.dependencyString

interp.load.ivy(
  // Using SBT-style syntax
  "xom" % "xom" % "1.1",
  // Directly using coursier API. Can pass in exclusions,
  // attributes, configurations, classifiers, etc.
  dep"net.sf.json-lib:json-lib:2.4,classifier=jdk15"
)

@

// Code using the dependencies
val serializer = new net.sf.json.xml.XMLSerializer