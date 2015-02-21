package ammonite.repl

import java.io.File

import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.{DefaultDependencyDescriptor, DefaultModuleDescriptor}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.parser.xml.XmlModuleDescriptorWriter
import org.apache.ivy.plugins.resolver.URLResolver

/**
 * Resolve artifacts from Ivy. Originally taken from
 *
 * http://makandracards.com/evgeny-goldin/5817-calling-ivy-from-groovy-or-java
 *
 * And transliterated into Scala. I have no idea how or why it works.
 */
object IvyThing {
  def resolveArtifact(groupId: String, artifactId: String, version: String) = {
    //creates clear ivy settings
    val ivySettings = new IvySettings()

    //url resolver for configuration of maven repo
    val resolver = new URLResolver()

    resolver.setM2compatible(true)
    resolver.setName("central")
    //you can specify the url resolution pattern strategy
    resolver.addArtifactPattern(
      "http://repo1.maven.org/maven2/[organisation]/[module]/[revision]/[artifact](-[revision]).[ext]"
    )
    //adding maven repo resolver
    ivySettings.addResolver(resolver)
    //set to the default resolver
    ivySettings.setDefaultResolver(resolver.getName())
    //creates an Ivy instance with settings
    val ivy = Ivy.newInstance(ivySettings)

    val ivyfile = File.createTempFile("ivy", ".xml")
    ivyfile.deleteOnExit()

    val dep = Array(groupId, artifactId, version)

    val md =
      DefaultModuleDescriptor.newDefaultInstance(
        ModuleRevisionId.newInstance(groupId,
        artifactId + "-caller", "working")
    )

    val dd = new DefaultDependencyDescriptor(
      md,
      ModuleRevisionId.newInstance(groupId, artifactId, version),
      false,
      false,
      true
    )

    md.addDependency(dd)

    //creates an ivy configuration file
    XmlModuleDescriptorWriter.write(md, ivyfile)

    val confs = Array("default")
    val resolveOptions = new ResolveOptions().setConfs(confs).setOutputReport(false)

    //init resolve report
    val report = ivy.resolve(ivyfile.toURL(), resolveOptions)

    //so you can get the jar library
    report.getAllArtifactsReports.map(_.getLocalFile)
  }
}
