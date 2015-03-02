package ammonite.repl

import java.io.File

import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.{DefaultDependencyDescriptor, DefaultModuleDescriptor}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.parser.xml.XmlModuleDescriptorWriter
import org.apache.ivy.plugins.resolver.{IBiblioResolver, URLResolver}
import acyclic.file
object IvyConstructor extends IvyConstructor
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = (groupId, artifactId)
    def %%(artifactId: String) = (groupId, artifactId + "_2.11")
  }
  implicit class ArtifactIdExt(t: (String, String)){
    def %(version: String) = (t._1, t._2, version)
  }
}

/**
 * Resolve artifacts from Ivy. Originally taken from
 *
 * http://makandracards.com/evgeny-goldin/5817-calling-ivy-from-groovy-or-java
 *
 * And transliterated into Scala. I have no idea how or why it works.
 */
object IvyThing {
  def resolveArtifact(groupId: String, artifactId: String, version: String) = {
    //url resolver for configuration of maven repo
    val resolver = {
      val res = new IBiblioResolver()
      res.setUsepoms(true)
//      res.setUseMavenMetadata(true)
      res.setM2compatible(true)
      res.setName("central")
      res.setRoot("http://repo1.maven.org/maven2/")
      res
    }

    val ivy = Ivy.newInstance{
      //creates clear ivy settings
      val ivySettings = new IvySettings()
      //adding maven repo resolver
      ivySettings.addResolver(resolver)
      //set to the default resolver
      ivySettings.setDefaultResolver(resolver.getName)
      //creates an Ivy instance with settings
      ivySettings
    }

    val ivyfile = File.createTempFile("ivy", ".xml")
    ivyfile.deleteOnExit()

    val md = DefaultModuleDescriptor.newDefaultInstance(
      ModuleRevisionId.newInstance(
        groupId,
        artifactId + "-caller",
        "working"
      )
    )

    md.addDependency(
      new DefaultDependencyDescriptor(
        md,
        ModuleRevisionId.newInstance(groupId, artifactId, version),
        false,
        false,
        true
      )
    )

    //creates an ivy configuration file
    XmlModuleDescriptorWriter.write(md, ivyfile)

    //init resolve report
    val report = ivy.resolve(
      ivyfile.toURI.toURL,
      new ResolveOptions().setConfs(Array("default")).setRefresh(true).setOutputReport(false)
    )
    //so you can get the jar libraries
    report.getAllArtifactsReports.map(_.getLocalFile)
  }
}
