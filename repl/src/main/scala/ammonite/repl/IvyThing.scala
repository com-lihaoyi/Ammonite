package ammonite.repl

import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.{DefaultDependencyDescriptor, DefaultModuleDescriptor}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.util._

import org.apache.ivy.plugins.resolver.IBiblioResolver
import acyclic.file
object IvyConstructor extends IvyConstructor
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = (groupId, artifactId)
    def %%(artifactId: String) = (groupId, artifactId + "_" + IvyThing.scalaBinaryVersion)
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
  val scalaBinaryVersion = scala.util.Properties.versionString
    .stripPrefix("version ").split('.').take(2).mkString(".")

  Message.setDefaultLogger(new AbstractMessageLogger {
    val maxLevel = 1
    def doEndProgress(msg: String) = Console.err.println("Done")
    def doProgress()               = Console.err.print(".")
    def log(msg: String, level: Int)    =
      if (level <= maxLevel &&
          msg != ":: problems summary ::" &&
          msg.trim.nonEmpty &&
          !msg.trim.startsWith("unknown resolver "))
        Console.err.println(msg)
    def rawlog(msg: String, level: Int) = log(msg, level)
  })

  def resolveArtifact(groupId: String, artifactId: String, version: String) = {

    val ivy = Ivy.newInstance{
      val resolver = {
        val res = new IBiblioResolver()
        res.setUsepoms(true)
        //      res.setUseMavenMetadata(true)
        res.setM2compatible(true)
        res.setName("central")
        res.setRoot("http://repo1.maven.org/maven2/")
        res
      }

      //creates clear ivy settings
      val ivySettings = new IvySettings()
      //adding maven repo resolver
      ivySettings.addResolver(resolver)

      println("ivySettings.getResolverNames " + ivySettings.getResolverNames)
      //set to the default resolver
      ivySettings.setDefaultResolver(resolver.getName)
      //creates an Ivy instance with settings
      ivySettings
    }

    val md = DefaultModuleDescriptor.newDefaultInstance(
      ModuleRevisionId.newInstance(
        groupId,
        artifactId + "-caller",
        "working"
      )
    )

    md.addDependency{
      val desc = new DefaultDependencyDescriptor(
        md,
        ModuleRevisionId.newInstance(groupId, artifactId, version),
        false,
        false,
        true
      )
      // No idea wtf this is
      desc.addDependencyConfiguration("*", "*")
      desc
    }

    val options = new ResolveOptions()
      .setConfs(Array("default"))
      .setRefresh(true)
      .setOutputReport(false)

    //init resolve report
    val report = ivy.resolve(md, options)
    //so you can get the jar libraries
    report.getAllArtifactsReports.map(_.getLocalFile)
  }
}
