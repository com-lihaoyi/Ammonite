package ammonite.repl

import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.{DefaultDependencyDescriptor, DefaultModuleDescriptor}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.util._

import org.apache.ivy.plugins.resolver.IBiblioResolver
import acyclic.file
import java.io.File

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
  val scalaBinaryVersion =
    scala.util.Properties
              .versionString
              .stripPrefix("version ")
              .split('.')
              .take(2)
              .mkString(".")

  var maxLevel = 2
  Message.setDefaultLogger(new AbstractMessageLogger {
    def doEndProgress(msg: String) = Console.err.println("Done")
    def doProgress() = Console.err.print(".")
    def log(msg: String, level: Int) =  if (level <= maxLevel) Console.err.println(msg)
    def rawlog(msg: String, level: Int) = log(msg, level)
  })

  def resolveArtifact(groupId: String,
                      artifactId: String,
                      version: String,
                      verbosity: Int = 2) : Option[Array[File]] = synchronized {
    maxLevel = verbosity
    val ivy = Ivy.newInstance{

      def resolver(name: String) = {
        val res = new IBiblioResolver()
        res.setUsepoms(true)
        res.setM2compatible(true)
        res.setName(name)
        res.setRoot("http://repo1.maven.org/maven2/")
        res
      }
      //add duplicate resolvers with different name to make Ivy shut up
      //and stop giving `unknown resolver null` or `unknown resolver sbt-chain`
      //errors
      val resolvers = Seq(
        resolver("central"),
        resolver("sbt-chain"),
        resolver("null")
      )
      //creates clear ivy settings
      val ivySettings = new IvySettings()
      //adding maven repo resolver
      resolvers.foreach(ivySettings.addResolver)

      //set to the default resolver
      ivySettings.setDefaultResolver(resolvers(0).getName)
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
    val unresolved = report.getUnresolvedDependencies()
    if (unresolved.size > 0) {
      Console.err.println("There were unresolved dependencies:")
      unresolved.foreach(ur => Console.err.println("Dependency %s was unresolved".format(ur.getId())))
      None
    }
    else Some(report.getAllArtifactsReports.map(_.getLocalFile))
  }
}
