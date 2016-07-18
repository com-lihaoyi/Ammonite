package ammonite.tools

import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.{DefaultDependencyDescriptor, DefaultModuleDescriptor}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.{ResolveOptions}
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.repository.file.FileRepository
import org.apache.ivy.util._

import org.apache.ivy.plugins.resolver._
import acyclic.file
import IvyThing._
import ammonite.frontend.Resolver


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
case class IvyThing(resolvers: () => List[Resolver]) {

  case class IvyResolutionException(failed: Seq[String]) extends Exception(
    "failed to resolve ivy dependencies " + failed.mkString(", ")
  )

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
                      verbosity: Int = 2) = synchronized {
    maxLevel = verbosity
    val ivy = ivyInstance(resolvers)

    val md = DefaultModuleDescriptor.newDefaultInstance(
      ModuleRevisionId.newInstance(
        groupId,
        artifactId + "-caller",
        "working"
      )
    )

    md.addDependency {
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
    val unresolved = report.getAllProblemMessages

    import collection.JavaConversions._
//    println("IVY THING DEBUGGING")
//    println(report.getAllProblemMessages.toSeq)
//    println(report.getProblemMessages.toSeq)
//    println(report.getAllArtifactsReports.toSeq)
//    println(report.getFailedArtifactsReports.toSeq)
//    println(report.getArtifacts.toSeq)
//    println(report.getUnresolvedDependencies.toSeq)
//    report.getUnresolvedDependencies
//          .toSeq
//          .map(_.getProblem)
//          .foreach(_.printStackTrace())
//
//    println(report.getUnresolvedDependencies.map(_.getProblemMessage).toSeq)
    if (unresolved.size == 0) report.getAllArtifactsReports.map(_.getLocalFile)
    else throw IvyResolutionException(unresolved.toSeq.map(_.toString))
  }

}

object IvyThing {

  def ivyInstance(resolvers: () => List[Resolver]) = Ivy.newInstance {

      // create clear ivy settings
      val ivySettings = new IvySettings(){
        // Override getResolver to make it stop spewing up useless
        // `unknown resolver` errors:  lihaoyi/Ammonite/issues/144
        override def getResolver(resolverName: String): DependencyResolver = {
          if (!this.getResolverNames.contains(resolverName)) null
          else super.getResolver(resolverName)
        }
      }

      // add maven repo resolver
      val chainResolver = new ChainResolver
      chainResolver.setName("chain-resolver")
      chainResolver.setReturnFirst(true)
      resolvers().map(_()).foreach(chainResolver.add)
      ivySettings.addResolver(chainResolver)

      // set to the default resolver
      ivySettings.setDefaultResolver(chainResolver.getName)

      // creates an Ivy instance with settings
      ivySettings
    }
    
  val scalaBinaryVersion =
    scala.util.Properties
              .versionString
              .stripPrefix("version ")
              .split('.')
              .take(2)
              .mkString(".")
  
}


object Resolvers {

  // this pattern comes from sbt.Resolver  
  val IvyPattern: String = 
    "[organisation]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)"+
    "[revision]/[type]s/[artifact](-[classifier]).[ext]"
    
  // this pattern comes from IBiblioResolver  
  val MavenPattern: String =
    "[organisation]/[module]/" + 
    "[revision]/[artifact]-[revision](-[classifier]).[ext]"
  
  // this pattern comes from IBiblioResolver  
  val DefaultPattern: String =
    "[module]/[type]s/[artifact]-[revision].[ext]"

  

 lazy val defaultResolvers: List[Resolver] = List(
   Resolver.File(
     "ivy-cache",
     "/.ivy2/cache",
     "/[organisation]/[module]/jars/[artifact]-[revision].[ext]",
     m2 = false
   ),
   Resolver.File(
     "cache",
     "/.ivy2/cache",
     "/[organisation]/[module]/jars/[artifact]-[revision].[ext]",
     m2 = false
   ),
   Resolver.File(
     "local",
     "/.ivy2/local",
     "/[organisation]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext]",
     m2 = false
   ),
   Resolver.File(
     "m2",
     "/.m2/repository",
     "/[organisation]/[module]/[revision]/[artifact]-[revision].[ext]",
     m2 = true
   ),
   Resolver.Http(
     "central",
     "http://repo1.maven.org/maven2/",
     MavenPattern,
    true
   )
 )
}
