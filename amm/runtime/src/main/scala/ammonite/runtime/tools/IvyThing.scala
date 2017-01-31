package ammonite.runtime.tools

import ammonite.runtime.tools.IvyThing._
import ammonite.util.Printer
import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.{DefaultDependencyDescriptor, DefaultModuleDescriptor}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.repository.file.FileRepository
import org.apache.ivy.plugins.resolver._
import org.apache.ivy.util._


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
case class IvyThing(resolvers: () => List[Resolver], printer: Printer, verboseOutput: Boolean) {

  case class IvyResolutionException(failed: Seq[String]) extends Exception(
    "failed to resolve ivy dependencies " + failed.mkString(", ")
  )

  var maxLevel = 2
  var silentIvyLogs: String = ""
  Message.setDefaultLogger(new AbstractMessageLogger {
    def doEndProgress(msg: String) = Console.err.println("Done")
    def doProgress() = Console.err.print(".")
    def log(msg: String, level: Int) =  if (level <= maxLevel) verboseOutput match {
      case true => printer.info(msg)
      case false => silentIvyLogs += msg
    }
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

    //set proxy properties from env:
    ProxyFromEnv.setPropProxyFromEnv()

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
    if (unresolved.size == 0) {
      val artifacts = report.getAllArtifactsReports.map(_.getLocalFile)
      if(artifacts.length == 0)
        throw new Exception(silentIvyLogs)
      else artifacts
    }
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

      // #433 changingPattern, changingMatcher and checkModified are required so that updates to
      // SNAPSHOT versions are pulled in
      // see: https://ant.apache.org/ivy/history/2.3.0/concept.html#change

      // look for changes to SNAPSHOT versions
      chainResolver.setChangingPattern(".*SNAPSHOT")
      // the above pattern is a regex
      chainResolver.setChangingMatcher("regexp")
      // check if ivy metadata has been modified (required for above to work)
      chainResolver.setCheckmodified(true)
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
     m2 = true
   )
 )
}

/**
  * A thin wrapper around [[RepositoryResolver]], which wraps them and provides
  * hashability in order to set the cache tags. This lets us invalidate the ivy
  * resolution cache if the set of resolvers changes
  */
sealed trait Resolver{
  def apply(): RepositoryResolver
}
object Resolver{
  case class File(name: String, root: String, pattern: String, m2: Boolean) extends Resolver{
    def apply() = {
      val testRepoDir = sys.props("user.home") + root
      val repo = new FileRepository(new java.io.File(testRepoDir))

      val res = new FileSystemResolver()
      res.addIvyPattern(testRepoDir + pattern)
      res.addArtifactPattern(testRepoDir + pattern)
      res.setRepository(repo)
      res.setM2compatible(m2)
      res.setName(name)
      // #433 required so that updates to SNAPSHOT versions are pulled in
      res.setChangingPattern(".*SNAPSHOT")
      res.setChangingMatcher("regexp")
      res.setCheckmodified(true)

      res

    }
  }
  case class Http(name: String, root: String, pattern: String, m2: Boolean) extends Resolver{
    def apply() = {
      val res = new IBiblioResolver()
      res.setUsepoms(true)
      res.setM2compatible(m2)
      res.setName(name)
      res.setRoot(root)
      res.setPattern(pattern)
      res
    }
  }
}

private object ProxyFromEnv {
  private lazy val KeyPattern ="""([\w\d]+)_proxy""".r
  private lazy val UrlPattern ="""([\w\d]+://)?(.+@)?([\w\d\.]+):(\d+)/?""".r

  /**
    * Get current proxy environment variables.
    */
  private def getEnvs =
    sys.env.map(p => (p._1.toLowerCase, p._2.toLowerCase)).filterKeys(_.endsWith("proxy"))

  /**
    * Convert single proxy environment variable to corresponding system proxy properties.
    */
  private def envToProps(env: (String, String)): Map[String, String] = env match {
    case ("no_proxy", noProxySeq) =>
      Map.empty //not implemented
    case (KeyPattern(proto), UrlPattern(_, cred, host, port)) =>
      val propHost = s"$proto.proxyHost" -> host
      val propPort = s"$proto.proxyPort" -> port
      val propCred = if (cred.isDefined) {
        val credPair = cred.dropRight(1).split(":")
        val propUser = s"$proto.proxyUser" -> credPair.head
        val propPassword = credPair.drop(1).map(s"$proto.proxyPassword" -> _)
        Seq(propUser) ++ propPassword
      } else Nil
      Seq(propHost, propPort) ++ propCred toMap
    case bad => Map.empty
  }


  /**
    * Set system proxy properties from environment variables.
    * Existing properties will not be overwritten.
    */
  def setPropProxyFromEnv(envs: Map[String, String] = this.getEnvs): Unit = {
    val sysProps = sys.props
    val proxyProps = envs.flatMap { env =>
      val props = envToProps(env)
      if (props.isEmpty) println(s"Warn: environment variable$env cannot be parsed.")
      props
    }.filter(p => !sysProps.exists(sp => sp._1 == p._1))
    sysProps ++= proxyProps
  }

  /**
    * helper implicit conversion: add isDefined method to String.
    */
  implicit private class StringIsDefined(s: String) {
    def isDefined: Boolean = s != null && s.length > 0
  }

}