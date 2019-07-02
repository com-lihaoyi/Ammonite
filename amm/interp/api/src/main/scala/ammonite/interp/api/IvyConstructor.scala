package ammonite.interp.api

import coursier.core.{ModuleName, Organization}

object IvyConstructor extends IvyConstructor {

  val scalaBinaryVersion =
    scala.util.Properties
              .versionString
              .stripPrefix("version ")
              .split('.')
              .take(2)
              .mkString(".")

  val scalaFullBinaryVersion =
    scala.util.Properties
              .versionNumberString

}
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = coursier.Module(Organization(groupId), ModuleName(artifactId))
    def %%(artifactId: String) = coursier.Module(
      Organization(groupId),
      ModuleName(artifactId + "_" + IvyConstructor.scalaBinaryVersion)
    )
  }
  implicit class ArtifactIdExt(t: coursier.Module){
    def %(version: String) = coursier.Dependency(t, version)
  }
}
