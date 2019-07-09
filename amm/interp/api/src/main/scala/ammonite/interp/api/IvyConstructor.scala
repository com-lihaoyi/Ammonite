package ammonite.interp.api

import coursierapi.{Dependency, Module}

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
    def %(artifactId: String) = Module.of(groupId, artifactId)
    def %%(artifactId: String) = Module.of(
      groupId,
      artifactId + "_" + IvyConstructor.scalaBinaryVersion
    )
  }
  implicit class ArtifactIdExt(t: Module){
    def %(version: String) = Dependency.of(t, version)
  }
}
