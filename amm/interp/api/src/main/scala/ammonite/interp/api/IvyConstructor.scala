package ammonite.interp.api

import coursierapi.{Dependency, Module}

case class ScalaVersion(value: String)

object IvyConstructor extends IvyConstructor {

  def scalaBinaryVersion(sv: String) =
    sv.split('.').take(2).mkString(".")

}
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = Module.of(groupId, artifactId)
    def %%(artifactId: String)(implicit sv: ScalaVersion) = Module.of(
      groupId,
      artifactId + "_" + IvyConstructor.scalaBinaryVersion(sv.value)
    )
  }
  implicit class ArtifactIdExt(t: Module){
    def %(version: String) = Dependency.of(t, version)
  }
}
