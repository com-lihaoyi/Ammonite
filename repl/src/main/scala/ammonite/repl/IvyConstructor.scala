package ammonite.repl

import acyclic.file

object IvyConstructor extends IvyConstructor
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    private def scalaBinaryVersion =
      scala.util.Properties
        .versionNumberString
        .split('.')
        .take(2)
        .mkString(".")

    def %(artifactId: String) = (groupId, artifactId)
    def %%(artifactId: String) = (groupId, artifactId + "_" + scalaBinaryVersion)
  }
  implicit class ArtifactIdExt(t: (String, String)){
    def %(version: String) = (t._1, t._2, version)
  }
}
