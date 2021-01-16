package scala.tools.nsc

import ammonite.util.Util

import scala.tools.nsc.classpath.ClassPathEntries
import scala.tools.nsc.util.ClassPath

class WhiteListClasspath(aggregates: Seq[ClassPath], whitelist: Set[Seq[String]])
  extends scala.tools.nsc.classpath.AggregateClassPath(aggregates) {
  override def findClassFile(name: String) = {
    val tokens = name.split('.')
    if (Util.lookupWhiteList(whitelist, tokens.init ++ Seq(tokens.last + ".class"))) {
      super.findClassFile(name)
    }
    else None
  }
  override def list(inPackage: String) = {
    val superList = super.list(inPackage)
    ClassPathEntries(
      superList.packages.filter{ p => Util.lookupWhiteList(whitelist, p.name.split('.')) },
      superList.classesAndSources.filter{ t =>
        Util.lookupWhiteList(whitelist, inPackage.split('.') ++ Seq(t.name + ".class"))
      }
    )
  }
}