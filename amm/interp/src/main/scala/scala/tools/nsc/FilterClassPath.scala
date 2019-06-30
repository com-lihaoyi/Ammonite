package scala.tools.nsc

import scala.tools.nsc.classpath.ClassPathEntries
import scala.tools.nsc.util.ClassPath

class FilterClassPath(aggregates: Seq[ClassPath], filter: Seq[String] => Boolean)
  extends scala.tools.nsc.classpath.AggregateClassPath(aggregates) {
  override def findClassFile(name: String) = {
    val tokens = name.split('.')
    if (!filter(tokens.init ++ Seq(tokens.last + ".class"))) None else super.findClassFile(name)
  }
  override def list(inPackage: String) = {
    val superList = super.list(inPackage)
    ClassPathEntries(
      superList.packages.filter{ p => filter(p.name.split('.')) },
      superList.classesAndSources.filter{ t =>
        filter(inPackage.split('.') ++ Seq(t.name + ".class"))
      }
    )
  }
}