package ammonite.repl.api

trait SessionChanged {
  def removedImports: Set[scala.Symbol]
  def addedImports: Set[scala.Symbol]
  def removedJars: Set[java.net.URL]
  def addedJars: Set[java.net.URL]
}
