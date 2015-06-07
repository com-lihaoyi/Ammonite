
package ammonite.shell
object Shell{
  def main(args: Array[String]) = ammonite.repl.Repl.run("""
    import ammonite.ops._
    import ammonite.pprint.pprintln
    implicit var wd = cwd

  """)
}