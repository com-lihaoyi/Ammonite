package ammonite.ops

import ammonite.pprint.{PPrint, Config, PPrinter}

import scala.language.dynamics

/**
 * Dynamic shell command execution. This allows you to run commands which
 * are not provided by Ammonite, by shelling out to bash. e.g. try
 *
 * %ls
 * %ls "/"
 * %ps "aux"
 */
object % extends %(Vector.empty)
case class %(cmd: Vector[String]) extends Dynamic with CommandBuilder[Int, %]{
  def extend(cmd2: Vector[String]) = new %(cmd ++ cmd2)
  def execute() = {
    new java.lang.ProcessBuilder().command(cmd:_*).inheritIO().start().waitFor()
    // This should work some day too
    //    %git                    %.git
    //    %git %diff              %.git(%).diff
    //    %git "clean"            %.git("clean")
    //    %git("clean", "-fdx")   %.git("clean", "-fdx")
    //    %git %clean %`-fdx`     %.git(%).clean(%).`-fdx`
    //    %git %clean "-fdx"      %.git(%).clean "-fdx"
  }
}

case class CommandResult(output: Stream[String]) extends Seq[String]{
  def iterator = output.iterator
  def apply(idx: Int) = output(idx)
  def length: Int = output.length
}

object CommandResult{
  implicit def commandResultRepr(implicit c: Config) = new PPrint(
    PPrinter[CommandResult]((x, c) =>
      x.output.iterator.flatMap(line =>
        Iterator("\n", c.color.literal(line))
      )
    ),
    c
  )
}

object %% extends %%(Vector.empty)
case class %%(cmd: Vector[String]) extends Dynamic with CommandBuilder[CommandResult, %%]{
  def extend(cmd2: Vector[String]) = new %%(cmd ++ cmd2)
  def execute() = {
    import scala.sys.process._
    CommandResult(cmd.lineStream)
  }
}
object Shellable{
  implicit def StringShellable(s: String): Shellable = Shellable(s)
  implicit def SymbolShellable(s: Symbol): Shellable = Shellable(s.name)
  implicit def BasePathShellable(s: BasePath[_]): Shellable = Shellable(s.toString)
  implicit def NumericShellable[T: Numeric](s: T): Shellable = Shellable(s.toString)
}
case class Shellable(s: String)
trait CommandBuilder[R, B <: CommandBuilder[R, B]]{
  def selectDynamic(s: String) = extend(Vector(s)).execute()
  def applyDynamic(op: String)(args: Shellable*): R = {
    this.extend(Vector(op) ++ args.map(_.s)).execute()
  }
  def extend(cmd2: Vector[String]): B
  def execute(): R
}
