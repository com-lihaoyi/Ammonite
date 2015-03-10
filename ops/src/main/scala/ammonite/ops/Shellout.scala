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
object % extends %(Vector.empty){
  def execute(cmd: Seq[String]): CommandResult = {
    import scala.sys.process._
    CommandResult(cmd.lines) // Should be lineStream instead of lines in 2.11, doing this for 2.10 compatibility
//    %git                    %.git
//    %git %diff              %.git(%).diff
//    %git "clean"            %.git("clean")
//    %git("clean", "-fdx")   %.git("clean", "-fdx")
//    %git %clean %`-fdx`     %.git(%).clean(%).`-fdx`
//    %git %clean "-fdx"      %.git(%).clean "-fdx"
  }
}

class %(val cmd: Vector[String]) extends Dynamic{
  def selectDynamic(s: String) = %.execute(cmd ++ Seq(s))
  def applyDynamic[T, V](op: String)(args: T*)(implicit ce: CommandExtender[T, V]): V = {
    ce.extend(cmd, op, args)
  }
}

trait CommandExtender[T, V]{
  def extend(cmd: Vector[String], op: String, args: Seq[T]): V
}

object CommandExtender{
  implicit object Str extends CommandExtender[String, CommandResult]{
    def extend(cmd: Vector[String], op: String, args: Seq[String]) =
      %.execute(cmd ++ Seq(op) ++ args)
  }
  implicit object Chain extends CommandExtender[%.type, %]{
    def extend(cmd: Vector[String], op: String, args: Seq[%.type]) =
      new %(cmd ++ Seq(op))
  }
}
object CommandResult{
  implicit def commandResultRepr(implicit c: Config) =
    new PPrint(
      PPrinter[CommandResult]((x, c) =>
      x.output.iterator.flatMap(line =>
        Iterator("\n", c.color.literal(line))
      )
    ),
    c
  )
}
case class CommandResult(output: Stream[String]) extends Seq[String]{
  def iterator = output.iterator
  def apply(idx: Int) = output(idx)
  def length: Int = output.length
}
