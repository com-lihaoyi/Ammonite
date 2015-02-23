package ammonite.ops

import ammonite.pprint.PPrinter

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
    CommandResult(cmd.lineStream)
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
  implicit val commandResultRepr = PPrinter[CommandResult]((x, c) =>
    x.output.iterator.flatMap(Iterator("\n", _))
  )
}
case class CommandResult(output: Stream[String])