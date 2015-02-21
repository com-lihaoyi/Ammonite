package ammonite.ops
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
  def execute(cmd: Seq[String]): Ret = {
    import scala.sys.process._
    cmd.!
//    %git                    %.git
//    %git %diff              %.git(%).diff
//    %git "clean"            %.git("clean")
//    %git("clean", "-fdx")   %.git("clean", "-fdx")
//    %git %clean %`-fdx`     %.git(%).clean(%).`-fdx`
//    %git %clean "-fdx"      %.git(%).clean "-fdx"
  }
  type Ret = Int
}


class %(val cmd: Vector[String]) extends Dynamic{
  def selectDynamic(s: String) = %.execute(cmd ++ Seq(s))
  def applyDynamic[T, V](op: String)(args: T*)(implicit ce: CommandExtender[T, V]) = {
    ce.extend(cmd, op, args)
  }
}

trait CommandExtender[T, V]{
  def extend(cmd: Vector[String], op: String, args: Seq[T]): V
}

object CommandExtender{
  implicit object Str extends CommandExtender[String, %.Ret]{
    def extend(cmd: Vector[String], op: String, args: Seq[String]) =
      %.execute(cmd ++ Seq(op) ++ args)
  }
  implicit object Chain extends CommandExtender[%.type, %]{
    def extend(cmd: Vector[String], op: String, args: Seq[%.type]) =
      new %(cmd ++ Seq(op))
  }
}

case class CommandException() extends Exception()