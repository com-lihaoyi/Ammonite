package ammonite.ops

import scala.language.dynamics

object Shellout{

  def executeInteractive(wd: Path, cmd: Command[_]) = {
    val builder = new java.lang.ProcessBuilder()
    import collection.JavaConversions._
    builder.environment().putAll(cmd.envArgs)
    builder.directory(new java.io.File(wd.toString))
    builder
      .command(cmd.cmd:_*)
      .inheritIO()
      .start()
      .waitFor()

  }
  def executeStream(wd: Path, cmd: Command[_]) = {
    val builder = new java.lang.ProcessBuilder()
    import collection.JavaConversions._
    builder.environment().putAll(cmd.envArgs)
    builder.directory(new java.io.File(wd.toString))
    val process =
      builder
        .command(cmd.cmd:_*)
        .start()
    val stdout = process.getInputStream
    val stderr = process.getErrorStream
    val chunks = collection.mutable.Buffer.empty[Either[Bytes, Bytes]]
    while(process.isAlive){
      for ((std, wrapper) <- Seq(stdout -> (Left(_: Bytes)), stderr -> (Right(_: Bytes)))){
        while (std.available() > 0){
          val array = new Array[Byte](std.available())
          val actuallyRead = std.read(array)
          chunks.append(wrapper(
            if (actuallyRead == array.length) new Bytes(array)
            else new Bytes(array.take(actuallyRead))
          ))
        }
      }
    }


//    val p = Process(cmd.cmd, new java.io.File(wd.toString), cmd.envArgs.toSeq:_*)

    CommandResult(chunks, process.exitValue())
  }
}

/**
 * A staged sub-process command that has yet to be executed.
 */
case class Command[T](cmd: Vector[String],
                      envArgs: Map[String, String],
                      execute: (Path, Command[_]) => T) extends Dynamic {
  def extend(cmd2: Traversable[String], envArgs2: Traversable[(String, String)]) =
    new Command(cmd ++ cmd2, envArgs ++ envArgs2, execute)
  def selectDynamic(name: String)(implicit wd: Path) = execute(wd, extend(Vector(name), Map()))
  def opArg(op: String) = if (op == "apply") Nil else Vector(op)

  def applyDynamic(op: String)(args: Shellable*)(implicit wd: Path): T = {

    execute(wd, this.extend(opArg(op) ++ args.flatMap(_.s), Map()))
  }
  def applyDynamicNamed(op: String)
                       (args: (String, Shellable)*)
                       (implicit wd: Path): T = {
    val (namedArgs, posArgs) = args.map{case (k, v) => (k, v.s)}.partition(_._1 != "")
    execute(wd, this.extend(opArg(op) ++ posArgs.flatMap(_._2),
      namedArgs.map{case (k, v) => (k, v.head)}))
  }
}

class Bytes(val array: Array[Byte]){
  override def equals(other: Any) = other match{
    case otherBytes: Bytes => java.util.Arrays.equals(array, otherBytes.array)
    case _ => false
  }
}
/**
 * Wrapper for the `Stream[String]` of lines returned by a subprocess
 * command, but with a better `PPrint` that makes it look like one
 * block of text for easy reading
 */
case class CommandResult(chunks: Seq[Either[Bytes, Bytes]],
                         exitCode: Int) {

  val output = StreamValue(chunks.collect{case Left(s) => s})
  val error = StreamValue(chunks.collect{case Right(s) => s})

}
case class StreamValue(chunks: Seq[Bytes]){
  def bytes = chunks.iterator.map(_.array).toArray.flatten
  def string: String = string("UTF-8")
  def string(codec: String): String = new String(bytes, codec)
  def lines = string.lines
}
/**
 * An implicit wrapper defining the things that can
 * be "interpolated" directly into a subprocess call.
 */
case class Shellable(s: Seq[String])
object Shellable{
  implicit def StringShellable(s: String): Shellable = Shellable(Seq(s))
  implicit def SeqShellable(s: Seq[String]): Shellable = Shellable(s)
  implicit def SymbolShellable(s: Symbol): Shellable = Shellable(Seq(s.name))
  implicit def BasePathShellable(s: BasePath[_]): Shellable = Shellable(Seq(s.toString))
  implicit def NumericShellable[T: Numeric](s: T): Shellable = Shellable(Seq(s.toString))
}

/**
 * Dynamic shell command execution. This allows you to run commands which
 * are not provided by Ammonite, by shelling out to bash. e.g. try
 *
 * %ls
 * %ls "/"
 * %ps 'aux
 */
abstract class ShelloutRoots(prefix: Seq[String], blankCallsOnly: Boolean) {

}