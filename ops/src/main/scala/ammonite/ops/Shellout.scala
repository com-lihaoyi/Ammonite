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

    val res = CommandResult(process.exitValue(), chunks)
    if (res.exitCode == 0) res
    else throw ShelloutException(res)
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
  override def toString = new String(array)
}
/**
  * Contains the accumulated output for the invocation of a subprocess command.
  *
  * Apart from the exit code, the primary data-structure is a sequence of byte
  * chunks, tagged with [[Left]] for stdout and [[Right]] for stderr. This is
  * interleaved roughly in the order it was emitted by the subprocess, and
  * reflects what a user would have see if the subprocess was run manually.
  *
  * Derived from that, is the aggregate `out` and `err` [[StreamValue]]s,
  * wrapping stdout/stderr respectively, and providing convenient access to
  * the aggregate output of each stream, as bytes or strings or lines.
  */
case class CommandResult(exitCode: Int,
                         chunks: Seq[Either[Bytes, Bytes]]) {

  val out = StreamValue(chunks.collect{case Left(s) => s})
  val err = StreamValue(chunks.collect{case Right(s) => s})
  override def toString() = {
    s"CommandResult $exitCode\n" +
      chunks.iterator
            .collect{case Left(s) => s case Right(s) => s}
            .map(x => new String(x.array))
            .mkString
  }
}

/**
  * Thrown when a shellout command results in a non-zero exit code.
  *
  * Doesn't contain any additional information apart from the [[CommandResult]]
  * that is normally returned, but ensures that failures in subprocesses happen
  * loudly and won't get ignored unless intentionally caught
  */
case class ShelloutException(result: CommandResult) extends Exception(result.toString)
case class StreamValue(chunks: Seq[Bytes]){
  def bytes = chunks.iterator.map(_.array).toArray.flatten
  lazy val string: String = string("UTF-8")
  def string(codec: String): String = new String(bytes, codec)
  lazy val lines: Iterator[String] = string.lines
  def lines(codec: String): Iterator[String] = string(codec).lines
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

