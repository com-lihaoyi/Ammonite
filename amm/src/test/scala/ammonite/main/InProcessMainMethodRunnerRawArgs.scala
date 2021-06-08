package ammonite.main

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

/**
  * Same purpose as [[InProcessMainMethodRunner]] but it accepts the arguments
  * as they are. This is somewhat analogous to [[ammonite.Main.main0]] but it
  * attempts to capture the output and error streams like
  * [[InProcessMainMethodRunner]] without having to append any prefix arguments
  * such as the current path.
  *
  * @see [[InProcessMainMethodRunner]].
  */
class InProcessMainMethodRunnerRawArgs(args: Seq[String]){

  val in = new ByteArrayInputStream(Array.empty[Byte])
  val err0 = new ByteArrayOutputStream()
  val out0 = new ByteArrayOutputStream()

  val success = Console.withIn(in){
    Console.withErr(err0){
      Console.withOut(out0){
        ammonite.Main.main0(args.toList, in, out0, err0)
      }
    }
  }

  val out = new String(out0.toByteArray)
  val err = new String(err0.toByteArray)

  override def toString =
    s"Executor($out, $err)"
}
