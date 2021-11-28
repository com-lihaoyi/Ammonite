package ammonite.main

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}


object InProcessMainMethodRunner{
  val base = os.pwd/'amm/'src/'test/'resources
}

/**
  * An in-memory mock subprocess; exposes the same API as a subprocess call:
  * stdin, stdout, stderr, and command-line arguments as a Array[String]. But
  * runs the computation in memory, which is often a lot faster.
  *
  * Attempts to capture stdout and stderr from the current thread via
  * the `Console.with*` methods. This doesn't always work, e.g. it doesn't
  * capture Java `System.*.println` methods, and overall this doesn't guarantee
  * JVM-level isolation between tests. But it works well enough for
  * our unit tests.
  */
class InProcessMainMethodRunner(p: os.RelPath, preArgs: List[String], args: Seq[String]){

  val in = new ByteArrayInputStream(Array.empty[Byte])
  val err0 = new ByteArrayOutputStream()
  val out0 = new ByteArrayOutputStream()
  val path = InProcessMainMethodRunner.base/p


  val success = Console.withIn(in){
    Console.withErr(err0){
      Console.withOut(out0){
        ammonite.Main.main0(
          List("--home", os.temp.dir().toString) ++
          preArgs ++
          Seq(path.toString) ++
          args.toList,
          in,
          out0,
          err0
        )
      }
    }
  }


  val out = new String(out0.toByteArray)
  val err = new String(err0.toByteArray)
  override def toString = {
    s"Executor($out, $err)"
  }
}
