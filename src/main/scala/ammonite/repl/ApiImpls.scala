// package ammonite.repl

// import ammonite.runtime._
// import ammonite.util.Util._
// import ammonite.util._
// import pprint.{Config, PPrint}

// class ReplApiImpl(val interp: Interpreter) extends DefaultReplAPI {

//   import interp._

//   implicit lazy val pprintConfig: Ref[pprint.Config] = {
//     Ref.live[pprint.Config](() => pprint.Config.apply())
//   }

//   def show[T: PPrint](implicit cfg: Config) = (t: T) => {
//     pprint.tokenize(t, height = 0)(implicitly[PPrint[T]], cfg).foreach(printer.out)
//     printer.out(newLine)
//   }
//   def show[T: PPrint](t: T,
//                       width: Integer = null,
//                       height: Integer = 0,
//                       indent: Integer = null,
//                       colors: pprint.Colors = null)(implicit cfg: Config = Config.Defaults.PPrintConfig) = {

//     pprint.tokenize(t, width, height, indent, colors)(implicitly[PPrint[T]], cfg).foreach(printer.out)
//     printer.out(newLine)
//   }

// }
