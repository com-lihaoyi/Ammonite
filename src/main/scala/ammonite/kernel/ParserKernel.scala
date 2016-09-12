// package ammonite.kernel

// import ammonite.runtime.Parsers
// import fastparse.core.{Parsed, ParseError}
// import scalaz._
// import Parsed.{Success, Failure}

// object ParserKernel {

//   def parseCode(code: String): Option[Validation[LogError, NonEmptyList[String]]] =
//     Parsers.Splitter.parse(code) match {
//       case Success(statements, _) =>
//         statements.toList match {
//           case h :: t =>
//             val nel = NonEmptyList(h, t: _*)
//             Some(Validation.success(nel))
//           case Nil => None
//         }
//       case Failure(_, index, extra) =>
//         Some(Validation.failure(LogError(ParseError.msg(extra.input, extra.traced.expected, index))))
//     }

// }