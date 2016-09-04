// package ammonite

// import ammonite.main.Router
// import ammonite.main.Router.Result.Error.{InvalidArguments, RedundantArguments, TooManyArguments}
// import ammonite.main.Router.Result.ParamError
// import ammonite.main.Router.{ArgSig, doc, main}
// import ammonite.util.Util
// import utest._

// object UnitTests extends TestSuite{

//   val tests = TestSuite {
//     println("UnitTests")
//     'transpose{
//       // Make sure this doesn't stack-overflow
//       val inner = List.fill(1000000)(1)
//       assert(Util.transpose(List.fill(10)(inner)) == List.fill(1000000)(List.fill(10)(1)))
//     }
//     'router{
//       case object MyException extends Exception
//       object Target{
//         @main
//         def foo() = 1
//         @main
//         def bar(i: Int) = i
//         @main
//         def qux(i: Int,
//                 @doc("Pass in a custom `s` to override it")
//                 s: String = "lols") = s * i
//         @main
//         def ex() = throw MyException

//         def notExported() = ???

//         val baz = "bazzz"

//         @main
//         def pureVariadic(nums: Int*) = nums.sum

//         @main
//         def mixedVariadic(first: Int, args: String*) = first + args.mkString
//       }
//       val routes = Router.generateRoutes(Target)


//       'basicModelling{
//         assert(
//           routes.map(_.name) == Seq("foo", "bar", "qux", "ex", "pureVariadic", "mixedVariadic")
//         )
//         val evaledArgs = routes.map(_.argSignatures.map{
//           case Router.ArgSig(name, tpe, docs, None) => (name, tpe, docs, None)
//           case Router.ArgSig(name, tpe, docs, Some(default)) => (name, tpe, docs, Some(default()))
//         })
//         assert(
//           evaledArgs == List(
//             List(),
//             List(("i", "Int", None, None)),
//             List(
//               ("i", "Int", None, None),
//               ("s", "String", Some("Pass in a custom `s` to override it"), Some("lols"))
//             ),
//             List(),
//             List(("nums", "Int*", None, None)),
//             List(("first", "Int", None, None), ("args", "String*", None, None))
//           )
//         )
//       }

//       'invoke - assert(
//         routes(0).invoke(Seq.empty, Seq.empty) == Router.Result.Success(1),
//         routes(1).invoke(Seq.empty, Seq("i" -> "2")) == Router.Result.Success(2),
//         routes(2).invoke(Seq.empty, Seq("i" -> "2")) == Router.Result.Success("lolslols"),
//         routes(2).invoke(Seq.empty, Seq("i" -> "3", "s" -> "x")) == Router.Result.Success("xxx")
//       )
//       'varargs{
//         'happyPathPasses - assert(
//           routes(4).invoke(Seq("1", "2", "3"), Seq()) == Router.Result.Success(6),
//           routes(5).invoke(Seq("1", "2", "3", "4", "5"), Seq()) == Router.Result.Success("12345")
//         )
//         'emptyVarargsPasses - assert(
//           routes(4).invoke(Seq(), Seq()) == Router.Result.Success(0),
//           routes(5).invoke(Seq("1"), Seq()) == Router.Result.Success("1")
//         )
//         'namedVarargAlonePasses - assert(
//           routes(4).invoke(Seq(), Seq("nums" -> "31337")) == Router.Result.Success(31337),
//           routes(5).invoke(Seq("1"), Seq("args" -> "foo")) == Router.Result.Success("1foo")
//         )
//         'duplicatePositionalAndNamedVarargsFails{
//           assertMatch(routes(4).invoke(Seq("1", "2", "3"), Seq("nums" -> "4"))){
//             case RedundantArguments(Seq("nums")) =>
//           }
//         }

//         'notEnoughNormalArgsStillFails{
//           assertMatch(routes(5).invoke(Seq(), Seq())){
//             case InvalidArguments(
//               Seq(ParamError.Missing(ArgSig("first", _, _, _)))
//             )=>
//           }
//         }
//         'multipleVarargParseFailures{
//           assertMatch(routes(4).invoke(Seq("aa", "bb", "3"), Seq())){
//             case InvalidArguments(
//               Seq(
//                 ParamError.Invalid(ArgSig("nums", "Int*", _, _), "aa", _: NumberFormatException),
//                 ParamError.Invalid(ArgSig("nums", "Int*", _, _), "bb", _: NumberFormatException)
//               )
//             )=>
//           }
//           assertMatch(routes(5).invoke(Seq("aa", "bb", "3"), Seq())){
//             case InvalidArguments(
//               Seq(
//                 ParamError.Invalid(ArgSig("first", "Int", _, _), "aa", _: NumberFormatException)
//               )
//             )=>
//           }
//         }
//       }

//       'failures{
//         'missingParams - {
//           assertMatch(routes(1).invoke(Seq.empty, Seq.empty)){
//             case InvalidArguments(
//               Seq(ParamError.Missing(ArgSig("i", _, _, _)))
//             ) =>
//           }
//           assertMatch(routes(2).invoke(Seq.empty, Seq("s" -> "omg"))){
//             case InvalidArguments(
//               Seq(ParamError.Missing(ArgSig("i", _, _, _)))
//             ) =>
//           }
//         }
//         'invalidParams - assertMatch(routes(1).invoke(Seq("lol"), Seq.empty)) {
//           case InvalidArguments(
//             Seq(ParamError.Invalid(ArgSig("i", _, _, _), "lol", _))
//           ) =>
//         }

//         'tooManyParams - assert(
//           routes(0).invoke(Seq("1", "2"), Seq.empty) ==
//           TooManyArguments(Seq("1", "2"))
//         )

//         'redundantParams - assert(
//           routes(1).invoke(Seq("1"), Seq("i" -> "2")) ==
//           RedundantArguments(Seq("i"))
//         )

//         'failing - assert(
//           routes(3).invoke(Seq(), Seq()) ==
//           Router.Result.Error.Exception(MyException)
//         )
//       }
//     }
//   }
// }
