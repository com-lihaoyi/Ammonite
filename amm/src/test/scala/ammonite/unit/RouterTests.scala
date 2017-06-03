package ammonite.unit

import ammonite.repl.Highlighter
import ammonite.main.{Cli, Router}
import ammonite.main.Router.Result.Error.{InvalidArguments, MismatchedArguments}
import ammonite.main.Router.Result.ParamError
import ammonite.main.Router.{ArgSig, EntryPoint, doc, main}
import ammonite.util.Util
import utest._
import ammonite.main.Scripts.groupArgs
object RouterTests extends TestSuite{
  def parseInvoke(entryPoint: EntryPoint, input: List[String]) = {
    val grouped = groupArgs(input)
    entryPoint.invoke(grouped)
  }
  def check[T](entryPoint: EntryPoint,
               input: List[String],
               expected: Router.Result[T]) = {
    val result = parseInvoke(entryPoint, input)
    assert(result == expected)
  }
  val tests = TestSuite {
    println("UnitTests")
    'transpose{
      // Make sure this doesn't stack-overflow
      val inner = List.fill(1000000)(1)
      assert(Util.transpose(List.fill(10)(inner)) == List.fill(1000000)(List.fill(10)(1)))
    }
    'router{
      case object MyException extends Exception
      object Target{
        @main
        def foo() = 1
        @main
        def bar(i: Int) = i
        @main
        def qux(i: Int,
                s: String @doc("Pass in a custom `s` to override it") = "lols") = s * i
        @main
        def ex() = throw MyException

        def notExported() = ???

        val baz = "bazzz"

        @main
        def pureVariadic(nums: Int*) = nums.sum

        @main
        def mixedVariadic(first: Int, args: String*) = first + args.mkString
      }
      val routes = Router.generateRoutes(Target)


      'basicModelling{
        assert(
          routes.map(_.name) == List("foo", "bar", "qux", "ex", "pureVariadic", "mixedVariadic")
        )
        val evaledArgs = routes.map(_.argSignatures.map{
          case Router.ArgSig(name, tpe, docs, None) => (name, tpe, docs, None)
          case Router.ArgSig(name, tpe, docs, Some(default)) => (name, tpe, docs, Some(default()))
        })
        assert(
          evaledArgs == List(
            List(),
            List(("i", "Int", None, None)),
            List(
              ("i", "Int", None, None),
              ("s", "String", Some("Pass in a custom `s` to override it"), Some("lols"))
            ),
            List(),
            List(("nums", "Int*", None, None)),
            List(("first", "Int", None, None), ("args", "String*", None, None))
          )
        )
      }


      'invoke - {
        check(routes(0), List(), Router.Result.Success(1))
        check(routes(1), List("2"), Router.Result.Success(2))
        check(routes(1), List("--i", "2"), Router.Result.Success(2))
        check(routes(2), List("2"), Router.Result.Success("lolslols"))
        check(routes(2), List("--i", "2"), Router.Result.Success("lolslols"))
        check(routes(2), List("3", "x"), Router.Result.Success("xxx"))
        check(routes(2), List("--i", "3", "x"), Router.Result.Success("xxx"))
        check(routes(2), List("--i", "3", "--s", "x"), Router.Result.Success("xxx"))
      }
      'varargs{
        'happyPathPasses - {
          check(routes(4), List("1", "2", "3"), Router.Result.Success(6))
          check(routes(5), List("1", "2", "3", "4", "5"), Router.Result.Success("12345"))
        }
        'emptyVarargsPasses - {
          check(routes(4), List(), Router.Result.Success(0))
          check(routes(5), List("1"), Router.Result.Success("1"))
        }
        'varargsAreAlwaysPositional - {
          val invoked = parseInvoke(routes(4), List("--nums", "31337"))
          assertMatch(invoked){
            case InvalidArguments(List(
              ParamError.Invalid(
                ArgSig("nums", "Int*", _, _),
                "--nums",
                _: NumberFormatException
              )
            ))=>
          }
          check(routes(5), List("1", "--args", "foo"), Router.Result.Success("1--argsfoo"))

          assertMatch(parseInvoke(routes(4), List("1", "2", "3", "--nums", "4"))){
            case InvalidArguments(List(
              ParamError.Invalid(
                ArgSig("nums", "Int*", _, _),
                "--nums",
                _: NumberFormatException
              )
            ))=>
          }
        }

        'notEnoughNormalArgsStillFails{
          assertMatch(parseInvoke(routes(5), List())){
            case MismatchedArguments(List(ArgSig("first", _, _, _)), Nil, Nil, None) =>
          }
        }
        'multipleVarargParseFailures{
          assertMatch(parseInvoke(routes(4), List("aa", "bb", "3"))){
            case InvalidArguments(
            List(
            ParamError.Invalid(ArgSig("nums", "Int*", _, _), "aa", _: NumberFormatException),
            ParamError.Invalid(ArgSig("nums", "Int*", _, _), "bb", _: NumberFormatException)
            )
            )=>
          }
          assertMatch(parseInvoke(routes(5), List("aa", "bb", "3"))){
            case InvalidArguments(
            List(
            ParamError.Invalid(ArgSig("first", "Int", _, _), "aa", _: NumberFormatException)
            )
            )=>
          }
        }
      }

      'failures{
        'missingParams - {
          assertMatch(parseInvoke(routes(1), List.empty)){
            case MismatchedArguments(List(ArgSig("i", _, _, _)), Nil, Nil, None) =>
          }
          assertMatch(parseInvoke(routes(2), List("--s", "omg"))){
            case MismatchedArguments(List(ArgSig("i", _, _, _)), Nil, Nil, None) =>
          }
        }
        'invalidParams - assertMatch(parseInvoke(routes(1), List("lol"))){
          case InvalidArguments(
          List(ParamError.Invalid(ArgSig("i", _, _, _), "lol", _))
          ) =>
        }

        'tooManyParams - check(
          routes(0), List("1", "2"),
          MismatchedArguments(Nil, List("1", "2"), Nil, None)
        )


        'redundantParams - {
          val parsed = parseInvoke(routes(2), List("1", "--i", "2"))
          assertMatch(parsed){
            case MismatchedArguments(
              Nil, Nil, Seq((ArgSig("i", _, _, _), Seq("1", "2"))), None
            ) =>
          }
        }
        'failing - check(routes(3), List(), Router.Result.Error.Exception(MyException))
      }
    }
  }
}
