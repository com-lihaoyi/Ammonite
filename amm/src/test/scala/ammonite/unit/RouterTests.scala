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
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    val grouped = groupArgs(input)
    entryPoint.invoke(base, grouped)
  }
  def check[B, T](base: B,
                  entryPoint: EntryPoint[B],
                  input: List[String],
                  expected: Router.Result[T]) = {
    val result = parseInvoke(base, entryPoint, input)
    assert(result == expected)
  }
  val tests = Tests {
    println("UnitTests")
    test("transpose"){
      // Make sure this doesn't stack-overflow
      val inner = List.fill(1000000)(1)
      assert(Util.transpose(List.fill(10)(inner)) == List.fill(1000000)(List.fill(10)(1)))
    }
    test("router"){
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
      val routes = Router.generateRoutes[Target.type]

      test("basicModelling"){
        val names = routes.map(_.name)
        assert(
          names == List("foo", "bar", "qux", "ex", "pureVariadic", "mixedVariadic")
        )
        val evaledArgs = routes.map(_.argSignatures.map{
          case Router.ArgSig(name, tpe, docs, None) => (name, tpe, docs, None)
          case Router.ArgSig(name, tpe, docs, Some(default)) =>
            (name, tpe, docs, Some(default(Target)))
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


      test("invoke"){
        check(Target, routes(0), List(), Router.Result.Success(1))
        check(Target, routes(1), List("2"), Router.Result.Success(2))
        check(Target, routes(1), List("--i", "2"), Router.Result.Success(2))
        check(Target, routes(2), List("2"), Router.Result.Success("lolslols"))
        check(Target, routes(2), List("--i", "2"), Router.Result.Success("lolslols"))
        check(Target, routes(2), List("3", "x"), Router.Result.Success("xxx"))
        check(Target, routes(2), List("--i", "3", "x"), Router.Result.Success("xxx"))
        check(Target, routes(2), List("--i", "3", "--s", "x"), Router.Result.Success("xxx"))
      }
      test("varargs"){
        test("happyPathPasses"){
          check(Target, routes(4), List("1", "2", "3"), Router.Result.Success(6))
          check(Target, routes(5), List("1", "2", "3", "4", "5"), Router.Result.Success("12345"))
        }
        test("emptyVarargsPasses"){
          check(Target, routes(4), List(), Router.Result.Success(0))
          check(Target, routes(5), List("1"), Router.Result.Success("1"))
        }
        test("varargsAreAlwaysPositional"){
          val invoked = parseInvoke(Target, routes(4), List("--nums", "31337"))
          assertMatch(invoked){
            case InvalidArguments(List(
              ParamError.Invalid(
                ArgSig("nums", "Int*", _, _),
                "--nums",
                _: NumberFormatException
              )
            ))=>
          }
          check(
            Target, routes(5), List("1", "--args", "foo"),
            Router.Result.Success("1--argsfoo")
          )

          assertMatch(parseInvoke(Target, routes(4), List("1", "2", "3", "--nums", "4"))){
            case InvalidArguments(List(
              ParamError.Invalid(
                ArgSig("nums", "Int*", _, _),
                "--nums",
                _: NumberFormatException
              )
            ))=>
          }
        }

        test("notEnoughNormalArgsStillFails"){
          assertMatch(parseInvoke(Target, routes(5), List())){
            case MismatchedArguments(List(ArgSig("first", _, _, _)), Nil, Nil, None) =>
          }
        }
        test("multipleVarargParseFailures"){
          assertMatch(parseInvoke(Target, routes(4), List("aa", "bb", "3"))){
            case InvalidArguments(
            List(
            ParamError.Invalid(ArgSig("nums", "Int*", _, _), "aa", _: NumberFormatException),
            ParamError.Invalid(ArgSig("nums", "Int*", _, _), "bb", _: NumberFormatException)
            )
            )=>
          }
          assertMatch(parseInvoke(Target, routes(5), List("aa", "bb", "3"))){
            case InvalidArguments(
            List(
            ParamError.Invalid(ArgSig("first", "Int", _, _), "aa", _: NumberFormatException)
            )
            )=>
          }
        }
      }

      test("failures"){
        test("missingParams"){
          assertMatch(parseInvoke(Target, routes(1), List.empty)){
            case MismatchedArguments(List(ArgSig("i", _, _, _)), Nil, Nil, None) =>
          }
          assertMatch(parseInvoke(Target, routes(2), List("--s", "omg"))){
            case MismatchedArguments(List(ArgSig("i", _, _, _)), Nil, Nil, None) =>
          }
        }
        test("invalidParams") - assertMatch(parseInvoke(Target, routes(1), List("lol"))){
          case InvalidArguments(
          List(ParamError.Invalid(ArgSig("i", _, _, _), "lol", _))
          ) =>
        }

        test("tooManyParams") - check(
          Target, routes(0), List("1", "2"),
          MismatchedArguments(Nil, List("1", "2"), Nil, None)
        )


        test("redundantParams"){
          val parsed = parseInvoke(Target, routes(2), List("1", "--i", "2"))
          assertMatch(parsed){
            case MismatchedArguments(
              Nil, Nil, Seq((ArgSig("i", _, _, _), Seq("1", "2"))), None
            ) =>
          }
        }
        test("failing") - check(Target, routes(3), List(), Router.Result.Error.Exception(MyException))
      }
    }
  }
}
