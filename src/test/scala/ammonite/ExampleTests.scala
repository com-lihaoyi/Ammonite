package ammonite

import ammonite.BasePath.NoRelativePathException
import ammonite.Path._
import ammonite.RelPath._
import ammonite._
import utest._

import scala.collection.{TraversableLike, GenTraversableOnce}
import scala.collection.generic.{CanBuildFrom => CBF}

object ExampleTests extends TestSuite{

  val tests = TestSuite { 
    var wd = cwd
    'cd{
      // cd /usr/local/bin/
      wd /= 'user/'local/'bin

      assert(wd - cwd == 'user/'local/'bin)
    }
    'findWc{
      // find . -name '*.txt' | xargs wc -l
      wd /= 'src/'test/'resources/'testdata
      val lines = 
        ls.rec(wd) |? (_.ext == "txt") | read.lines | (_.length) sum
      
      assert(lines == 14)
    }
    'addUpScalaSize{
      ls.rec! cwd |? (_.ext == "scala") | meta | (_.size) |& (_ + _)
    }
    'concatAll{
      ls.rec! wd |? (_.ext == "scala") | read |> write! wd/'target/"omg.txt"
    }
  }
}
