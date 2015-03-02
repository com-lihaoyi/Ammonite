package test.ammonite.ops

import ammonite.ops._
import utest._

object ExampleTests extends TestSuite{

  val tests = TestSuite {
    'longExample{
      import ammonite.ops._

      // Pick the directory you want to work with,
      // relative to the process working dir
      val wd = cwd/'ops/'target/"scala-2.11"/"test-classes"/'example2

      // Delete a file or folder, if it exists
      rm! wd

      // Copy a file or folder
      cp(wd/up/'example, wd)

      // Make a folder named "folder1"
      mkdir! wd/'folder1

      // List the current directory
      val listed = ls! wd

      // Write to a file without pain! Necessary
      // enclosing directories are created automatically
      write(wd/'dir2/"file1.scala", "package example\nclass Foo{}")
      write(wd/'dir2/"file2.scala", "package example\nclass Bar{}")

      // Rename all .scala files inside the folder d into .java files
      ls! wd/'dir2 | mv{case r"$x.scala" => s"$x.java"}

      // List files in a folder
      val renamed = ls! wd/'dir2

      // Line-count of all .java files recursively in wd
      val lineCount = ls.rec! wd |? (_.ext == "java") | read.lines | (_.size) sum

      // Find and concatenate all .java files directly in the working directory
      ls! wd/'dir2|? (_.ext == "java") | read |> write! wd/'target/"bundled.java"

      assert(
        listed == Seq(wd/"folder1"),
        lineCount == 4,
        renamed == Seq(wd/'folder2/"file1.java", wd/'folder2/"file2.java"),
        read(wd/'target/"bundled.java") == "package example\nclass Foo{}\npackage example\nclass Bar{}"
      )


      write(wd/'py/"cow.scala", "Hello World")
      write(wd/".file", "Hello")
      // Chains

      // Move all files inside the "py" folder out of it
      ls! wd/"py" | mv.all*{case d/"py"/x => d/x }

      // Find all dot-files in the current folder
      val dots = ls! wd |? (_.last(0) == '.')

      // Find the names of the 10 largest files in the current working directory
      ls.rec! wd | (x => x.size -> x) sortBy (-_._1) take 10

      // Sorted list of the most common words in your .scala source files
      def txt = ls.rec! wd |? (_.ext == "scala") | read
      def freq(s: Seq[String]) = s groupBy (x => x) mapValues (_.length) toSeq
      val map = txt || (_.split("[^a-zA-Z0-9_]")) |> freq sortBy (-_._2)

      assert(
        ls(wd).toSeq.contains(wd/"cow.scala"),
        dots == Seq(wd/".file"),
        map == Seq("Hello" -> 1, "World" -> 1)
      )
    }
    'comparison{
      rm! cwd/'target/'folder/'thing/'file
      write(cwd/'target/'folder/'thing/'file, "Hello!")

      def removeAll(path: String) = {
        def getRecursively(f: java.io.File): Seq[java.io.File] = {
          f.listFiles.filter(_.isDirectory).flatMap(getRecursively) ++ f.listFiles
        }
        getRecursively(new java.io.File(path)).foreach{f =>
          println(f)
          if (!f.delete())
            throw new RuntimeException("Failed to delete " + f.getAbsolutePath)
        }
        new java.io.File(path).delete
      }
      removeAll("target/folder/thing")

      assert(ls(cwd/'target/'folder).toSeq == Nil)

      write(cwd/'target/'folder/'thing/'file, "Hello!")

      rm! cwd/'target/'folder/'thing
      assert(ls(cwd/'target/'folder).toSeq == Nil)
    }

    'constructingPaths{
      // Get the process' Current Working Directory. As a convention
      // the directory that "this" code cares about (which may differ
      // from the cwd) is called `wd`
      val wd = cwd

      // A path nested inside `wd`
      wd/'folder/'file

      // A path starting from the root
      root/'folder/'file

      // A path with spaces or other special characters
      wd/"My Folder"/"My File.txt"

      // Up one level from the wd
      wd/up

      // Up two levels from the wd
      wd/up/up
    }
    'newPath{
      val target = cwd/'target
    }
    'relPaths{
      // The path "folder/file"
      val rel1 = 'folder/'file
      val rel2 = 'folder/'file

      // The path "file"; will get converted to a RelPath by an implicit
      val rel3 = 'file

      // The relative difference between two paths
      val target = cwd/'target/'file
      assert(target - cwd == 'target/'file)

      // `up`s get resolved automatically
      val minus = cwd - target
      val ups = up/up
      assert(minus == ups)
      rel1: RelPath
      rel2: RelPath
      rel3: RelPath
    }
    'relPathCombine{
      val target = cwd/'target/'file
      val rel = target - cwd
      val newBase = root/'code/'server
      assert(newBase/rel == root/'code/'server/'target/'file)
    }
    'relPathUp{
      val target = root/'target/'file
      assert(target/up == root/'target)
    }
    'canonical{
      assert((root/'folder/'file/up).toString == "/folder")
      // not "/folder/file/.."

      assert(('folder/'file/up).toString == "folder")
      // not "folder/file/.."
    }
    'findWc{
      val wd = cwd/'ops/'src/'test/'resources/'testdata

      // find . -name '*.txt' | xargs wc -l
      val lines = ls.rec(wd) |? (_.ext == "txt") | read.lines | (_.length) sum

      assert(lines == 20)
    }
    'addUpScalaSize{
      ls.rec! cwd |? (_.ext == "scala") | (_.size) |& (_ + _)
    }
    'concatAll{
      ls.rec! cwd |? (_.ext == "scala") | read |> write! cwd/'target/'test/"omg.txt"
    }

    'rename{
//      val d1/"omg"/x1 = wd
//      val d2/"omg"/x2 = wd
//      ls! wd |? (_.ext == "scala") | (x => mv! x ! x.pref)
    }
    'allSubpathsResolveCorrectly{
      for(abs <- ls.rec! cwd){
        val rel = abs - cwd
        assert(rel.ups == 0)
        assert(cwd / rel == abs)
      }
    }

    'pprint{
      import ammonite.pprint.Config.Defaults._

      assert(
        ammonite.pprint.PPrint(root/'hello/'world).mkString == "root/'hello/'world",
        ammonite.pprint.PPrint('hello/'world).mkString == "'hello/'world",
        ammonite.pprint.PPrint(empty/'world).mkString == "'world",
        ammonite.pprint.PPrint(empty/'hello/'world).mkString == "'hello/'world",
        ammonite.pprint.PPrint(empty/"hello world").mkString == "\"hello world\""
      )

    }
  }
}
