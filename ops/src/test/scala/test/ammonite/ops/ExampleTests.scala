package test.ammonite.ops

import java.nio.file.attribute.{GroupPrincipal, FileTime}

import ammonite.ops._
import utest._

object ExampleTests extends TestSuite{

  val tests = Tests {
    'reference{
      import ammonite.ops._

      // Let's pick our working directory
      val wd: Path = pwd/'ops/'target/"scala-2.11"/"test-classes"/'example3

      // And make sure it's empty
      rm! wd
      mkdir! wd

      // Reading and writing to files is done through the read! and write!
      // You can write `Strings`, `Traversable[String]`s or `Array[Byte]`s
      write(wd/"file1.txt", "I am cow")
      write(wd/"file2.txt", Seq("I am cow\n", "hear me moo"))
      write(wd/'file3, "I weigh twice as much as you".getBytes)

      // When reading, you can either `read!` a `String`, `read.lines!` to
      // get a `Vector[String]` or `read.bytes` to get an `Array[Byte]`
      read! wd/"file1.txt"        ==> "I am cow"
      read! wd/"file2.txt"        ==> "I am cow\nhear me moo"
      read.lines! wd/"file2.txt"  ==> Vector("I am cow", "hear me moo")
      read.bytes! wd/"file3"      ==> "I weigh twice as much as you".getBytes

      // These operations are mirrored in `read.resource`,
      // `read.resource.lines` and `read.resource.bytes` to conveniently read
      // files from your classpath:
      val resourcePath = resource/'test/'ammonite/'ops/'folder/"file.txt"
      read(resourcePath).length        ==> 18
      read.bytes(resourcePath).length  ==> 18
      read.lines(resourcePath).length  ==> 1

      // You can read resources relative to any particular class, including
      // the "current" class by passing in `getClass`
      val relResourcePath = resource(getClass)/'folder/"file.txt"
      read(relResourcePath).length        ==> 18
      read.bytes(relResourcePath).length  ==> 18
      read.lines(relResourcePath).length  ==> 1

      // By default, `write` fails if there is already a file in place. Use
      // `write.append` or `write.over` if you want to append-to/overwrite
      // any existing files
      write.append(wd/"file1.txt", "\nI eat grass")
      write.over(wd/"file2.txt", "I am cow\nHere I stand")

      read! wd/"file1.txt"        ==> "I am cow\nI eat grass"
      read! wd/"file2.txt"        ==> "I am cow\nHere I stand"

      // You can create folders through `mkdir!`. This behaves the same as
      // `mkdir -p` in Bash, and creates and parents necessary
      val deep = wd/'this/'is/'very/'deep
      mkdir! deep
      // Writing to a file also creates necessary parents
      write(deep/'deeeep/"file.txt", "I am cow")

      // `ls` provides a listing of every direct child of the given folder.
      // Both files and folders are included
      ls! wd    ==> Seq(wd/"file1.txt", wd/"file2.txt", wd/'file3, wd/'this)

      // `ls.rec` does the same thing recursively
      ls.rec! deep ==> Seq(deep/'deeeep, deep/'deeeep/"file.txt")

      // You can move files or folders with `mv` and remove them with `rm!`
      ls! deep  ==> Seq(deep/'deeeep)
      mv(deep/'deeeep, deep/'renamed_deeeep)
      ls! deep  ==> Seq(deep/'renamed_deeeep)

      // `mv.into` lets you move a file into a
      // particular folder, rather than to particular path
      mv.into(deep/'renamed_deeeep/"file.txt", deep)
      ls! deep/'renamed_deeeep ==> Seq()
      ls! deep  ==> Seq(deep/"file.txt", deep/'renamed_deeeep)

      // `mv.over` lets you move a file to a particular path, but
      // if something was there before it stomps over it
      mv.over(deep/"file.txt", deep/'renamed_deeeep)
      ls! deep  ==> Seq(deep/'renamed_deeeep)
      read! deep/'renamed_deeeep ==> "I am cow" // contents from file.txt

      // `rm!` behaves the same as `rm -rf` in Bash, and deletes anything:
      // file, folder, even a folder filled with contents
      rm! deep/'renamed_deeeep
      rm! deep/"file.txt"
      ls! deep  ==> Seq()

      // You can stat paths to find out information about any file or
      // folder that exists there
      val info = stat! wd/"file1.txt"
      info.isDir  ==> false
      info.isFile ==> true
      info.size   ==> 20

      // Ammonite provides an implicit conversion from `Path` to
      // `stat`, so you can use these attributes directly
      (wd/"file1.txt").size ==> 20

      // You can also use `stat.full` which provides more information
      val fullInfo = stat.full(wd/"file1.txt")
      fullInfo.ctime: FileTime
      fullInfo.atime: FileTime
      fullInfo.group: GroupPrincipal
      ()
    }
    'longExample{
      import ammonite.ops._

      // Pick the directory you want to work with,
      // relative to the process working dir
      val wd = pwd/'ops/'target/"scala-2.11"/"test-classes"/'example2

      // Delete a file or folder, if it exists
      rm! wd

      // Make a folder named "folder"
      mkdir! wd/'folder

      // Copy a file or folder to a particular path
      cp(wd/'folder, wd/'folder1)
      // Copy a file or folder *into* another folder at a particular path
      // There's also `cp.over` which copies it to a path and stomps over
      // anything that was there before.
      cp.into(wd/'folder, wd/'folder1)


      // List the current directory
      val listed = ls! wd

      // Write to a file without pain! Necessary
      // enclosing directories are created automatically
      write(wd/'dir2/"file1.scala", "package example\nclass Foo{}\n")
      write(wd/'dir2/"file2.scala", "package example\nclass Bar{}\n")

      // Rename all .scala files inside the folder d into .java files
      ls! wd/'dir2 | mv{case r"$x.scala" => s"$x.java"}

      // List files in a folder
      val renamed = ls! wd/'dir2

      // Line-count of all .java files recursively in wd
      val lineCount = ls.rec! wd |? (_.ext == "java") | read.lines | (_.size) sum

      // Find and concatenate all .java files directly in the working directory
      ls! wd/'dir2 |? (_.ext == "java") | read |> (write(wd/'target/"bundled.java", _))

      assert(
        listed == Seq(wd/'folder, wd/'folder1),
        ls(wd/'folder1) == Seq(wd/'folder1/'folder),
        lineCount == 4,
        renamed == Seq(wd/'dir2/"file1.java", wd/'dir2/"file2.java"),
        read(wd/'target/"bundled.java") ==
        "package example\nclass Foo{}\npackage example\nclass Bar{}\n"
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
      rm! pwd/'target/'folder/'thing/'file
      write(pwd/'target/'folder/'thing/'file, "Hello!")

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

      assert(ls(pwd/'target/'folder).toSeq == Nil)

      write(pwd/'target/'folder/'thing/'file, "Hello!")

      rm! pwd/'target/'folder/'thing
      assert(ls(pwd/'target/'folder).toSeq == Nil)
    }

    'constructingPaths{
      // Get the process' Current Working Directory. As a convention
      // the directory that "this" code cares about (which may differ
      // from the pwd) is called `wd`
      val wd = pwd

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
      val target = pwd/'target
    }
    'relPaths{
      // The path "folder/file"
      val rel1 = 'folder/'file
      val rel2 = 'folder/'file

      // The path "file"; will get converted to a RelPath by an implicit
      val rel3 = 'file

      // The relative difference between two paths
      val target = pwd/'target/'file
      assert((target relativeTo pwd) == 'target/'file)

      // `up`s get resolved automatically
      val minus = pwd relativeTo target
      val ups = up/up
      assert(minus == ups)
      rel1: RelPath
      rel2: RelPath
      rel3: RelPath
    }
    'relPathCombine{
      val target = pwd/'target/'file
      val rel = target relativeTo pwd
      val newBase = root/'code/'server
      assert(newBase/rel == root/'code/'server/'target/'file)
    }
    'relPathUp{
      val target = root/'target/'file
      assert(target/up == root/'target)
    }
    'canonical - {if (Unix()){

      assert((root/'folder/'file/up).toString == "/folder")
      // not "/folder/file/.."

      assert(('folder/'file/up).toString == "folder")
      // not "folder/file/.."
    }}
    'findWc{
      val wd = pwd/'ops/'src/'test/'resources/'testdata

      // find . -name '*.txt' | xargs wc -l
      val lines = ls.rec(wd) |? (_.ext == "txt") | read.lines | (_.length) sum

      assert(lines == 20)
    }
    'addUpScalaSize{
      ls.rec! pwd |? (_.ext == "scala") | (_.size) |& (_ + _)
    }
    'concatAll{if (Unix()){
      ls.rec! pwd |? (_.ext == "scala") | read |> (write(pwd/'target/'test/"omg.txt", _))
    }}

    'noLongLines{
      // Ensure that we don't have any Scala files in the current working directory
      // which have lines more than 100 characters long, excluding generated sources
      // in `src_managed` folders.

      def longLines(p: Path) =
        (p, read.lines(p).zipWithIndex |? (_._1.length > 100) | (_._2))

      val filesWithTooLongLines = (
        %%("git", "ls-files")(ammonite.ops.pwd).out.lines
            | (Path(_, ammonite.ops.pwd))
            |? (_.ext == "scala")
            | longLines
            |? (_._2.length > 0)
            |? (!_._1.segments.contains("src_managed"))
      )

      assert(filesWithTooLongLines.length == 0)
    }
    'rename{
//      val d1/"omg"/x1 = wd
//      val d2/"omg"/x2 = wd
//      ls! wd |? (_.ext == "scala") | (x => mv! x ! x.pref)
    }
    'allSubpathsResolveCorrectly{
      for(abs <- ls.rec! pwd){
        val rel = abs relativeTo pwd
        assert(rel.ups == 0)
        assert(pwd / rel == abs)
      }
    }
  }
}
