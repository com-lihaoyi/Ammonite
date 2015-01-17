Ammonite 0.1.0
==============

```scala
import ammonite._

// Get the current working directory
val d = cwd

// Copy a file or folder
cp(d/'file, d/'file2)

// Make a folder named "folder"
mkdir! d/'folder

// Rename all .scala files inside the folder d into .java files
ls! d | mv{case r"$x.scala" => s"$x.java"}

// Line-count of all .scala files recursively in d
ls.rec! cwd |? (_.ext == "scala") | read || (_.split("\n")) length

// Find and concatenate all .js files directly in the working directory
ls! cwd |? (_.ext == "js") | read |> write! wd/'target/"bundle.js"
```

Ammonite is a library used to write rock-solid shell scripts using Scala. It provides a very lightweight way to perform common system housekeeping operations in Scala, without having to drop down to sketchy, fragile bash scripts. Ammonite aims to be safe enough to use large programs while concise enough to use from the Scala command-line.

In general, a Ammonite command to do a particular action should be not-much-longer than the equivalent bash command, but it is far safer due to Scala's type-safe nature and design principles (immutability, no-global-state, etc.). Ammonite also can serve as Scala's missing IO library, serving to replace the common mess of boilerplate:

```scala
def removeAll(path: String) = {
  def getRecursively(f: File): Seq[File] =
    f.listFiles.filter(_.isDirectory).flatMap(getRecursively) ++ f.listFiles
  getRecursively(new File(path)).foreach{f =>
    if (!f.delete())
      throw new RuntimeException("Failed to delete " + f.getAbsolutePath)}
  }
removeAll("target/thing")
```

With single, sleek command:

```scala
// Remove a file or folder target/thing
rm! wd/'target/'thing
```

Getting Started
===============

To begin, add the following to your build.sbt:

```scala
"com.lihaoyi" %% "ammonite" % "0.1.0",
```

Then at the top of your file,

```scala
import ammonite._
```

And you're all set!

Paths
=====

Ammonite uses strongly-typed data-structures to represent filesystem paths. The two basic versions are:

- `Path`: an absolute path, starting from the root
- `RelPath`: a relative path

Generally, almost all commands take absolute `Path`s. Absolute paths can be created in a few ways:

```scala
// Get the process' working directory
val wd = cwd

// A path nested inside `wd`
wd/'folder/'file

// A path with spaces or other special characters
wd/"My Folder"/"My File.txt"

// Up one level from the cwd
wd/up

// Up two levels from the cwd
wd/up/up
```

Not that there are no in-built operations to change the `cwd`. You can of course do so using Java APIs, but in general you should not need to. Simply defining a new path, e.g.

```scala
val target = cwd/'target
```

Should be sufficient for most needs. Two absolute `Path`s are pre-defined:

- `cwd`: the current working directory of your process. Note this can change over time, and `cwd` always reflects the current value.
- `root`: the root of the filesystem, traditionally represented as `/`


RelPaths
========

Relative `RelPath`s can be created in the following ways:

```scala
// The path "folder/file"
empty/'folder/'file
'folder/'file

// The path "file"
empty/'file

// The relative difference between two paths
val target = cwd/'target/'file
(target - cwd) // same as 'target/'file or "target/file"

// `up`s get resolved automatically
(cwd - target) // same as up/up or "../.."
```

In general, very few APIs take relative paths. Their main purpose is to be combined with absolute paths in order to create new absolute paths. e.g.

```scala
val target = cwd/'target/'file
val rel = target - cwd
val newBase = root/'code/'server
newBase/rel // root/'code/'server/'target/'file
```

`up` is a relative path that comes in-built:

```scala
val target = root/'target/'file
target/up // root/'target
```

Note that all paths are always expressed in a canonical manner:

```scala
(root/'folder/'file/up).toString
"/folder"
// not "/folder/file/.."

('folder/'file/up).toString
"folder"
// not "folder/file/.."
```

So you don't need to worry about canonicalizing your paths before comparing.

Extensions
==========

Ammonite comes with a bunch of extension methods that make everything much more concise. These turn Scala from a "relatively-concise" language into one as tight as Bash scripts, while still maintaining the high level of type-safety and maintainability that comes with Scala code.

Extensions of `Traversable`
---------------------------

These extensions apply to any `Traversable`: `Seq`s, `List`s, `Array`s, and others.

- `things | f` is an alias for `things map f`
- `things || f` is an alias for `things flatMap f`
- `things |? f` is an alias for `things filter f`
- `things |& f` is an alias for `things reduce f`

These should behave exactly the same as their implementations; their sole purpose is to make things more concise at the command-line.

`Pipeable`
----------

Ammonite brings with it the `|>` operator on all values, defined as:

```scala
implicit class Pipeable[T](t: T){
  def |>[V](f: T => V) = f(t)
}
```

It basically lets you write `a |> b` rather than `b(a)`. It does nothing more

Operations
==========

Ammonite comes with a set of built-in operations that mirrors much of what you could do in the Bash shell. The naming is similar, to what you'd use in Bash, except you call them via `op! arg` or `op(arg)` or `op(arg1, arg2)` instead of `op arg` and `op arg1 arg2` that you'd see in Bash.


```scala
// Make a directory
mkdir! cwd/'folder

// Move it
mv(cwd/'folder, cwd/'folder2)

// Copy it; recursive by default
cp(cwd/'folder2, cwd/'folder3)

// List the files in cwd
ls! cwd

// List the files in cwd recursively
ls.rec! cwd

// Remove one of them; recursive by default
rm! cwd/'folder2

// Write a file, if it doesn't exist
write(cwd/'folder2/"data.txt", "I am a cow")

// Write to a file, stomping over anything that exists
write.over(cwd/'folder2/"data.txt", "I am a cow")

// Append to a file
write.over(cwd/'folder2/"data.txt", "I am a cow")

// Read a file as a String
read! cwd/'folder2/"data.txt"

// Read a file as an Iterator[String]
read.lines! cwd/'folder2/"data.txt"

// Read a file as an Array[Byte]
read.bytes! cwd/'folder2/"data.bin"

// Check if a file or folder exists
exists! cwd/'folder2/"data.txt"
```

All of these operations are pre-defined and strongly typed, so feel free to jump to their implementation to look at what they do or what else is available.

In general, each operator has sensible/safe defaults: `rm` and `cp` are recursive, `rm` ignores the file if it doesn't exist, and all operations that create a file or folder (`mkdir`, `write`, `mv`) automatically create any necessary parent directories. `write` also does *not* stomp over existing files by default.

Chains
======

The real value of Ammonite is the fact that you can pipe things together as easily as you could in Bash. No longer do you need to write reams of boilerplate. to accomplish simple tasks. Some of these chains are listed at the top of this readme, here are a few more fun examples:

```scala
// Move all files inside the "py" folder out of it
ls! cwd/"py" | mv.all*{case d/"py"/x => d/x }

// Find all dot-files in the current folder
ls! cwd |? (_.last(0) == '.')

// Write the names of the 10 largest files in
// the current working directory to a file
ls.rec! cwd | (x => x.size -> x) sortBy (-_._1) take 10

// Sorted list of the most common words in your .scala source files
def txt = ls.rec! cwd |? (_.ext == "scala") | read
def frequencies(s: Seq[String]) = s groupBy (x => x) mapValues (_.length)
txt || (_.split("[^a-zA-Z0-9_]")) |> frequencies |> (_.toSeq) sortBy (-_._2)
```

Each of these would be a significant amount of ugly filesystem traversals, readings, and other operations. With Ammonite, it is often a single line.