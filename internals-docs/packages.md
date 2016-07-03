Packages
========

Like most JVM programs, Ammonite is split into separate packages. Unlike
most of them, it also contains some synthetic packages that house user code
compiled and run within the Ammonite REPL.

The main static packages containing Ammonite code are:
 
- `ammonite.ops`: a standalone usable filesystem library that feel great to
  use from the REPL, but can be used as part of any other program
  
- `ammonite.terminal`: a standalone replacement for JLine, with more features
  such as syntax-highlighting, GUI-style keybindings and good multiline editing
  
- `ammonite`: the main Ammonite REPL

- `ammonite.shell`: an extension package for the Ammonite REPL to configure it 
  for use as a system shell: a cwd-based-prompt, file-path-autocomplete, etc.
  
- `ammonite.sshd`: spin up an Ammonite server in any existing process you can
  connect to via SSH
  
Each of these has its own unit tests, along with the integration suite in

- `ammonite.integration`: tests that package up the Ammonite jar, spawn 
  Ammonite subprocess via the command-line interface and ensure the whole
  thing works end-to-end. This has no published artifact
  
Lastly, there are the packages which aren't visible in the source code but 
contain code that is generated at runtime by Ammonite

- `$sess`: contains the wrapper-objects that contains any code 
  a user enters into the REPL. Any values, functions, classes, etc. you write
  into the REPL are wraNpped in a wrapper object and placed in this package
  
- `$file`: contains the code that results from loading Ammonite
  script files into the REPL, or running them using Ammonite from the 
  command-line. Any scripts are placed in `$file.foo.bar.baz`
  for a script in folder `foo/bar/baz/` relative to the current working
  directory; scripts outside the current working directory have the 
  corresponding number of `..` packages e.g. `$file.`..`.foo.bar`
  to represent a script in the folder `../foo/bar/` relative to the working
  directory.