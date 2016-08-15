Layering
========

The Ammonite codebase is laid out in the following modules, with arrows
representing dependencies:

```
            +-- ammRuntime <----------------+          +---------- shell
            |        ^                      |          |
            |        |                      |          |
            |        +-----+                |          |
            |               |               |          |
 ammUtil <--+--------- ammCompiler <--------+-- amm <--+---------- sshd
            |               ^               |
            |               |               |
            |               +-----+         |
            |                     |         |
            +------------------- ammRepl <--+
                                  |
                                  |
                                  |
                                  |
                   terminal <-----+

```

`amm` is the entry point for the "main" Ammonite REPL. Internally it is
modularized into submodules to help maintain the layering, and e.g. avoid
accidental use of unnecessary APIs (and paying their classloading/initialization
cost) in the core codepaths.

- `ammUtil`: basic data-structures and logic common throughout the codebase

- `ammRuntime`: everything necessary to run an Ammonite Scala Script that has
  already been compiled and cached. This code is the "critical path" for using
  Ammonite to run slow-changing scripts (i.e. most of them) and should be fast
  and without heavy dependencies like `scala-compiler`. Only provides a core
  `InterpAPI` for scripts to call, and is without the rich `ReplAPI` for use
  in the REPL

- `ammCompiler`: everything necessary to run an Ammonite Scala Script that
  has *not* been compiled and cached; includes `scala-compiler` and `fastparse`
  and all the code necessary to preprocess Scala source code and compile it
  into Java bytecode. Does not contain any REPL-specific functionality

- `ammRepl`: everything necessary to run an Ammonite REPL that takes in stdin
  and prints to stdout; includes JLine, `ammonite-terminal`, REPL-specific
  `ReplAPI`s, and other code specific to interactive REPL work

- `amm`: contains the Ammonite's main entry-points: for the REPL,
  script-runner, debugger (same as REPL), etc. and associated code for
  marshalling command-line script arguments into the Ammonite's main methods.

There are many classes involved in the Ammonite REPL that can conceivably be
thought of as "the thing which runs your code". This diagram roughly breaks
down the relationship between these classes:

```
amm:             ----- Main
                |       |
                |       |
                |       |
                |       v
ammRepl         |      Repl ------------
                |       |               |
                |       |               v
                |       |              FrontEnd
                |       v
ammCompiler:     ----> Interpreter ----------------------------------
                        |               |              |             |
                        |               v              v             v
                        |              Compiler       Pressy        Preprocessor
                        v
ammRuntime:            Evaluator
```

The distribution of responsibilities is

- `Evaluator`: runs Java bytecode

  Manages classloaders, caching, etc. to make that happen

- `Interpreter`: runs Scala source code
  
  Made up of `Evaluator` + `Compiler` (and `Pressy`). Runs source code by 
  transforming it via `Preprocessor`, compiling it to bytecode via `Compiler` 
  and sending it to `Evaluator` to execute 
 
- `Repl`: runs user-input

  Made up of `Interpreter` + `FrontEnd`, handles the full pipeline from taking
  user input at the command prompt to executing it 

- `Main`: a nicer API/CLI around `Repl`

  Provides the nice external-API and CLI in a separate place from all the 
  messy `Repl` internals
  
  
This is the ideal layering that we want to achieve. It's likely that the 
current implementation does not entirely line up with this, and there is code
living in places it shouldn't, but over time we should try to move it to this
layering.