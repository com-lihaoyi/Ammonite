Layering
========

There are many classes involved in the Ammonite REPL that can conceivably be
thought of as "the thing which runs your code". This diagram roughly breaks
down the relationship between these classes:

```
        Main
         |
         |
         |
        Repl-------------
         |               |
         |              FrontEnd
         |
        Interpreter-----------------------------------
         |               |              |             |
         |              Compiler       Pressy        Preprocessor
         |
        Evaluator
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