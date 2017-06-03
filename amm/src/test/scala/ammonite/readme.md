The tests in this folder are roughly broken down into:

- `unit` tests: small, self-contained functions or utilities

- `interp` tests: instantiating an `Interpreter` object, then poking at its
  internals and running its functionality
  
- `session` tests: simulating a REPL session, running multiple commands one 
  after another and checking that the output matches what you'd expect
  
- `main` tests: tests running Ammonite's `main0` method, which is basically 
  equivalent to spawning a new Ammonite process, except the logic is run 
  in-process with mock `stdin`/`stdout`/`stderr` streams.
  
These are listed in increasing levels of "integration"-ness; anything more
integrated than this would be in the separate integration/ test subproject,
which literally spins up a separate Ammonite process from the jar file and
makes sure everything works end-to-end