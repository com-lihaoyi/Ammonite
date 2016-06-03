Predef
======

Ammonite has the concept of a "Predef": Code you can run before entering the 
REPL, and provides all sorts of useful things:

- Import the implicits necessary for the REPL to run at all (pretty-printing 
  code and types)
  
- Loading "common" modules that you want available: `ammonite-shell` to make the
  REPL behave like a systems shell,
   
- Initialize common definitions *the end-user* of Ammonite 