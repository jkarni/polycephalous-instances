# Polycephalous instances

Make stuff likes this:

     instance Show a => Print a where
       print x = putStrLn (show x)
     instance           Print a where
       print x = putStrLn "No show method"

Work.

## Notes

This is really just Template Haskell; all the ideas are from Oleg Kiselyov and
Simon Peyton-Jones. See [this page](http://okmij.org/ftp/Haskell/TypeClass.html#class-based-overloading) 
for more.
