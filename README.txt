Hi there,

If you're reading this you're probably wondering how to build this typechecker.

Well first things first, go into the EHC folder and build UHC variant 8.
You don't actually need to build the whole compiler just aslong 
as EH-Utils and the EH8 libs are build.

./configure
./make 8/ehclibs

after that enter de HML folder, and you first need to compile the .rul file

ruler-core --nodata HML.rul

This will produce an HML.hs file.

Next you have two options:

Typechecker.hs -- build this to get a type checker on files. Give it a file, 
                  it'll give you the type of Main or an error if any function
                  inside the module failed to type.
                  
Terminal.hs    -- This implements a crude terminal. Give it expressions and it'll
                  give you types. To add "build in" functions, edit the TestLude.hs file
                  you can use the foreign import statements to import basic prelude types.

A couple of interesting files also:

Utils.hs      -- contains a lot of utility functions, among others trace. Here is where you
                 can turn on and off debug tracing by commenting and uncommenting the line
                 from trace = flip const to trace = Debug.trace
                 
                 A few things to note, there's a simplifier here to remove superflous parenthesis. 
                 Sometimes it's a bit eager, so keep this in mind. Some types may look incorrect but
                 it's just the simplifier at the end. You can disable this by modifying
                 the Simplify class instances.

TestLude.hs   -- the "prelude" used in the Terminal. You can edit this file to define more build in functions

As to how to interpret the results:

> single id
---- Result --------------------------------
Infered Types: forall a . List a -> a
               forall (a96>=forall (a2>=_|_). a2 -> a2). List a96
Environment  :
Cleaned Env. :
Prefixes used:
Fresh varsc  : 387
--------------------------------------------

the environment and prefixes should all always be empty, unless something went wrong, 
in which case it'll show what wasn't properly scoped. The fresh variable count it larely irrelevant
which leaves the  Types.

The first type is a possible SystemF instantiation for the Type Scheme inferred, which is on the second line.
The second line is what is subsequently used for any further typechecking.
