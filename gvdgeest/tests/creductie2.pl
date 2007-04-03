:- module(class).
:- use_module(library(chr)).
:- chr_constraint eq/1, ord/1, real/1.

%% class Eq, remove duplicates
   ord(T) \ eq(T) <=>  true.
  ord(T) ==> eq(T).

  real(T) \ ord(T) <=> true.
 real(T) ==> ord(T).

   eq(int)           <=>  true.
   eq(list(T))       <=>   eq(T).
  ord(list(T))       <=>  ord(T).
   eq(int    )       <=>  true.
 real(int    )       <=>  true.

