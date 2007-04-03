:- use_module( library(chr)).

handler class.

constraints c/1.


c(ord(A)) \ c(eq(A)) <=> true.
c(num(A)) \ c(ord(A)) <=> true.
c(num(A)) \ c(eq(A)) <=> true.



/*
%% Sample queries

c(num(v1)), c(eq(v1)).

*/

