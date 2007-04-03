:- module(class).
:- use_module(library(chr)).
:- chr_constraint eq/2, ord/2, real/2, sub/1.

 eq(A, N), eq(A, M)    <=> eq(A, cons(1, fork(N, M))).
 eq(A, N) ==> sub(ord(A, cons(2, N))).

 ord(A, N), ord(A, M)  <=> ord(A, cons(3, fork(N, M))).
 ord(A, N) ==> sub(real(A, cons(4, N))).

 real(A, N), real(A, M) <=> real(A, cons(5, fork(N, M))).
