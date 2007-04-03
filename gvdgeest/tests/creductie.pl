:- module(class).
:- use_module(library(chr)).
:- chr_constraint eq/2, ord/2, real/2, ent/1.

%% class Eq, remove duplicates
   eq(T, N),   eq(T, M) <=>   eq(T, cons(1, fork(N, M))).

   eq(T, N),  ord(T, M) <=>  ord(T, cons(2, fork(N, M))).
  ord(T, N),  ord(T, M) <=>  ord(T, cons(3, fork(N, M))).

   eq(T, N), real(T, M) <=> real(T, cons(4, fork(N, M))).
  ord(T, N), real(T, M) <=> real(T, cons(5, fork(N, M))).
 real(T, N), real(T, M) <=> real(T, cons(6, fork(N, M))).

   eq(int    , N)       <=>  ent(N).
   eq(list(T), N)       <=>   eq(T, cons(7, N)).
  ord(list(T), N)       <=>  ord(T, cons(8, N)).
   eq(int    , N)       <=>  ent(N).
 real(int    , N)       <=>  ent(N).

