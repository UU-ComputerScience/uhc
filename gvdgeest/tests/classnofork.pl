:- module(class).
:- use_module(library(chr)).
:- chr_constraint eq/2, ord/2, prove/1, assume/1, ent/1.

%% class Eq, remove duplicates
%prove(eq(A, N)), prove(eq(A, M)) <=> prove(eq(A, cons(r1, fork(N, M)))).

%% instances Eq Int, Eq Bool, Eq List
prove(eq(int, N))     <=> ent(cons(r2, N)).
prove(eq(char, N))    <=> ent(cons(r3, N)).
prove(eq(list(A), N)) <=> prove(eq(A, cons(r4, N))).

%% class Ord, subclass Eq, remove duplicates
%prove(ord(A, N)), prove(eq(A, M))  <=> prove(ord(A, cons(r5, fork(N, M)))).
%prove(ord(A, N)), prove(ord(A, M)) <=> prove(ord(A, cons(r6, fork(N, M)))).

prove(ord(A, N)), prove(eq(A, M))  <=> prove(ord(A, N)), prove(ord(A, cons(r5, M))).

prove(ord(int, N))     <=> ent(cons(r7, N)).
prove(ord(char, N))    <=> ent(cons(r8, N)).
prove(ord(list(A), N)) <=> prove(ord(A, cons(r9, N))).

assume(ord(A, N))      ==> assume(eq(A, cons(r10, N))).

assume(ord(A, N)), prove(ord(A, M)) <=> assume(ord(A, N)), ent(cons(N, M)).
assume(eq(A, N)), prove(eq(A, M)) <=> assume(eq(A, N)), ent(cons(N, M)).
