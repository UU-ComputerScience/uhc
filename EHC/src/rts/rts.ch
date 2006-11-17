%%[8
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "config.h"
#include "utils.h"
#include "bits.h"
#include "grinbc/grinbc.h"
%%]

%%[8
typedef intptr_t GrWord;
typedef GrWord* Pointer;
%%]

%%[8
/* the empty PRIM define is used to mark exported functions from prim.c,
   used to automatically generate prim.h
*/
#define PRIM

#include "prim.h"
%%]

%%[8
#if USE_BOEHM_GC
#include "gc.h"
#else

#define HEAPSIZE 100000

extern Pointer HP;
extern Pointer Heap;
extern Pointer HeapEndCAF, HeapLimit;

#endif
%%]

%%[8

#ifndef HEAPALLOC_SIG_
#define HEAPALLOC_SIG_
GrWord heapalloc(int);
#endif /* HEAPALLOC_SIG_ */

%%]

%%[8
#define STACKSIZE 100000
#define RETURNSIZE 100

extern Pointer SP, RP, BP;
extern Pointer Stack, ReturnArea;
%%]

%%[8
extern GrWord global_False;
extern GrWord global_True;
%%]

%%[99
extern GrWord global_LT;
extern GrWord global_GT;
extern GrWord global_EQ;
%%]
