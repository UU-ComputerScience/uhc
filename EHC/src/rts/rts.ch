%%[8
#include <stdio.h>
#include "config.h"
%%]

%%[8
typedef unsigned int GrWord;
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
