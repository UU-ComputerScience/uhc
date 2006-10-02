#include <stdio.h>
#include "config.h"

typedef unsigned int GrWord;
typedef GrWord* Pointer;

#include "prim.h"

#if USE_BOEHM_GC
#include "gc.h"
#else

#define HEAPSIZE 100000

extern Pointer HP;
extern Pointer Heap;
extern Pointer HeapEndCAF, HeapLimit;

#endif

#define STACKSIZE 100000
#define RETURNSIZE 100

extern Pointer SP, RP, BP;
extern Pointer Stack, ReturnArea;


extern GrWord global_False;
extern GrWord global_True;
extern GrWord global_LT;
extern GrWord global_GT;
extern GrWord global_EQ;
