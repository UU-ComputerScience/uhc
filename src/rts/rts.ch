%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include "config.h"
%%]

%%[97
#if USE_GMP
#include "gmp.h"
#endif
%%]

Internal config

%%[8
#define TRACE 					1

#if TRACE
#define GB_COUNT_STEPS			1
#else
#define GB_COUNT_STEPS			0
#endif
%%]

%%[8
#include "utils.h"
#include "bits.h"
#include "grinbc/grinbc.h"
#include "grinbc/gbprim.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef uintptr_t GrWord;
typedef GrWord* Pointer;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
/* the empty PRIM define is used to mark exported functions from prim.c,
   used to automatically generate prim.h
*/
#define PRIM

#include "prim.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack, heap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if USE_BOEHM_GC
#include "gc.h"
#else

#define HEAPSIZE 1000000

extern Pointer HP;
extern Pointer Heap;
extern Pointer HeapLimit;

#endif
%%]

%%[8
#if USE_BOEHM_GC
#define heapalloc(sz)	Cast(GrWord,GC_MALLOC(sz*sizeof(GrWord)))
#else
GrWord heapalloc(int);
#endif
%%]

%%[8
#define STACKSIZE 200000
#define RETURNSIZE 100

extern Pointer SP, RP;
extern Pointer Stack, ReturnArea;

extern Pointer StackAreaHigh, StackAreaLow ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Globals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern GrWord global_False;
extern GrWord global_True;
%%]

%%[8
extern GrWord global_LT;
extern GrWord global_GT;
extern GrWord global_EQ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry points
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern int main_Sil_Init1(int argc, char** argv) ;
extern int main_Sil_Run(int argc, char** argv) ;
extern int main_Sil_Exit(int argc, char** argv) ;

extern int main_GB_Init1(int argc, char** argv, int* nRtsOpt) ;
extern int main_GB_Run(int argc, char** argv, GB_BytePtr initPC, GB_Word initCAF) ;
extern int main_GB_Exit(int argc, char** argv) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dumping internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define DUMP_INTERNALS	1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define TRACE_LEV_DFLT 	3

#if TRACE
extern int traceLevel ;
#  define	IF_TR_ON(l,x)			if ( l <= traceLevel ) { x ; } else {}
#else
#  define	IF_TR_ON(l,x)
#endif

%%]

%%[8
%%]
