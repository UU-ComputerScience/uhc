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

%%[98
#include <errno.h>
%%]

%%[97
#include "math.h"
#include "float.h"
#if USE_GMP
#include "gmp.h"
#endif

#ifndef FP_ZERO
#warning FP_ZERO not defined (assuming value 2). Using floating point numbers may give problems.
#define FP_ZERO 2
#endif
%%]

Internal config

%%[8.TRACE
#define TRACE 					1

#if TRACE
#define GB_COUNT_STEPS			1
#else
#define GB_COUNT_STEPS			0
#endif
%%]

%%[100 -8.TRACE
%%]

%%[8
#define INFO_EXITSTATE			1
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
typedef intptr_t GrWord;
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
extern Pointer HeapAreaLow;
extern Pointer HeapAreaHigh;

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

%%[8.DUMP_INTERNALS
#define DUMP_INTERNALS	1
%%]

%%[100 -8.DUMP_INTERNALS
%%]

%%[8
#if DUMP_INTERNALS
#  define	IF_INFO_IS(info)				( gb_Opt_Info & info )
#  define	IF_INFO_ON(info,x)				if ( IF_INFO_IS( info ) ) { x ; } else {}
#else
#  define	IF_INFO_IS(info)				0
#  define	IF_INFO_ON(info,x)
#endif

#define	IF_INFO_EXITSTATE_ON(x)				IF_INFO_ON(INFO_EXITSTATE,x)
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
