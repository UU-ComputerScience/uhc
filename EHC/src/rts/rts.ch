%%[8
#ifndef __RTS_H__
#define __RTS_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "rtsbase.h"
#include "base/types.h"
#include "event/event.h"
#include "mm/mm.h"
%%]

%%[98
#include <errno.h>
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

#ifdef __UHC_TARGET_BC__
#include "bc/prim-const.h"
#else
#include "C/prim-const.h"
#endif

#include "priminline.h"
#include "primdecl.h"

#if defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_BC__)
#include "prim-shared.h"
#endif

#ifdef __UHC_TARGET_C__
#include "C/prim.h"
#endif


#ifdef __UHC_TARGET_BC__
#include "bc/primdecl.h"
%%[[99
#include "bc/prim-array.h"
#include "bc/prim-thread.h"
%%]]
#include "bc/prim.h"
%%[[97
#include "bc/prim-integer.h"
%%]]
%%[[98
#include "bc/prim-handle.h"
%%]]
#endif


// the empty PRIM define is used to mark exported functions from prim.c, used to automatically generate prim.h
#define PRIM

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
%%]
// Hack around absence of FP_ZERO on some platforms.
// Assume IEEE-754, check zero-ness of IEEE-754 float/double using encoding (see e.g. http://babbage.cs.qc.edu/courses/cs341/IEEE-754references.html).
// Is zero when exp == 0 && mant == 0, so shift out sign (most sign bit) and rest must be all zero to be a FP zero.

#define fp_iszero(x)	( sizeof(x) == sizeof(float) ? ((x)&(1<<31)) == 0 : ((x)&(1<<63)) == 0 )




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack, heap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if USE_BOEHM_GC
#include "gc/gc.h"
#elif USE_EHC_MM
#else

#define HEAPSIZE 100000000

extern WPtr HP;
extern WPtr HeapAreaLow;
extern WPtr HeapAreaHigh;
#endif
%%]

%%[8
#if USE_BOEHM_GC
#	define heapalloc(sz)                Cast(Word,GC_MALLOC(sz*sizeof(Word)))
#	define heapalloc_uncollectable(sz)  Cast(Word,GC_MALLOC_UNCOLLECTABLE(sz*sizeof(Word)))
#elif USE_EHC_MM
#	define heapalloc(sz)                Cast(Word,mm_itf_alloc(sz*sizeof(Word),0))
#	define heapalloc_uncollectable(sz)  Cast(Word,mm_itf_allocResident(sz*sizeof(Word)))
#else
	Word heapalloc(int);
#	define heapalloc_uncollectable(sz)  heapalloc(sz)
#endif
%%]

%%[8
#define STACKSIZE 					0x400000 // 0x400000
#define STACKSIZE_SPARE_UNUSED 		0x100		/* part of stack which is left as unused spare, but can be used by exception handling */
#define RETURNSIZE 100

extern WPtr SP, RP;
extern WPtr Stack, ReturnArea;
extern WPtr StackAreaHigh, StackAreaLow ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Globals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
extern int rtsArgC ;
extern char** rtsArgV ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry points
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef __UHC_TARGET_C__
extern int main_Sil_Init1(int argc, char** argv) ;
extern int main_Sil_Run(int argc, char** argv, int (*sillymainfunction)() );
extern int main_Sil_Exit(int argc, char** argv) ;
#endif

#ifdef __UHC_TARGET_BC__
extern int main_GB_Init1(int argc, char** argv, int* nRtsOpt) ;
extern int main_GB_Run(int argc, char** argv, GB_BytePtr initPC, GB_Word initCAF) ;
extern int main_GB_Exit(int argc, char** argv) ;
#endif
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __RTS_H__ */
%%]

