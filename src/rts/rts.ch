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
#define LOCALSSIZE 1000

extern WPtr SP, RP;
extern WPtr Stack, ReturnArea, LocalsArea;
extern WPtr StackAreaHigh, StackAreaLow ;

extern Word 
  Ret0,  Ret1,  Ret2,  Ret3,  Ret4,  Ret5,  Ret6,  Ret7,  Ret8,  Ret9
, Ret10, Ret11, Ret12, Ret13, Ret14, Ret15, Ret16, Ret17, Ret18, Ret19
, Ret20, Ret21, Ret22, Ret23, Ret24, Ret25, Ret26, Ret27, Ret28, Ret29
, Ret30, Ret31, Ret32, Ret33, Ret34, Ret35, Ret36, Ret37, Ret38, Ret39
, Ret40, Ret41, Ret42, Ret43, Ret44, Ret45, Ret46, Ret47, Ret48, Ret49
, Ret50, Ret51, Ret52, Ret53, Ret54, Ret55, Ret56, Ret57, Ret58, Ret59
, Ret60, Ret61, Ret62, Ret63, Ret64, Ret65, Ret66, Ret67, Ret68, Ret69
, Ret70, Ret71, Ret72, Ret73, Ret74, Ret75, Ret76, Ret77, Ret78, Ret79
, Ret80, Ret81, Ret82, Ret83, Ret84, Ret85, Ret86, Ret87, Ret88, Ret89
, Ret90, Ret91, Ret92, Ret93, Ret94, Ret95, Ret96, Ret97, Ret98, Ret99;

extern Word 
  Loc0,  Loc1,  Loc2,  Loc3,  Loc4,  Loc5,  Loc6,  Loc7,  Loc8,  Loc9
, Loc10, Loc11, Loc12, Loc13, Loc14, Loc15, Loc16, Loc17, Loc18, Loc19
, Loc20, Loc21, Loc22, Loc23, Loc24, Loc25, Loc26, Loc27, Loc28, Loc29
, Loc30, Loc31, Loc32, Loc33, Loc34, Loc35, Loc36, Loc37, Loc38, Loc39
, Loc40, Loc41, Loc42, Loc43, Loc44, Loc45, Loc46, Loc47, Loc48, Loc49
, Loc50, Loc51, Loc52, Loc53, Loc54, Loc55, Loc56, Loc57, Loc58, Loc59
, Loc60, Loc61, Loc62, Loc63, Loc64, Loc65, Loc66, Loc67, Loc68, Loc69
, Loc70, Loc71, Loc72, Loc73, Loc74, Loc75, Loc76, Loc77, Loc78, Loc79
, Loc80, Loc81, Loc82, Loc83, Loc84, Loc85, Loc86, Loc87, Loc88, Loc89
, Loc90, Loc91, Loc92, Loc93, Loc94, Loc95, Loc96, Loc97, Loc98, Loc99;



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

