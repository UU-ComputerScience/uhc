%%[8
#ifndef __RTSBASE_H__
#define __RTSBASE_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Base info & config of rts, to be used as the basis of other libraries used by the rts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes for config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "HsFFI.h"
%%]
#include "config.h"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal config, must proceed includes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
#ifdef __UHC_TARGET_BC__
// For now, switch off Boehm GC, turn on own GC
#undef USE_BOEHM_GC
#define USE_EHC_MM				1

// internal MM admin uses structs with functions, which will be bypassed (for speed) with MM_BYPASS_PLAN on
#define MM_BYPASS_PLAN			1

// use LTM (LibTomMath), instead of GMP
// #define USE_LTM					1

// debugging
%%[[8
#define GB_DEBUG				0	// 0 or 1 for debugging settings
%%][99
#define GB_DEBUG				0	// 0 or 1 for debugging settings
%%][100
#define GB_DEBUG				0	// always 0
%%]]
#endif
%%]

%%[8
%%]
// not used
#define GB_IND_IN_HDR			1


%%[8
#define INFO_EXITSTATE			1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// #include <inttypes.h>
// #include <limits.h>
// #include "base/sizes.h"
// #include "base/basictypes.h"
// #include "base/bits.h"
#if __UHC_TARGET_BC__
#include "bc/registers.h"
#endif
#include "base/sysalloc.h"
#include "base/panic.h"
#include "mm/mmitf.h"
#include "base/utils.h"

%%[[97
#ifndef FP_ZERO
#warning FP_ZERO not defined (assuming value 2). Using floating point numbers may give problems.
#define FP_ZERO 2
#endif
%%]]

#ifdef __UHC_TARGET_BC__
#include "bc/types.h"
#endif
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __RTSBASE_H__ */
%%]
