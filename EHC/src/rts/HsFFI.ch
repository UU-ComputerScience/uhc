%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FFI: C level interface to types used by FFI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifndef __HSFFI_H__
#define __HSFFI_H__
%%]

%%[99
#ifdef __cplusplus
extern "C" {
#endif
%%]

%%[8
#include <inttypes.h>
#include <limits.h>
%%]

%%[97
#include <math.h>
#include <float.h>
%%]

%%[8
#include "config.h"
#include "base/sizes.h"
#include "base/basictypes.h"
#include "base/bits.h"
#ifdef __UHC_TARGET_BC__
#include "bc/base.h"
#include "bc/prim-bool.h"
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
typedef Word			HsChar;
typedef Int				HsInt;
typedef Int8			HsInt8;
typedef Int16			HsInt16;
typedef Int32			HsInt32;
typedef Int64			HsInt64;
typedef Word			HsWord;
typedef Word8			HsWord8;
typedef Word16			HsWord16;
typedef Word32			HsWord32;
typedef Word64			HsWord64;
typedef Float			HsFloat;
typedef Double			HsDouble;
typedef Word			HsBool;
%%]

TBD: These are not yet sorted out properly

%%[99
typedef void*			HsPtr;          /* this should better match StgAddr */
typedef void			(*HsFunPtr)(void); /* this should better match StgAddr */
typedef void*			HsForeignPtr;   /* ... and this StgForeignPtr       */
typedef void*			HsStablePtr;
typedef void*			HsAddr;         /* DEPRECATED */
typedef void*			HsForeignObj;   /* DEPRECATED */
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Min & max values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
/* this should correspond to the type of StgChar in StgTypes.h */
#define HS_CHAR_MIN				0
#define HS_CHAR_MAX				Int8_MaxValue

#ifdef __UHC_TARGET_BC__
# define HS_BOOL_FALSE			gb_False
# define HS_BOOL_TRUE			gb_True
#else
# define HS_BOOL_FALSE			0
# define HS_BOOL_TRUE			1
#endif

#define HS_BOOL_MIN             HS_BOOL_FALSE
#define HS_BOOL_MAX             HS_BOOL_TRUE

#define HS_INT_MIN				Int_MinValue
#define HS_INT_MAX				Int_MaxValue
#define HS_WORD_MIN				Word_MinValue
#define HS_WORD_MAX				Word_MaxValue

#define HS_INT8_MIN				Int8_MinValue
#define HS_INT8_MAX				Int8_MaxValue
#define HS_INT16_MIN			Int16_MinValue
#define HS_INT16_MAX			Int16_MaxValue
#define HS_INT32_MIN			Int32_MinValue
#define HS_INT32_MAX			Int32_MaxValue
#define HS_INT64_MIN			Int64_MinValue
#define HS_INT64_MAX			Int64_MaxValue
#define HS_WORD8_MAX			Word8_MaxValue
#define HS_WORD16_MAX			Word16_MaxValue
#define HS_WORD32_MAX			Word32_MaxValue
#define HS_WORD64_MAX			Word64_MaxValue

#define HS_FLOAT_RADIX			FLT_RADIX
#define HS_FLOAT_ROUNDS			FLT_ROUNDS
#define HS_FLOAT_EPSILON		FLT_EPSILON
#define HS_FLOAT_DIG			FLT_DIG
#define HS_FLOAT_MANT_DIG		FLT_MANT_DIG
#define HS_FLOAT_MIN			FLT_MIN
#define HS_FLOAT_MIN_EXP		FLT_MIN_EXP
#define HS_FLOAT_MIN_10_EXP		FLT_MIN_10_EXP
#define HS_FLOAT_MAX			FLT_MAX
#define HS_FLOAT_MAX_EXP		FLT_MAX_EXP
#define HS_FLOAT_MAX_10_EXP		FLT_MAX_10_EXP

#define HS_DOUBLE_RADIX			DBL_RADIX
#define HS_DOUBLE_ROUNDS		DBL_ROUNDS
#define HS_DOUBLE_EPSILON		DBL_EPSILON
#define HS_DOUBLE_DIG			DBL_DIG
#define HS_DOUBLE_MANT_DIG		DBL_MANT_DIG
#define HS_DOUBLE_MIN			DBL_MIN
#define HS_DOUBLE_MIN_EXP		DBL_MIN_EXP
#define HS_DOUBLE_MIN_10_EXP	DBL_MIN_10_EXP
#define HS_DOUBLE_MAX			DBL_MAX
#define HS_DOUBLE_MAX_EXP		DBL_MAX_EXP
#define HS_DOUBLE_MAX_10_EXP	DBL_MAX_10_EXP
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interfaces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Not yet supported

%%[99
%%]
extern void hs_init     (int *argc, char **argv[]);
extern void hs_exit     (void);
extern void hs_set_argv (int argc, char *argv[]);
extern void hs_add_root (void (*init_root)(void));

extern void hs_perform_gc (void);

extern void hs_free_stable_ptr (HsStablePtr sp);
extern void hs_free_fun_ptr    (HsFunPtr fp);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#ifdef __cplusplus
}
#endif
%%]

%%[8
#endif /* __HSFFI_H__ */
%%]
