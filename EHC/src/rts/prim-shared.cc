%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives shared between bc and C backends
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "rts.h"
%%]

%%[8
PRIMS_INTLIKE_CODE(Int,Int)
%%]

%%[97
PRIMS_INTLIKE_CODE(Word,Word)
PRIMS_INTLIKE_CODE(Int64,Int64)
PRIMS_INTLIKE_CODE(Word64,Word64)
#ifdef USE_32_BITS
PRIMS_INTLIKE_CODE(Int32,Int32)
PRIMS_INTLIKE_CODE(Word32,Word32)
#endif
%%]

%%[97
PRIMS_BOUNDED_CODE(Int8,Int8)
PRIMS_BOUNDED_CODE(Int16,Int16)
PRIMS_BOUNDED_CODE(Int32,Int32)
PRIMS_BOUNDED_CODE(Int64,Int64)
// No Bounded Word!
PRIMS_BOUNDED_CODE(Word8,Word8)
PRIMS_BOUNDED_CODE(Word16,Word16)
PRIMS_BOUNDED_CODE(Word32,Word32)
PRIMS_BOUNDED_CODE(Word64,Word64)
%%]

%%[97
PRIMS_INTCONVERT_CODE(Int8,Int8)
PRIMS_INTCONVERT_CODE(Int16,Int16)
PRIMS_INTCONVERT_CODE(Int32,Int32)
PRIMS_INTCONVERT_CODE(Int64,Int64)
PRIMS_INTCONVERT_CODE(Word,Word)
PRIMS_INTCONVERT_CODE(Word8,Word8)
PRIMS_INTCONVERT_CODE(Word16,Word16)
PRIMS_INTCONVERT_CODE(Word32,Word32)
PRIMS_INTCONVERT_CODE(Word64,Word64)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Storable read/write (peek/poke)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIMS_STORABLE_CODE(Word8 ,Word8 ) 
PRIMS_STORABLE_CODE(Word16,Word16) 
PRIMS_STORABLE_CODE(Word32,Word32) 
// PRIMS_STORABLE_CODE(Word64,Word64) 
PRIMS_STORABLE_CODE(Word  ,Word  ) 
PRIMS_STORABLE_CODE(Float ,float ) 
PRIMS_STORABLE_CODE(Double,double) 
%%]

Using the above macros for primWriteWord64OffAddr make gcc crash because not enough registers are available.
Hence the handcoded variant below:

%%[99
PRIM Word64 primReadWord64OffAddr( Word64* ptr, Word off )
{					
	return ptr[ off ] ;																						
}																															
																															
PRIM Word primWriteWord64OffAddr( Word32* ptr, Word off, Word64 val )
{	
	ptr += (off << 1) ;
	Word32* p = (Word32*)(&val) ;
	*ptr++ = *p++ ;
	*ptr   = *p   ;
	return (Word)RTS_Unit ;																								
}																															
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Float, Double
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%[97

PRIMS_FLOATLIKE_CODE(Float,Float)
PRIMS_FLOATLIKE_CODE(Double,Double)


PRIM Word primIsNaNFloat( Float x )
{
	if ( isnan(x) )
		return RTS_True ;
	else
		return RTS_False ;
}

PRIM Word primIsDenormalizedFloat( Float x )
{
	if ( ! isnormal(x) )
		return RTS_True ;
	else
		return RTS_False ;
}

PRIM Word primIsInfiniteFloat( Float x )
{
	if ( isinf(x) )
		return RTS_True ;
	else
		return RTS_False ;
}

PRIM Word primIsNegativeZeroFloat( Float x )
{
	 
	if ( fpclassify(x) == FP_ZERO && signbit(x) )
		return RTS_True ;
	else
		return RTS_False ;
}

PRIM Word primDigitsFloat( )
{
	return FLT_MANT_DIG ;
}

PRIM Word primMaxExpFloat( )
{
	return FLT_MAX_EXP ;
}

PRIM Word primMinExpFloat( )
{
	return FLT_MIN_EXP ;
}

PRIM Word primIsNaNDouble( Double x )
{
	if ( isnan(x) )
		return RTS_True ;
	else
		return RTS_False ;
}

PRIM Word primIsDenormalizedDouble( Double x )
{
	if ( ! isnormal(x) )
		return RTS_True ;
	else
		return RTS_False ;
}

PRIM Word primIsInfiniteDouble( Double x )
{
	if ( isinf(x) )
		return RTS_True ;
	else
		return RTS_False ;
}

PRIM Word primIsNegativeZeroDouble( Double x )
{
	 
	if ( fpclassify(x) == FP_ZERO && signbit(x) )
		return RTS_True ;
	else
		return RTS_False ;
}

PRIM Word primDigitsDouble( )
{
	return DBL_MANT_DIG ;
}

PRIM Word primMaxExpDouble( )
{
	return DBL_MAX_EXP ;
}

PRIM Word primMinExpDouble( )
{
	return DBL_MIN_EXP ;
}

PRIM Float primIntToFloat(Int x)
{
	return Cast(Float,x) ;
}

PRIM Double primIntToDouble(Int x)
{
	return Cast(Double,x) ;
}

PRIM Double primFloatToDouble(Float x)
{
	return Cast(Double,x) ;
}

PRIM Float primDoubleToFloat(Double x)
{
	return Cast(Float,x) ;
}

PRIM Word primIsIEEE( )
{
	// We do not know for sure whether we use IEEE or not.
	// This should be dependending on some compile time C info, but not yet sorted out.
	return RTS_True ;
}

PRIM Word primRadixDoubleFloat( )
{
	return FLT_RADIX ;
}

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[11
PRIM Word primUnsafeId(Word x)
{
	return x;
}
%%]

%%[99
PRIM Word primNullAddr()
{
	return (Word)NULL ;
}
%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Char
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM Word primCharIsUpper( Word x )
{
	char c = x;
	if ( c >= 'A' && c <= 'Z' )
		return RTS_True ;
  	return RTS_False ;
}

PRIM Word primCharIsLower( Word x )
{
	char c = x;
	if ( c >= 'a' && c <= 'z' )
		return RTS_True ;
  	return RTS_False ;
}

PRIM Word primCharToUpper( Word x )
{
  	return x - 'a' + 'A' ;
}

PRIM Word primCharToLower( Word x )
{
  	return x - 'A' + 'a' ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% String
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

PRIM Word primPackedStringNull(Word w)
{
	char *s = (char *)w;
	if ( *s )
		return RTS_False ;
  	return RTS_True ;
}

PRIM Word primPackedStringTail(Word w)
{
	char *s = (char *)w;
  	return Cast(Word,s+1) ;
}


PRIM Word primPackedStringHead(Word w)
{
	char *s = (char *)w;
  	return Cast(Word,(*s)) ;
}

%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM Word primGetArgC()
{
	return (Word) rtsArgC ;
}

PRIM Word primGetArgVAt( Word argc )
{
	return (Word) rtsArgV[ argc ] ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stable ptr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Stable pointers are guaranteed not to be moved by garbage collection.
The primitives below assume the use of the Boehm garbage collector (BGC),
making their implementation simple because nothing is ever moved using BGC.

%%[99
// #if USE_BOEHM_GC
PRIM Word primMakeStableAddr( Word a )
{
	return a ;
}

PRIM Word primDeRefStableAddr( Word a )
{
	return a ;
}

PRIM Word primFreeStableAddr( Word a )
{
	return (Word)RTS_Unit ;
}

PRIM Word primEqStableAddr( Word x, Word y )
{
	if ( x == y )
		return RTS_True ;
  	return RTS_False ;
}

// #endif
%%]

