%%[8
#ifndef __PRIMDECL_H__
#define __PRIMDECL_H__
%%]

%%% Declarations for macro-generated primitive functions


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIMS_INTLIKE_INTERFACE(Int,Int)
%%]

%%[97
PRIMS_INTLIKE_INTERFACE(Word,Word)
PRIMS_INTLIKE_INTERFACE(Word64,Word64)
PRIMS_INTLIKE_INTERFACE(Int64,Int64)
#ifdef USE_32_BITS
PRIMS_INTLIKE_INTERFACE(Word32,Word32)
PRIMS_INTLIKE_INTERFACE(Int32,Int32)
#endif
%%]

%%[97
PRIMS_INTCONVERT_INTERFACE(Word,Word)
PRIMS_INTCONVERT_INTERFACE(Word8,Word8)
PRIMS_INTCONVERT_INTERFACE(Word16,Word16)
PRIMS_INTCONVERT_INTERFACE(Word32,Word32)
PRIMS_INTCONVERT_INTERFACE(Word64,Word64)
PRIMS_INTCONVERT_INTERFACE(Int8,Int8)
PRIMS_INTCONVERT_INTERFACE(Int16,Int16)
PRIMS_INTCONVERT_INTERFACE(Int32,Int32)
PRIMS_INTCONVERT_INTERFACE(Int64,Int64)
%%]

%%[97
PRIMS_BOUNDED_INTERFACE(Word8,Word8)
PRIMS_BOUNDED_INTERFACE(Word16,Word16)
PRIMS_BOUNDED_INTERFACE(Word32,Word32)
PRIMS_BOUNDED_INTERFACE(Word64,Word64)
PRIMS_BOUNDED_INTERFACE(Int8,Int8)
PRIMS_BOUNDED_INTERFACE(Int16,Int16)
PRIMS_BOUNDED_INTERFACE(Int32,Int32)
PRIMS_BOUNDED_INTERFACE(Int64,Int64)
%%]

%%[97
PRIMS_FLOATLIKE_INTERFACE(Float,Float)
PRIMS_FLOATLIKE_INTERFACE(Double,Double)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Storable read/write (peek/poke)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIMS_STORABLE_INTERFACE(Word8 ,Word8 ) 
PRIMS_STORABLE_INTERFACE(Word16,Word16) 
PRIMS_STORABLE_INTERFACE(Word32,Word32) 
// PRIMS_STORABLE_INTERFACE(Word64,Word64) 
PRIMS_STORABLE_INTERFACE(Word  ,Word  ) 
PRIMS_STORABLE_INTERFACE(Float ,float ) 
PRIMS_STORABLE_INTERFACE(Double,double) 
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __PRIMDECL_H__ */
%%]
