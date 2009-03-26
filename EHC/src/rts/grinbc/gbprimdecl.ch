%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Definitions for interfaces of primitives which cannot be derived automatically from gbprim.h
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
INTLIKE_ARITH_PRIMS_INTERFACE(gb_,Int,Int,GB_Word,GB_NodePtr)
%%]

%%[99
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Int,Int,GB_Word,GB_NodePtr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
INTLIKE_ARITH_PRIMS_INTERFACE(gb_,Word,Word,GB_Word,GB_NodePtr)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Word,Word,GB_Word)

%%]

%%[99
INTLIKE_BITS_PRIMS_INTERFACE(gb_,Word,Word,GB_Word,GB_NodePtr)
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Word,Word,GB_Word,GB_NodePtr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word8, Word16, Word32, Word64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
INTLIKE_BOUNDED_PRIMS_INTERFACE(gb_,Word8,Word8)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Word8,Word8,GB_Word)

%%]

%%[99
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Word8,Word8,GB_Word,GB_NodePtr)
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_INTERFACE(gb_,Word16,Word16)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Word16,Word16,GB_Word)

%%]

%%[99
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Word16,Word16,GB_Word,GB_NodePtr)
%%]

%%[99
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Word16,Word16,GB_Word,GB_NodePtr)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[97
INTLIKE_BOUNDED_PRIMS_INTERFACE(gb_,Word32,Word32)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Word32,Word32,GB_Word)
#ifdef USE_32_BITS
INTLIKE_ARITH_PRIMS_INTERFACE(gb_,Word32,Word32,GB_Word,GB_NodePtr)
#endif

%%]

%%[99
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Word32,Word32,GB_Word,GB_NodePtr)
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_INTERFACE(gb_,Word64,Word64)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Word64,Word64,GB_Word)
INTLIKE_ARITH_PRIMS_INTERFACE(gb_,Word64,Word64,GB_Word,GB_NodePtr)

%%]

%%[99
INTLIKE_BITS_PRIMS_INTERFACE(gb_,Word64,Word64,GB_Word,GB_NodePtr)
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Word64,Word64,GB_Word,GB_NodePtr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int8, Int16, Int32, Int64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
INTLIKE_BOUNDED_PRIMS_INTERFACE(gb_,Int8,Int8)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Int8,Int8,GB_Word)

%%]

%%[99
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Int8,Int8,GB_Word,GB_NodePtr)
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_INTERFACE(gb_,Int16,Int16)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Int16,Int16,GB_Word)

%%]

%%[99
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Int16,Int16,GB_Word,GB_NodePtr)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[97
INTLIKE_BOUNDED_PRIMS_INTERFACE(gb_,Int32,Int32)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Int32,Int32,GB_Word)
#ifdef USE_32_BITS
INTLIKE_ARITH_PRIMS_INTERFACE(gb_,Int32,Int32,GB_Word,GB_NodePtr)
#endif

%%]

%%[99
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Int32,Int32,GB_Word,GB_NodePtr)
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_INTERFACE(gb_,Int64,Int64)
INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(gb_,Int64,Int64,GB_Word)
INTLIKE_ARITH_PRIMS_INTERFACE(gb_,Int64,Int64,GB_Word,GB_NodePtr)

%%]

%%[99
INTLIKE_BITS_PRIMS_INTERFACE(gb_,Int64,Int64,GB_Word,GB_NodePtr)
INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(gb_,Int64,Int64,GB_Word,GB_NodePtr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Storable read/write (peek/poke)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
STORABLE_PEEKPOKE_PRIMS_INTERFACE(gb_,Word8 ,Word8 ,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_INTERFACE(gb_,Word16,Word16,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_INTERFACE(gb_,Word32,Word32,GB_Word) 
// STORABLE_PEEKPOKE_PRIMS_INTERFACE(gb_,Word64,Word64,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_INTERFACE(gb_,Word  ,Word  ,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_INTERFACE(gb_,Float ,float ,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_INTERFACE(gb_,Double,double,GB_Word) 
%%]

