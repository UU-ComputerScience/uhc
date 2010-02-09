%%[8
#ifndef __BC_PRIMDECL_H__
#define __BC_PRIMDECL_H__
%%]

%%% Declarations for macro-generated primitive functions

%%[99
PRIMS_BITLOGIC_INTERFACE(Word,Word)
PRIMS_BITLOGIC_INTERFACE(Word64,Word64)
PRIMS_BITLOGIC_INTERFACE(Int64,Int64)

PRIMS_BITSHIFT_INTERFACE(Int,Int,SWord)
PRIMS_BITSHIFT_INTERFACE(Int8,Int8,SWord)
PRIMS_BITSHIFT_INTERFACE(Int16,Int16,SWord)
PRIMS_BITSHIFT_INTERFACE(Int32,Int32,SWord)
PRIMS_BITSHIFT_INTERFACE(Int64,Int64,SWord64)
PRIMS_BITSHIFT_INTERFACE(Word,Word,Word)
PRIMS_BITSHIFT_INTERFACE(Word8,Word8,Word)
PRIMS_BITSHIFT_INTERFACE(Word16,Word16,Word)
PRIMS_BITSHIFT_INTERFACE(Word32,Word32,Word)
PRIMS_BITSHIFT_INTERFACE(Word64,Word64,Word64)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
extern void prim_integer_Initialize() ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BC_PRIMDECL_H__ */
%%]
