%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Definitions for interfaces of primitives which cannot be derived automatically from gbprim.h
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
IntLikeArithPrimsInterface(gb_,Int,Int,GB_Word,GB_NodePtr)
%%]

%%[99
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Int,Int,GB_Word,GB_NodePtr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
IntLikeArithPrimsInterface(gb_,Word,Word,GB_Word,GB_NodePtr)
IntLikeIntConversionPrimsInterface(gb_,Word,Word,GB_Word)

%%]

%%[99
IntLikeBitsPrimsInterface(gb_,Word,Word,GB_Word,GB_NodePtr)
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Word,Word,GB_Word,GB_NodePtr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word8, Word16, Word32, Word64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
IntLikeBoundedPrimsInterface(gb_,Word8,Word8)
IntLikeIntConversionPrimsInterface(gb_,Word8,Word8,GB_Word)

%%]

%%[99
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Word8,Word8,GB_Word,GB_NodePtr)
%%]

%%[97
IntLikeBoundedPrimsInterface(gb_,Word16,Word16)
IntLikeIntConversionPrimsInterface(gb_,Word16,Word16,GB_Word)

%%]

%%[99
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Word16,Word16,GB_Word,GB_NodePtr)
%%]

%%[99
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Word16,Word16,GB_Word,GB_NodePtr)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[97
IntLikeBoundedPrimsInterface(gb_,Word32,Word32)
IntLikeIntConversionPrimsInterface(gb_,Word32,Word32,GB_Word)
#ifdef USE_32_BITS
IntLikeArithPrimsInterface(gb_,Word32,Word32,GB_Word,GB_NodePtr)
#endif

%%]

%%[99
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Word32,Word32,GB_Word,GB_NodePtr)
%%]

%%[97
IntLikeBoundedPrimsInterface(gb_,Word64,Word64)
IntLikeIntConversionPrimsInterface(gb_,Word64,Word64,GB_Word)
IntLikeArithPrimsInterface(gb_,Word64,Word64,GB_Word,GB_NodePtr)

%%]

%%[99
IntLikeBitsPrimsInterface(gb_,Word64,Word64,GB_Word,GB_NodePtr)
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Word64,Word64,GB_Word,GB_NodePtr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int8, Int16, Int32, Int64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
IntLikeBoundedPrimsInterface(gb_,Int8,Int8)
IntLikeIntConversionPrimsInterface(gb_,Int8,Int8,GB_Word)

%%]

%%[99
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Int8,Int8,GB_Word,GB_NodePtr)
%%]

%%[97
IntLikeBoundedPrimsInterface(gb_,Int16,Int16)
IntLikeIntConversionPrimsInterface(gb_,Int16,Int16,GB_Word)

%%]

%%[99
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Int16,Int16,GB_Word,GB_NodePtr)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[97
IntLikeBoundedPrimsInterface(gb_,Int32,Int32)
IntLikeIntConversionPrimsInterface(gb_,Int32,Int32,GB_Word)
#ifdef USE_32_BITS
IntLikeArithPrimsInterface(gb_,Int32,Int32,GB_Word,GB_NodePtr)
#endif

%%]

%%[99
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Int32,Int32,GB_Word,GB_NodePtr)
%%]

%%[97
IntLikeBoundedPrimsInterface(gb_,Int64,Int64)
IntLikeIntConversionPrimsInterface(gb_,Int64,Int64,GB_Word)
IntLikeArithPrimsInterface(gb_,Int64,Int64,GB_Word,GB_NodePtr)

%%]

%%[99
IntLikeBitsPrimsInterface(gb_,Int64,Int64,GB_Word,GB_NodePtr)
IntLikeBitsBitsizeDpdPrimsInterface(gb_,Int64,Int64,GB_Word,GB_NodePtr)
%%]

