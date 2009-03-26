%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives which are not supposed to be used directly,
%%% but indirectly by being used/bound in other primitives.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Macros which expands to interface defs for primitives for a primitive type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define INTLIKE_ARITH_PRIMS_INTERFACE(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord,PrimNodePtr) 									\
																																	\
extern PrimTypeC        PrimPrefix ## primAdd   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\
extern PrimTypeC        PrimPrefix ## primSub   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\
extern PrimTypeC        PrimPrefix ## primMul   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\
extern PrimTypeC        PrimPrefix ## primDiv   ## PrimTypeName( PrimTypeC numerator, PrimTypeC divisor ) ;							\
extern PrimTypeC        PrimPrefix ## primMod   ## PrimTypeName( PrimTypeC numerator, PrimTypeC divisor ) ;							\
extern PrimTypeC        PrimPrefix ## primQuot  ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\
extern PrimTypeC        PrimPrefix ## primRem   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\
extern PrimTypeC        PrimPrefix ## primNeg   ## PrimTypeName( PrimTypeC x ) ;													\
extern PrimTypeWord     PrimPrefix ## primEq    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\
extern PrimTypeWord     PrimPrefix ## primGt    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\
extern PrimTypeWord     PrimPrefix ## primLt    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\
extern PrimTypeWord     PrimPrefix ## primCmp   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;										\

#define INTLIKE_BOUNDED_PRIMS_INTERFACE(PrimPrefix,PrimTypeName,PrimTypeC) 															\
																																	\
extern PrimTypeC        PrimPrefix ## primMax   ## PrimTypeName()	;																\
extern PrimTypeC        PrimPrefix ## primMin   ## PrimTypeName()	;																\

#define INTLIKE_INT_CONVERSION_PRIMS_INTERFACE(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord) 										\
																																	\
extern PrimTypeC        PrimPrefix ## primIntTo ## PrimTypeName( PrimTypeWord x ) ;													\
extern PrimTypeWord     PrimPrefix ## prim      ## PrimTypeName ## ToInt( PrimTypeC x ) ;											\

%%]

%%[99
#define INTLIKE_BITS_PRIMS_INTERFACE(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord,PrimNodePtr)							 		\
																																	\
extern PrimTypeC 		PrimPrefix ## primAnd 			## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;								\
extern PrimTypeC 		PrimPrefix ## primOr 			## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;								\
extern PrimTypeC 		PrimPrefix ## primXor 			## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;								\

#define INTLIKE_BITS_BITSIZE_DPD_PRIMS_INTERFACE(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord,PrimNodePtr)						\
																																	\
extern PrimTypeC 		PrimPrefix ## primShiftLeft 	## PrimTypeName( PrimTypeC x, PrimTypeWord y ) ;							\
extern PrimTypeC 		PrimPrefix ## primShiftRight 	## PrimTypeName( PrimTypeC x, PrimTypeWord y ) ;							\
extern PrimTypeC 		PrimPrefix ## primComplement 	## PrimTypeName( PrimTypeC x ) ;											\
extern PrimTypeC 		PrimPrefix ## primRotateLeft 	## PrimTypeName( PrimTypeC x, PrimTypeWord y ) ;							\
extern PrimTypeC 		PrimPrefix ## primRotateRight 	## PrimTypeName( PrimTypeC x, PrimTypeWord y ) ;							\

%%]

%%[99
#define STORABLE_PEEKPOKE_PRIMS_INTERFACE(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord) 											\
																																	\
extern PrimTypeC PrimPrefix ## primRead ## PrimTypeName ## OffAddr( PrimTypeC* ptr, PrimTypeWord off ) ;							\
extern PrimTypeWord PrimPrefix ## primWrite ## PrimTypeName ## OffAddr( PrimTypeC* ptr, PrimTypeWord off, PrimTypeC val ) ;			\

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One macro which expands to code for arithmetic primitives for a primitive type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define INTLIKE_ARITH_PRIMS_CODE(PrimPrefix,PrimTypeName,PrimTypeC,PrimFalse,PrimTrue,PrimLT,PrimEQ,PrimGT,PrimTypeWord) 			\
																																	\
PRIM PrimTypeC PrimPrefix ## primAdd ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
  	return x+y;																														\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primSub ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
  	return x-y;																														\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primMul ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
  	return x*y;																														\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primDiv ## PrimTypeName( PrimTypeC numerator, PrimTypeC divisor )										\
{																																	\
	PrimTypeC div = numerator / divisor ;																							\
																																	\
	if ( div < 0 ) {																												\
		div -= 1 ;																													\
	}																																\
																																	\
  	return (div) ;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primMod ## PrimTypeName( PrimTypeC numerator, PrimTypeC divisor )										\
{																																	\
	PrimTypeC mod = numerator % divisor ;																							\
																																	\
	if ( mod > 0 && divisor < 0 || mod < 0 && divisor > 0 ) {																		\
		mod += divisor ;																											\
	}																																\
																																	\
  	return (mod) ;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primQuot ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
  	PrimTypeC res = x / y ;																											\
  	return res ;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primRem ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
  	PrimTypeC res = x % y ;																											\
  	return res ;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primNeg ## PrimTypeName( PrimTypeC x )																	\
{																																	\
	return -(x) ;																													\
}																																	\
																																	\
PRIM PrimTypeWord PrimPrefix ## primEq ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
	if ( x == y )																													\
		return PrimTrue ;																											\
  	return PrimFalse ;																												\
}																																	\
																																	\
PRIM PrimTypeWord PrimPrefix ## primGt ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
	if ( x > y )																													\
		return PrimTrue ;																											\
  	return PrimFalse ;																												\
}																																	\
																																	\
PRIM PrimTypeWord PrimPrefix ## primLt ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
	if ( x < y )																													\
		return PrimTrue ;																											\
  	return PrimFalse ;																												\
}																																	\
																																	\
PRIM PrimTypeWord PrimPrefix ## primCmp ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																																	\
	if ( x < y )																													\
		return PrimLT ;																												\
	else if ( x == y )																												\
		return PrimEQ ;																												\
  	return PrimGT ;																													\
}																																	\
																				
%%]

%%[8
#define INTLIKE_INT_CONVERSION_PRIMS_CODE(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord) 											\
																																	\
PRIM PrimTypeC PrimPrefix ## primIntTo ## PrimTypeName( PrimTypeWord x )															\
{																																	\
	return (x) ;																													\
}																																	\
																																	\
PRIM PrimTypeWord PrimPrefix ## prim ## PrimTypeName ## ToInt( PrimTypeC x )														\
{																																	\
	return (x) ;																													\
}																																	\

%%]

%%[8
#define INTLIKE_BOUNDED_PRIMS_CODE(PrimPrefix,PrimTypeName,PrimTypeC) 																\
																																	\
PRIM PrimTypeC PrimPrefix ## primMax ## PrimTypeName()																				\
{																																	\
  	return PrimTypeC ## _MaxValue ; 																								\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primMin ## PrimTypeName()																				\
{																																	\
  	return PrimTypeC ## _MinValue ;  																								\
}																				
																				
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One macro which expands to code for Bits primitives for a primitive type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#define INTLIKE_BITS_PRIMS_CODE(PrimPrefix,PrimBitSize,PrimTypeName,PrimTypeC,PrimFalse,PrimTrue,PrimLT,PrimEQ,PrimGT,PrimTypeWord) \
																																	\
PRIM PrimTypeC PrimPrefix ## primAnd ## PrimTypeName( PrimTypeC x, PrimTypeC y ) {													\
  	return x & y;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primOr ## PrimTypeName( PrimTypeC x, PrimTypeC y ) {													\
  	return x | y;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primXor ## PrimTypeName( PrimTypeC x, PrimTypeC y ) {													\
  	return x ^ y;																													\
}																																	\
																																	\


%%]

Bitsize dependent operations where PrimTypeName needs exactly the same nr of bits of PrimTypeC.
Hence no additional masking is needed.

%%[99
#define INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(PrimPrefix,PrimBitSize,PrimTypeName,PrimTypeC,PrimTypeWord) 							\
																																	\
PRIM PrimTypeC PrimPrefix ## primShiftLeft ## PrimTypeName( PrimTypeC x, PrimTypeWord y ) {											\
	return x << y ;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primShiftRight ## PrimTypeName( PrimTypeC x, PrimTypeWord y ) {										\
	return x >> y ;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primComplement ## PrimTypeName( PrimTypeC x ) {														\
  	return ~x ;																														\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primRotateLeft ## PrimTypeName( PrimTypeC x, PrimTypeWord y ) {										\
	return (x << y) | (x >> (PrimBitSize - y)) ;																					\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primRotateRight ## PrimTypeName( PrimTypeC x, PrimTypeWord y ) {										\
	return (x >> y) | (x << (PrimBitSize - y)) ;																					\
}																																	\
																																	\

%%]

Bitsize dependent operations where PrimTypeName needs strictly less bits of PrimTypeC.
For masking take the largest used int variant, Word64.

%%[99
#define INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(PrimPrefix,PrimBitSize,PrimTypeName,PrimTypeC,PrimTypeWord) 							\
																																	\
PRIM PrimTypeC PrimPrefix ## primShiftLeft ## PrimTypeName( PrimTypeC x, PrimTypeWord y ) {											\
	return (x << y) & Bits_Size2LoMask(Word64,PrimBitSize) ;																		\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primShiftRight ## PrimTypeName( PrimTypeC x, PrimTypeWord y ) {										\
	return x >> y ;																													\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primComplement ## PrimTypeName( PrimTypeC x ) {														\
  	return (~x) & Bits_Size2LoMask(Word64,PrimBitSize) ;																														\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primRotateLeft ## PrimTypeName( PrimTypeC x, PrimTypeWord y ) {										\
	PrimTypeC xx = x & Bits_Size2LoMask(Word64,PrimBitSize) ;																		\
	return ((xx << y) | (xx >> (PrimBitSize - y))) & Bits_Size2LoMask(Word64,PrimBitSize) ;											\
}																																	\
																																	\
PRIM PrimTypeC PrimPrefix ## primRotateRight ## PrimTypeName( PrimTypeC x, PrimTypeWord y ) {										\
	PrimTypeC xx = x & Bits_Size2LoMask(Word64,PrimBitSize) ;																		\
	return (xx >> y) | (xx << (PrimBitSize - y)) ;																					\
}																																	\
																																	\

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One macro which expands to code for Storable peek/poke primitives for a primitive type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#define STORABLE_PEEKPOKE_PRIMS_CODE(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord) 												\
																																	\
PRIM PrimTypeC PrimPrefix ## primRead ## PrimTypeName ## OffAddr( PrimTypeC* ptr, PrimTypeWord off ) {								\
	return ptr[ off ] ;																												\
}																																	\
																																	\
PRIM PrimTypeWord PrimPrefix ## primWrite ## PrimTypeName ## OffAddr( PrimTypeC* ptr, PrimTypeWord off, PrimTypeC val ) {			\
	ptr[ off ] = val ;																												\
	return (GB_Word)gb_Unit ;																										\
}																																	\

%%]

