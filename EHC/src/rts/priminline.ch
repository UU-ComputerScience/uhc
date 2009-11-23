%%[8
#ifndef __PRIMINLINE_H__
#define __PRIMINLINE_H__
%%]

Some C functions are overloaded for various numeric C types:
- arithmetic
- minimum and maximum values of type
- conversion to Int
- bitwise logic
- bit-shifts
- take an element of an array

We define macros to define groups of primitive functions for corresponding numeric Haskell types.
There are macros for generating the function headers and for generating the function bodies.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Macros which expand to interface definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

#define PRIMS_EQ_INTERFACE(PrimTypeName,PrimTypeC) 									\
extern Word       primEq    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern Word       primNe    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\

#define PRIMS_ORD_INTERFACE(PrimTypeName,PrimTypeC) 									\
extern Word       primGt    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern Word       primLt    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern Word       primGe    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern Word       primLe    ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern Word       primCmp   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\

#define PRIMS_NUM_INTERFACE(PrimTypeName,PrimTypeC) 									\
extern PrimTypeC  primAdd   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern PrimTypeC  primSub   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern PrimTypeC  primMul   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern PrimTypeC  primNeg   ## PrimTypeName( PrimTypeC x ) ;							\

#define PRIMS_INTEGRAL_INTERFACE(PrimTypeName,PrimTypeC) 									\
extern PrimTypeC  primDiv   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern PrimTypeC  primMod   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern PrimTypeC  primQuot  ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern PrimTypeC  primRem   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\

#define PRIMS_FRACTIONAL_INTERFACE(PrimTypeName,PrimTypeC) 									\
extern PrimTypeC  primDivide   ## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;				\
extern PrimTypeC  primRecip    ## PrimTypeName( PrimTypeC x ) ;				\

#define PRIMS_EQ_ORD_NUM_INTERFACE(PrimTypeName,PrimTypeC) 									\
PRIMS_EQ_INTERFACE(PrimTypeName,PrimTypeC) \
PRIMS_ORD_INTERFACE(PrimTypeName,PrimTypeC) \
PRIMS_NUM_INTERFACE(PrimTypeName,PrimTypeC) \

#define PRIMS_INTLIKE_INTERFACE(PrimTypeName,PrimTypeC) 									\
PRIMS_EQ_ORD_NUM_INTERFACE(PrimTypeName,PrimTypeC) \
PRIMS_INTEGRAL_INTERFACE(PrimTypeName,PrimTypeC) \

#define PRIMS_FLOATLIKE_INTERFACE(PrimTypeName,PrimTypeC) 									\
PRIMS_EQ_ORD_NUM_INTERFACE(PrimTypeName,PrimTypeC) \
PRIMS_FRACTIONAL_INTERFACE(PrimTypeName,PrimTypeC) \

#define PRIMS_BOUNDED_INTERFACE(PrimTypeName,PrimTypeC) 								\
extern PrimTypeC         primMax   ## PrimTypeName()	;								\
extern PrimTypeC         primMin   ## PrimTypeName()	;								\


#define PRIMS_INTCONVERT_INTERFACE(PrimTypeName,PrimTypeC) 								\
extern PrimTypeC         primIntTo ## PrimTypeName( Word x ) ;							\
extern Word              prim      ## PrimTypeName ## ToInt( PrimTypeC x ) ;			\

%%]

%%[99
#define PRIMS_BITLOGIC_INTERFACE(PrimTypeName,PrimTypeC)							 		\
extern PrimTypeC 		 primAnd 			## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;	\
extern PrimTypeC 		 primOr 			## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;	\
extern PrimTypeC 		 primXor 			## PrimTypeName( PrimTypeC x, PrimTypeC y ) ;	\

#define PRIMS_BITSHIFT_INTERFACE(PrimTypeName,PrimTypeC,PrimTypeCWord)						\
extern PrimTypeCWord 		 primShiftLeft 	## PrimTypeName( PrimTypeC x, Word y ) ;		\
extern PrimTypeCWord 		 primShiftRight 	## PrimTypeName( PrimTypeC x, Word y ) ;	\
extern PrimTypeCWord 		 primComplement 	## PrimTypeName( PrimTypeC x ) ;			\
extern PrimTypeCWord 		 primRotateLeft 	## PrimTypeName( PrimTypeC x, Word y ) ;	\
extern PrimTypeCWord 		 primRotateRight 	## PrimTypeName( PrimTypeC x, Word y ) ;	\

%%]

%%[99

#ifdef __UHC_TARGET_C__

#define PRIMS_STORABLE_INTERFACE(PrimTypeName,PrimTypeC) 											\
extern PrimTypeC  primRead ## PrimTypeName ## OffAddr( Word ptr, Word off ) ;					\
extern Word       primWrite ## PrimTypeName ## OffAddr( Word ptr, Word off, Word val ) ;	\

#else

#define PRIMS_STORABLE_INTERFACE(PrimTypeName,PrimTypeC) 											\
extern PrimTypeC  primRead ## PrimTypeName ## OffAddr( PrimTypeC* ptr, Word off ) ;					\
extern Word       primWrite ## PrimTypeName ## OffAddr( PrimTypeC* ptr, Word off, PrimTypeC val ) ;	\

#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Macros which expand to code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8


#define PRIMS_EQ_CODE(PrimTypeName,PrimTypeC) 		\
PRIM Word  primEq ## PrimTypeName( PrimTypeC x, PrimTypeC y )														\
{																													\
	if ( x == y )																									\
		return RTS_True ;																							\
  	return RTS_False ;																								\
}																													\
PRIM Word  primNe ## PrimTypeName( PrimTypeC x, PrimTypeC y )														\
{																													\
	if ( x != y )																									\
		return RTS_True ;																							\
  	return RTS_False ;																								\
}																													\


#define PRIMS_ORD_CODE(PrimTypeName,PrimTypeC) 		\
PRIM Word  primGt ## PrimTypeName( PrimTypeC x, PrimTypeC y )														\
{																													\
	if ( x > y )																									\
		return RTS_True ;																							\
  	return RTS_False ;																								\
}																													\
PRIM Word  primLt ## PrimTypeName( PrimTypeC x, PrimTypeC y )														\
{																													\
	if ( x < y )																									\
		return RTS_True ;																							\
  	return RTS_False ;																								\
}																													\
PRIM Word  primGe ## PrimTypeName( PrimTypeC x, PrimTypeC y )														\
{																													\
	if ( x >= y )																									\
		return RTS_True ;																							\
  	return RTS_False ;																								\
}																													\
PRIM Word  primLe ## PrimTypeName( PrimTypeC x, PrimTypeC y )														\
{																													\
	if ( x <= y )																									\
		return RTS_True ;																							\
  	return RTS_False ;																								\
}																													\
PRIM Word  primCmp ## PrimTypeName( PrimTypeC x, PrimTypeC y )														\
{																													\
	if ( x < y )																									\
		return RTS_LT ;																								\
	else if ( x == y )																								\
		return RTS_EQ ;																								\
  	return RTS_GT ;																									\
}																													\
																													\

#define PRIMS_NUM_CODE(PrimTypeName,PrimTypeC) 		\
PRIM PrimTypeC  primAdd ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																													\
  	return x+y;																										\
}																													\
																													\
PRIM PrimTypeC  primSub ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																													\
  	return x-y;																										\
}																													\
																													\
PRIM PrimTypeC  primMul ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																													\
  	return x*y;																										\
}																													\
PRIM PrimTypeC  primNeg ## PrimTypeName( PrimTypeC x )																\
{																													\
	return -(x) ;																									\
}																													\


#define PRIMS_INTEGRAL_CODE(PrimTypeName,PrimTypeC) 		\
PRIM PrimTypeC  primDiv ## PrimTypeName( PrimTypeC numerator, PrimTypeC divisor )									\
{																													\
	PrimTypeC div = numerator / divisor ;																			\
	if ( div < 0 ) {																								\
		div -= 1 ;																									\
	}																												\
  	return (div) ;																									\
}																													\
PRIM PrimTypeC  primMod ## PrimTypeName( PrimTypeC numerator, PrimTypeC divisor )									\
{																													\
	PrimTypeC mod = numerator % divisor ;																			\
	if ( mod > 0 && divisor < 0 || mod < 0 && divisor > 0 ) {														\
		mod += divisor ;																							\
	}																												\
  	return (mod) ;																									\
}																													\
PRIM PrimTypeC  primQuot ## PrimTypeName( PrimTypeC x, PrimTypeC y )												\
{																													\
  	PrimTypeC res = x / y ;																							\
  	return res ;																									\
}																													\
PRIM PrimTypeC  primRem ## PrimTypeName( PrimTypeC x, PrimTypeC y )													\
{																													\
  	PrimTypeC res = x % y ;																							\
  	return res ;																									\
}																													\


#define PRIMS_FRACTIONAL_CODE(PrimTypeName,PrimTypeC) 		\
PRIM PrimTypeC  primDivide ## PrimTypeName( PrimTypeC numerator, PrimTypeC divisor )								\
{																													\
	return numerator / divisor ;																			\
}																													\
PRIM PrimTypeC  primRecip ## PrimTypeName( PrimTypeC divisor )									\
{																													\
	return 1 / divisor ;																			\
}																													\



#define PRIMS_EQ_ORD_NUM_CODE(PrimTypeName,PrimTypeC) 									\
PRIMS_EQ_CODE(PrimTypeName,PrimTypeC) \
PRIMS_ORD_CODE(PrimTypeName,PrimTypeC) \
PRIMS_NUM_CODE(PrimTypeName,PrimTypeC) \

#define PRIMS_INTLIKE_CODE(PrimTypeName,PrimTypeC) 									\
PRIMS_EQ_ORD_NUM_CODE(PrimTypeName,PrimTypeC) \
PRIMS_INTEGRAL_CODE(PrimTypeName,PrimTypeC) \

#define PRIMS_FLOATLIKE_CODE(PrimTypeName,PrimTypeC) 									\
PRIMS_EQ_ORD_NUM_CODE(PrimTypeName,PrimTypeC) \
PRIMS_FRACTIONAL_CODE(PrimTypeName,PrimTypeC) \

%%]




%%[8
#define PRIMS_INTCONVERT_CODE(PrimTypeName,PrimTypeC) 																\
PRIM PrimTypeC  primIntTo ## PrimTypeName( Word x )																	\
{																													\
	return (x) ;																									\
}																													\
PRIM Word  prim ## PrimTypeName ## ToInt( PrimTypeC x )																\
{																													\
	return (x) ;																									\
}																													\

%%]




%%[8
#define PRIMS_BOUNDED_CODE(PrimTypeName,PrimTypeC) 																	\
PRIM PrimTypeC  primMax ## PrimTypeName()																			\
{																													\
  	return PrimTypeC ## _MaxValue ; 																				\
}																													\
PRIM PrimTypeC  primMin ## PrimTypeName()																			\
{																													\
  	return PrimTypeC ## _MinValue ;  																				\
}																				
																				
%%]





%%[99
#define PRIMS_BITLOGIC_CODE(PrimBitSize,PrimTypeName,PrimTypeC) \
																														\
PRIM PrimTypeC  primAnd ## PrimTypeName( PrimTypeC x, PrimTypeC y ) {													\
  	return x & y;																										\
}																														\
																														\
PRIM PrimTypeC  primOr ## PrimTypeName( PrimTypeC x, PrimTypeC y ) {													\
  	return x | y;																										\
}																														\
																														\
PRIM PrimTypeC  primXor ## PrimTypeName( PrimTypeC x, PrimTypeC y ) {													\
  	return x ^ y;																										\
}																														\

%%]




Bitsize dependent operations where PrimTypeName needs exactly the same nr of bits of PrimTypeC.
Hence no additional masking is needed.

%%[99
#define PRIMS_BITSHIFT_CODE(PrimBitSize,PrimTypeName,PrimTypeC,PrimTypeCWord) 			\
																														\
PRIM PrimTypeCWord  primShiftLeft ## PrimTypeName( PrimTypeC x, Word y ) {												\
	return x << y ;																										\
}																														\
																														\
PRIM PrimTypeCWord  primShiftRight ## PrimTypeName( PrimTypeC x, Word y ) {												\
	return x >> y ;																										\
}																														\
																														\
PRIM PrimTypeCWord  primComplement ## PrimTypeName( PrimTypeC x ) {														\
  	return ~x ;																											\
}																														\
																														\
PRIM PrimTypeCWord  primRotateLeft ## PrimTypeName( PrimTypeC x, Word y ) {												\
	return (x << y) | (x >> (PrimBitSize - y)) ;																		\
}																														\
																														\
PRIM PrimTypeCWord  primRotateRight ## PrimTypeName( PrimTypeC x, Word y ) {											\
	return (x >> y) | (x << (PrimBitSize - y)) ;																		\
}																														\
																														\

%%]




Bitsize dependent operations where PrimTypeName needs strictly less bits of PrimTypeC.
For masking take the largest used int variant, Word64.

%%[99
#define PRIMS_BITSHIFT_MASKCODE(PrimBitSize,PrimTypeName,PrimTypeC,PrimTypeCWord) 										\
																														\
PRIM PrimTypeCWord  primShiftLeft ## PrimTypeName( PrimTypeC x, Word y ) {												\
	PrimTypeC xx = x & Bits_Size2LoMask(Word64,PrimBitSize) ;															\
	return (PrimTypeC) (xx << y) ;																						\
}																														\
																														\
PRIM PrimTypeCWord  primShiftRight ## PrimTypeName( PrimTypeC x, Word y ) {												\
	return x >> y ;																										\
}																														\
																														\
PRIM PrimTypeCWord  primComplement ## PrimTypeName( PrimTypeC x ) {														\
  	return (PrimTypeC) ((~x) & Bits_Size2LoMask(Word64,PrimBitSize)) ;													\
}																														\
																														\
PRIM PrimTypeCWord  primRotateLeft ## PrimTypeName( PrimTypeC x, Word y ) {												\
	PrimTypeC xx = x & Bits_Size2LoMask(Word64,PrimBitSize) ;															\
	return (PrimTypeC) (((xx << y) | (xx >> (PrimBitSize - y))) & Bits_Size2LoMask(Word64,PrimBitSize)) ;				\
}																														\
																														\
PRIM PrimTypeCWord  primRotateRight ## PrimTypeName( PrimTypeC x, Word y ) {											\
	PrimTypeC xx = x & Bits_Size2LoMask(Word64,PrimBitSize) ;															\
	return (PrimTypeC) (((xx >> y) | (xx << (PrimBitSize - y))) & Bits_Size2LoMask(Word64,PrimBitSize)) ;				\
}																														\
																														\

%%]



%%[99

#ifdef __UHC_TARGET_C__

#define PRIMS_STORABLE_CODE(PrimTypeName,PrimTypeC) 																	\
																														\
PRIM PrimTypeC  primRead ## PrimTypeName ## OffAddr( Word ptr, Word off ) {										\
	return ((PrimTypeC*)ptr)[ off ] ;																									\
}																														\
																														\
PRIM Word  primWrite ## PrimTypeName ## OffAddr( Word ptr, Word off, Word val ) {							\
	((PrimTypeC*)ptr)[ off ] = (PrimTypeC) val ;																									\
	return (Word)RTS_Unit ;																								\
}																														\


#else

#define PRIMS_STORABLE_CODE(PrimTypeName,PrimTypeC) 																	\
																														\
PRIM PrimTypeC  primRead ## PrimTypeName ## OffAddr( PrimTypeC* ptr, Word off ) {										\
	return ptr[ off ] ;																									\
}																														\
																														\
PRIM Word  primWrite ## PrimTypeName ## OffAddr( PrimTypeC* ptr, Word off, PrimTypeC val ) {							\
	ptr[ off ] = val ;																									\
	return (Word)RTS_Unit ;																								\
}																														\

#endif

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __PRIMINLINE_H__ */
%%]
