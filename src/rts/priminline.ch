%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives which are not supposed to be used directly,
%%% but indirectly by being used/bound in other primitives.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One macro which expands to interface defs for primitives for a primitive type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define IntLikeArithPrimsInterface(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord,PrimNodePtr) 										\
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

#define IntLikeBoundedPrimsInterface(PrimPrefix,PrimTypeName,PrimTypeC) 															\
																																	\
extern PrimTypeC        PrimPrefix ## primMax   ## PrimTypeName()	;																\
extern PrimTypeC        PrimPrefix ## primMin   ## PrimTypeName()	;																\

#define IntLikeIntConversionPrimsInterface(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord) 											\
																																	\
extern PrimTypeC        PrimPrefix ## primIntTo ## PrimTypeName( PrimTypeWord x ) ;													\
extern PrimTypeWord     PrimPrefix ## prim      ## PrimTypeName ## ToInt( PrimTypeC x ) ;											\

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One macro which expands to code for primitives for a primitive type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define IntLikeArithPrimsCode(PrimPrefix,PrimTypeName,PrimTypeC,PrimFalse,PrimTrue,PrimLT,PrimEQ,PrimGT,PrimTypeWord) 				\
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
#define IntLikeIntConversionPrimsCode(PrimPrefix,PrimTypeName,PrimTypeC,PrimTypeWord) \
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
#define IntLikeBoundedPrimsCode(PrimPrefix,PrimTypeName,PrimTypeC) \
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

