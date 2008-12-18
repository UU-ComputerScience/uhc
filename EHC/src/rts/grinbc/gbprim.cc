%%[8
#include "../rts.h"
%%]

%%[97
#include <alloca.h>
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives for grin bytecode interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM GB_NodePtr gb_Unit 
	= NULL ;

PRIM GB_Word gb_False
	= GB_MkConEnumNodeAsTag( 0 ) ;
PRIM GB_Word gb_True
	= GB_MkConEnumNodeAsTag( 1 ) ;

PRIM GB_Node gb_Nil
    = GB_MkConEnumNode( GB_Tag_List_Nil ) ;

PRIM GB_Word gb_EQ
	= GB_MkConEnumNodeAsTag( 0 ) ;
PRIM GB_Word gb_GT
	= GB_MkConEnumNodeAsTag( 1 ) ;
PRIM GB_Word gb_LT
	= GB_MkConEnumNodeAsTag( 2 ) ;

%%]

%%[98
PRIM GB_Node gb_Nothing
    = GB_MkConEnumNode( GB_Tag_Maybe_Nothing ) ;
%%]

The definition of IOErrorType must coincide with the one in Prelude.hs

%%[98
PRIM GB_Word gb_AlreadyExists
	= GB_MkConEnumNodeAsTag( 0 ) ;
PRIM GB_Word gb_AlreadyInUse
	= GB_MkConEnumNodeAsTag( 1 ) ;
PRIM GB_Word gb_DoesNotExist	
	= GB_MkConEnumNodeAsTag( 2 ) ;
PRIM GB_Word gb_EOF
	= GB_MkConEnumNodeAsTag( 3 ) ;
PRIM GB_Word gb_FullError
	= GB_MkConEnumNodeAsTag( 4 ) ;
PRIM GB_Word gb_IllegalOperation
	= GB_MkConEnumNodeAsTag( 5 ) ;
PRIM GB_Word gb_PermissionDenied
	= GB_MkConEnumNodeAsTag( 6 ) ;
PRIM GB_Word gb_ResourceExhausted
	= GB_MkConEnumNodeAsTag( 7 ) ;
PRIM GB_Word gb_UserError
	= GB_MkConEnumNodeAsTag( 8 ) ;
%%]

The definition of IOMode must coincide with the one in Prelude.hs

%%[98
PRIM GB_Word gb_AppendBinaryMode
	= GB_MkConEnumNodeAsTag( 0 ) ;
PRIM GB_Word gb_AppendMode
	= GB_MkConEnumNodeAsTag( 1 ) ;
PRIM GB_Word gb_ReadBinaryMode
	= GB_MkConEnumNodeAsTag( 2 ) ;
PRIM GB_Word gb_ReadMode
	= GB_MkConEnumNodeAsTag( 3 ) ;
PRIM GB_Word gb_ReadWriteBinaryMode
	= GB_MkConEnumNodeAsTag( 4 ) ;
PRIM GB_Word gb_ReadWriteMode
	= GB_MkConEnumNodeAsTag( 5 ) ;
PRIM GB_Word gb_WriteBinaryMode
	= GB_MkConEnumNodeAsTag( 6 ) ;
PRIM GB_Word gb_WriteMode
	= GB_MkConEnumNodeAsTag( 7 ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[11
PRIM GB_Word gb_primUnsafeId( GB_Word x )
{
	return x ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Float gb_primIntToFloat( GB_Int x )
{
	// GB_NodePtr n ;
	// GB_NodeAlloc_Float_In(n) ;
	// n->content.flt = (float)GB_GBInt2Int(x) ;
	// n->content.flt = (float)x ;
	return Cast(GB_Float,x) ;
}

PRIM GB_Float gb_primDoubleToFloat( GB_Double /*nd*/ x )
{
	//GB_NodePtr nf ;
	//GB_NodeAlloc_Float_In(nf) ;
	//nf->content.flt = nd->content.dbl ;
	//return nf ;
	return Cast(GB_Float,x) ;
}

PRIM GB_Double gb_primFloatToDouble( GB_Float x )
{
	//GB_NodePtr nd ;
	//GB_NodeAlloc_Double_In(nd) ;
	//nd->content.dbl = nf->content.flt ;
	//return nd ;
	return Cast(GB_Double,x) ;
}

PRIM GB_Double gb_primIntToDouble( GB_Int x )
{
	// GB_NodePtr n ;
	// GB_NodeAlloc_Double_In(n) ;
	// n->content.dbl = (double)GB_GBInt2Int(x) ;
	// n->content.dbl = (double)x ;
	// return n ;
	return Cast(GB_Double,x) ;
}

#if USE_GMP
PRIM GB_Float gb_primRationalToFloat( GB_NodePtr nr )
{
	// GB_NodePtr nf ;
	GB_NodePtr numerator, divisor ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(nr) ;
	GB_GC_Safe2_Zeroed(numerator, divisor) ;
	GB_PassExc_Cast_GCSafe( GB_Word, numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ) ;
	GB_PassExc_Cast_GCSafe( GB_Word, divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ) ;
	GB_Float res ;
	res = Cast( GB_Float, mpz_get_d( numerator->content.mpz ) / mpz_get_d( divisor->content.mpz ) ) ;
	GB_GC_SafeLeave ;
	return res ;
}

PRIM GB_Double gb_primRationalToDouble( GB_NodePtr nr )
{
	// GB_NodePtr nf ;
	GB_NodePtr numerator, divisor ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(nr) ;
	GB_GC_Safe2_Zeroed(numerator, divisor) ;
	GB_PassExc_Dflt_GCSafe( 0.0, numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ) ;
	GB_PassExc_Dflt_GCSafe( 0.0, divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ) ;
	// GB_NodeAlloc_Double_In(nf) ;
	// nf->content.dbl = mpz_get_d( numerator->content.mpz ) / mpz_get_d( divisor->content.mpz ) ;
	// return nf ;
	GB_GC_SafeLeave ;
	return Cast( GB_Double, mpz_get_d( numerator->content.mpz ) / mpz_get_d( divisor->content.mpz ) ) ;
}

PRIM GB_Float gb_primIntegerToFloat( GB_NodePtr n )
{
	// GB_NodePtr nf ;
	// GB_NodeAlloc_Float_In(nf) ;
	// nf->content.flt = mpz_get_d( n->content.mpz ) ;		// not sure whether this works without explicit truncation or something like that...
	// return nf ;
	return Cast( GB_Float, mpz_get_d( n->content.mpz ) ) ;
}

PRIM GB_Double gb_primIntegerToDouble( GB_NodePtr n )
{
	// GB_NodePtr nf ;
	// GB_NodeAlloc_Double_In(nf) ;
	// nf->content.dbl = mpz_get_d( n->content.mpz ) ;
	// return nf ;
	return Cast( GB_Double, mpz_get_d( n->content.mpz ) ) ;
}

PRIM GB_Word gb_primIntegerToInt( GB_NodePtr n )
{
	// return GB_Int2GBInt( mpz_get_si( n->content.mpz ) ) ;
	return ( mpz_get_si( n->content.mpz ) ) ;
}

PRIM GB_NodePtr gb_primCStringToInteger( char* s )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_set_str( n->content.mpz, s, 10 ) ;
	return n ;
}

PRIM GB_NodePtr gb_primIntToInteger( GB_Int x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetInt_In( n, x ) ;
	return n ;
}

PRIM GB_NodePtr gb_primFloatToInteger( GB_Float x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetDbl_In( n, x ) ;
	return n ;
}

PRIM GB_NodePtr gb_primDoubleToInteger( GB_Double x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetDbl_In( n, x ) ;
	return n ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM GB_Word gb_primAddInt( GB_Word x, GB_Word y )
{
	// IF_GB_TR_ON(3,printf("gb_primAddInt %d(%d)+%d(%d)=%d(%d)\n", GB_GBInt2Int(x), x, GB_GBInt2Int(y), y, GB_GBInt2Int(GB_Int_Add(x,y)), GB_Int_Add(x,y) );) ;
  	// return GB_Int_Add(x,y);
  	return x+y;
}

PRIM GB_Word gb_primSubInt( GB_Word x, GB_Word y )
{
	// IF_GB_TR_ON(3,printf("gb_primSubInt %d(%d)-%d(%d)=%d(%d)\n", GB_GBInt2Int(x), x, GB_GBInt2Int(y), y, GB_GBInt2Int(GB_Int_Sub(x,y)), GB_Int_Sub(x,y) );) ;
  	// return GB_Int_Sub(x,y);
  	return x-y;
}

PRIM GB_Word gb_primMulInt( GB_Word x, GB_Word y )
{
	// IF_GB_TR_ON(3,printf("gb_primMulInt %d(%d)*%d(%d)=%d(%d)\n", GB_GBInt2Int(x), x, GB_GBInt2Int(y), y, GB_GBInt2Int(GB_Int_Mul(x,y)), GB_Int_Mul(x,y) );) ;
  	// return GB_Int_Mul(x,y);
  	return x*y;
}

/* DivInt and ModInt use euclidean division, ie.
   the modulus is always positive.
   
   These routines are taken from lvm.
*/

PRIM GB_Word gb_primDivInt( GB_Int numerator, GB_Int divisor )
{
	GB_Int div = numerator / divisor ;
	
	// todo: if ( divisor == 0 ) ...
	
	/* adjust to euclidean division */
	if ( div < 0 ) {
		div -= 1 ;
	}
	
  	return (div) ;
}
%%]

%%[8
PRIM GB_Word gb_primModInt( GB_Int numerator, GB_Int divisor )
{
	GB_Int mod = numerator % divisor ;
	
	// todo: if ( divisor == 0 ) ...
	
	/* adjust to euclidean modulus */
	if ( mod > 0 && divisor < 0 || mod < 0 && divisor > 0 ) {
		mod += divisor ;
	}
	
  	return (mod) ;
}

/* QuotInt and RemInt use truncated division, ie.
   QuotInt D d = trunc(D/d)
   RemInt D d  = D - d*(QuotInt D d)
*/

PRIM GB_Int gb_primQuotInt( GB_Int x, GB_Int y )
{
	// todo: if ( divisor == 0 ) ...
	
  	// return GB_Int_Quot(x,y);
  	GB_Int res = x / y ;
  	return res ;
}

PRIM GB_Int gb_primRemInt( GB_Int x, GB_Int y )
{
	// todo: if ( divisor == 0 ) ...
	
  	// return GB_Int_Rem(x,y);
  	GB_Int res = x % y ;
  	return res ;
}

PRIM GB_NodePtr gb_primQuotRemInt( GB_Int x, GB_Int y )
{
	GB_NodePtr n ;
	GB_Int q = x / y ;
	GB_Int r = x % y ;
  	// printf( "gb_primQuotRemInt %d %d %d %d\n", x, y, q, r ) ;
	GB_MkTupNode2_In(n,GB_Int2GBInt(q),GB_Int2GBInt(r)) ;
	return n ;
}

PRIM GB_NodePtr gb_primDivModInt( GB_Int x, GB_Int y )
{
	GB_NodePtr n ;
	GB_MkTupNode2_In(n, GB_Int2GBInt( gb_primDivInt(x,y) ), GB_Int2GBInt( gb_primModInt(x,y) )) ;
	return n ;
}

%%]

%%[8
PRIM GB_Int gb_primNegInt( GB_Int x )
{
	// return GB_Int_Neg(x) ;
	return -(x) ;
}
%%]

%%[8
PRIM GB_Word gb_primEqInt( GB_Int x, GB_Int y )
{
	if ( x == y )
		return gb_True ;
  	return gb_False ;
}

PRIM GB_Word gb_primGtInt( GB_Int x, GB_Int y )
{
	if ( x > y )
		return gb_True ;
  	return gb_False ;
}

PRIM GB_Word gb_primLtInt( GB_Int x, GB_Int y )
{
	if ( x < y )
		return gb_True ;
  	return gb_False ;
}

PRIM GB_Word gb_primCmpInt( GB_Int x, GB_Int y )
{
	if ( x < y )
		return gb_LT ;
	else if ( x == y )
		return gb_EQ ;
  	return gb_GT ;
}

%%]

%%[95
PRIM GB_Word gb_primMaxInt()
{
  	// return GB_Int2GBInt(Bits_MaxSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
  	return (Bits_MaxSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
}

PRIM GB_Word gb_primMinInt()
{
  	// return GB_Int2GBInt(Bits_MinSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)+1) ;
  	return (Bits_MinSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)+1) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Double/Float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

We do not know for sure whether we use IEEE or not.
This should be dependending on some compile time C info, but not yet sorted out.

%%[97
PRIM GB_Word gb_primIsIEEE( )
{
	return gb_True ;
}

%%]

%%[97
PRIM GB_Word gb_primRadixDoubleFloat( )
{
	return FLT_RADIX ;
}

%%]

%%[97
#define gb_intlDecode(ty,x)																	\
{																							\
	int exp ;																				\
	int mantdig = ( sizeof(ty) == sizeof(GB_Double) ? DBL_MANT_DIG : FLT_MANT_DIG ) ;		\
	ty mant = ( sizeof(ty) == sizeof(GB_Double) ? frexp( x, &exp ) : frexpf( x, &exp) ) ;	\
	if ( fpclassify(x) == FP_ZERO ) {														\
		exp = 0 ;																			\
	} else {																				\
		exp -= mantdig ;																	\
	}																						\
	GB_NodePtr n, ni ;																		\
	GB_GC_SafeEnter ;																		\
	GB_GC_Safe2_Zeroed(n,ni) ;																		\
	GB_NodeAlloc_Mpz_SetDbl_In( ni, ldexp( mant, mantdig ) ) ;								\
	GB_MkTupNode2_In(n,ni,GB_Int2GBInt(exp)) ;												\
	GB_GC_SafeLeave ;																		\
	return n ;																				\
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Word gb_primEqFloat( GB_Float x, GB_Float y )
{
	return GB_CmpBasic(x,y, gb_False, gb_True, gb_False ) ;
}

PRIM GB_Word gb_primCmpFloat( GB_Float x, GB_Float y )
{
	return GB_CmpBasic(x,y, gb_LT, gb_EQ, gb_GT ) ;
}

PRIM GB_Float gb_primAddFloat( GB_Float x, GB_Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Add_In(n,x,y) ;
	// return n ;
	return x + y ;
}

PRIM GB_Float gb_primSubFloat( GB_Float x, GB_Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Sub_In(n,x,y) ;
	// return n ;
	return x - y ;
}

PRIM GB_Float gb_primMulFloat( GB_Float x, GB_Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Mul_In(n,x,y) ;
	// return n ;
	return x * y ;
}

PRIM GB_Float gb_primDivFloat( GB_Float x, GB_Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Div_In(n,x,y) ;
	// return n ;
	return x / y ;
}

PRIM GB_Float gb_primNegFloat( GB_Float x )
{
	// GB_NodePtr n ;
	// GB_Float_Neg_In(n,x) ;
	// return n ;
	return -x ;
}

PRIM GB_Word gb_primIsNaNFloat( GB_Float x )
{
	if ( isnan(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsDenormalizedFloat( GB_Float x )
{
	if ( ! isnormal(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsInfiniteFloat( GB_Float x )
{
	if ( isinf(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsNegativeZeroFloat( GB_Float x )
{
	 
	if ( fpclassify(x) == FP_ZERO && signbit(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primDigitsFloat( )
{
	return FLT_MANT_DIG ;
}

PRIM GB_Word gb_primMaxExpFloat( )
{
	return FLT_MAX_EXP ;
}

PRIM GB_Word gb_primMinExpFloat( )
{
	return FLT_MIN_EXP ;
}

PRIM GB_NodePtr gb_primDecodeFloat( GB_Float x )
	gb_intlDecode(GB_Float,x)

PRIM GB_Float gb_primEncodeFloat( GB_NodePtr frac, GB_Word exp )
{
	GB_Float d = ldexp( mpz_get_d( frac->content.mpz ), exp ) ;
	return d ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Double
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Word gb_primEqDouble( GB_Double x, GB_Double y )
{
	return GB_CmpBasic(x,y, gb_False, gb_True, gb_False ) ;
}

PRIM GB_Word gb_primCmpDouble( GB_Double x, GB_Double y )
{
	return GB_CmpBasic(x,y, gb_LT, gb_EQ, gb_GT ) ;
}

PRIM GB_Double gb_primAddDouble( GB_Double x, GB_Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Add_In(n,x,y) ;
	// return n ;
	return x + y ;
}

PRIM GB_Double gb_primSubDouble( GB_Double x, GB_Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Sub_In(n,x,y) ;
	// return n ;
	return x - y ;
}

PRIM GB_Double gb_primMulDouble( GB_Double x, GB_Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Mul_In(n,x,y) ;
	// return n ;
	return x * y ;
}

PRIM GB_Double gb_primDivDouble( GB_Double x, GB_Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Div_In(n,x,y) ;
	// return n ;
	return x / y ;
}

PRIM GB_Double gb_primNegDouble( GB_Double x )
{
	// GB_NodePtr n ;
	// GB_Double_Neg_In(n,x) ;
	// return n ;
	return -x ;
}

PRIM GB_Word gb_primIsNaNDouble( GB_Double x )
{
	if ( isnan(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsDenormalizedDouble( GB_Double x )
{
	if ( ! isnormal(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsInfiniteDouble( GB_Double x )
{
	if ( isinf(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsNegativeZeroDouble( GB_Double x )
{
	 
	if ( fpclassify(x) == FP_ZERO && signbit(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primDigitsDouble( )
{
	return DBL_MANT_DIG ;
}

PRIM GB_Word gb_primMaxExpDouble( )
{
	return DBL_MAX_EXP ;
}

PRIM GB_Word gb_primMinExpDouble( )
{
	return DBL_MIN_EXP ;
}

PRIM GB_NodePtr gb_primDecodeDouble( GB_Double x )
	gb_intlDecode(GB_Double,x)

PRIM GB_Double gb_primEncodeDouble( GB_NodePtr frac, GB_Word exp )
{
	GB_Double d = ldexp( mpz_get_d( frac->content.mpz ), exp ) ;
	return d ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer, via GMP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!!!!! Todo: adapt allocation for GMP, GC works improperly here because can create race conditions.

%%[97
#if USE_GMP
PRIM GB_Word gb_primEqInteger( GB_NodePtr x, GB_NodePtr y )
{
	if ( GB_Integer_Cmp(x,y) == 0 )
		return gb_True ;
  	return gb_False ;
}

PRIM GB_Word gb_primCmpInteger( GB_NodePtr x, GB_NodePtr y )
{
	int c = GB_Integer_Cmp(x,y) ;
	if ( c < 0 )
		return gb_LT ;
	else if ( c == 0 )
		return gb_EQ ;
  	return gb_GT ;
}

PRIM GB_NodePtr gb_primAddInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Add_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primSubInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Sub_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primMulInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Mul_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primDivInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Div_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primModInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Mod_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primQuotInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Quot_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primRemInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Rem_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primQuotRemInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n1, n2 ;
	GB_GC_SafeEnter ;
	GB_GC_Safe2(x,y) ;
	GB_GC_Safe3_Zeroed(n,n1,n2) ;
	GB_Integer_QuotRem_In(n1,n2,x,y) ;
	GB_MkTupNode2_In(n,n1,n2) ;
	GB_GC_SafeLeave ;
	return n ;
}

PRIM GB_NodePtr gb_primDivModInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n1, n2 ;
	GB_GC_SafeEnter ;
	GB_GC_Safe2(x,y) ;
	GB_GC_Safe3_Zeroed(n,n1,n2) ;
	GB_Integer_DivMod_In(n1,n2,x,y) ;
	GB_MkTupNode2_In(n,n1,n2) ;
	GB_GC_SafeLeave ;
	return n ;
}
#endif
%%]

%%[97
#if USE_GMP
PRIM GB_NodePtr gb_primNegInteger( GB_NodePtr x )
{
	GB_NodePtr n ;
	GB_Integer_Neg_In(n,x) ;
	return n ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Char
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM GB_Word gb_primCharIsUpper( GB_Int x )
{
	// char c = GB_GBInt2Int( x ) ;
	char c = ( x ) ;
	if ( c >= 'A' && c <= 'Z' )
		return gb_True ;
  	return gb_False ;
}

PRIM GB_Word gb_primCharIsLower( GB_Int x )
{
	// char c = GB_GBInt2Int( x ) ;
	char c = ( x ) ;
	if ( c >= 'a' && c <= 'z' )
		return gb_True ;
  	return gb_False ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% String
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

PRIM GB_Word gb_primPackedStringNull( char *s )
{
  	IF_GB_TR_ON(3,printf("gb_primPackedStringNull %x, %d, %s\n", s, *s, s ););
	if ( *s )
		return gb_False ;
  	return gb_True ;
}

PRIM GB_Word gb_primPackedStringTail( char *s )
{
  	IF_GB_TR_ON(3,printf("gb_primPackedStringTail %x, %d, %s\n", s, *s, s ););
  	return Cast(GB_Word,s+1) ;
}

/*
GB_NodePtr gb_primPackedStringTail( char *s )
{
  	GB_NodePtr n ;
  	GB_MkPackedString(n,Cast(GB_NodePtr,s+1)) ;
  	return n ;
}
*/

PRIM GB_Word gb_primPackedStringHead( char *s )
{
  	// IF_GB_TR_ON(3,printf("gb_primPackedStringHead %x, %d, %s, %d\n", s, *s, s, GB_Int2GBInt(*s) ););
  	// return Cast(GB_Word,GB_Int2GBInt(*s)) ;
  	return Cast(GB_Word,(*s)) ;
}

%%]



/* Old implementation of packedStringToString
   deprecated because it allocates cells, and introduces a new thunk tag,
   which is not possible in the Grin Compiler.
   In order to make BC an GrinC use the same prelude, we replaced
   the PackedString handling by the functions above
*/

The implementation is left here for the GB based impl.

%%[8
GB_NodePtr gb_primCStringToString1Char( char* s, GB_Int goff )
{
	char c = s[ GB_GBInt2Int(goff) ] ;
  	GB_NodePtr n, n2 ;
	GB_GC_SafeEnter ;
  	GB_GC_Safe2_Zeroed(n,n2) ;
  	IF_GB_TR_ON(3,printf("gb_primCStringToString1Char1 %x:'%s'[%d]\n", s, s, GB_GBInt2Int(goff) ););
	if ( c ) {
		GB_MkCFunNode2In(n2,&gb_primCStringToString1Char,s,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("gb_primCStringToString1Char2 n %x\n", n ););
  	GB_GC_SafeLeave ;
  	return n ;
}

PRIM GB_NodePtr gb_primCStringToString( char* s )
{
  	return gb_primCStringToString1Char( s, GB_Int0 ) ;
}

// temporary:
PRIM GB_NodePtr gb_priv_primCStringToString( char* s )
{
  	return gb_primCStringToString1Char( s, GB_Int0 ) ;
}
%%]







In the following function GB_List_Iterate causes a Bus error when:
  - compiled on MacIntel, gcc 4.01
  - with -O3
  - without additional trace statements.
  Sigh...

%%[8

PRIM GB_NodePtr gb_primTraceStringExit( GB_NodePtr n )
{
	char buf[100] ;
	int bufInx = 0 ;
	int sz = 99 ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(n) ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit1 n %x\n", n ););
%%[[8
	gb_listForceEval( &n, &sz ) ;
%%][96
	GB_PassExc_GCSafe( gb_listForceEval( &n, &sz ) ) ;
%%]]
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit2 n %x\n", n ););
	GB_List_Iterate(n,sz,{buf[bufInx++] = GB_GBInt2Int(GB_List_Head(n));}) ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit3 n %x\n", n ););
	buf[bufInx] = 0 ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit4 `%s'\n", buf ););
	GB_GC_SafeLeave ;
	gb_error( buf ) ;
	return n ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exiting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This version also prints a stacktrace

%%[96.gb_primExitWith
PRIM GB_Word gb_primExitWith( GB_Word exitCode )
{
	// gb_exit( GB_GBInt2Int( exitCode ) ) ;
	gb_exit( exitCode ) ;
	return exitCode ; // for now
}
%%]

Whereas this is left to the runtime wrapper (see EHC.Prelude.ehcRunMain) in this version

%%[99 -96.gb_primExitWith
PRIM GB_Word gb_primExitWith( GB_Word exitCode )
{
	// rts_exit( GB_GBInt2Int( exitCode ) ) ;
	rts_exit( exitCode ) ;
	gb_panic( "impossible: exit failed" ) ;
	return exitCode ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Byte array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95
GB_NodePtr gb_primByteArrayToString1Char( GB_NodePtr mn, GB_Int goff )
{
	char* s = Cast(char*,mn->content.bytearray.ptr) ;
	int   igoff = GB_GBInt2Int(goff) ;
	char  c = s[ igoff ] ;
  	GB_NodePtr n, n2 ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(mn) ;
	GB_GC_Safe2_Zeroed(n,n2) ;
  	IF_GB_TR_ON(3,printf("gb_primByteArrayToString1Char %x:'%s'[%d]\n", s, s, igoff ););
	if ( igoff < mn->content.bytearray.size && c ) {
		GB_MkCFunNode2In(n2,&gb_primByteArrayToString1Char,mn,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("gb_primByteArrayToString1Char n %x\n", n ););
	GB_GC_SafeLeave ;
  	return n ;
}

PRIM GB_NodePtr gb_primByteArrayToString( GB_Word a )
{
	GB_NodePtr n ;
%%[[95
	n = Cast( GB_NodePtr, gb_eval( a ) ) ;
%%][96
	GB_PassExc( n = Cast( GB_NodePtr, gb_eval( a ) ) ) ;
%%]]
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}

PRIM GB_Word gb_primByteArrayLength( GB_Word a )
{
	GB_NodePtr n ;
%%[[95
	n = Cast( GB_NodePtr, gb_eval( a ) ) ;
%%][96
	GB_PassExc_CastAsWord( n = Cast( GB_NodePtr, gb_eval( a ) ) ) ;
%%]]
  	// return GB_Int2GBInt(n->content.bytearray.size) ;
  	return (n->content.bytearray.size) ;
}

/*
  In the following function gb_eval(GB_List_Head(n)) must be stored into a local var (here: xx),
  inlining produces a faulty program.
  Reason unknown :-(.
*/

PRIM GB_NodePtr gb_primStringToByteArray( GB_NodePtr n, GB_Int sz )
{
	GB_NodePtr n2 ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(n) ;
	GB_GC_Safe1_Zeroed(n2) ;
  	IF_GB_TR_ON(3,printf("gb_primStringToByteArray1 sz=%d n=%x\n", sz, n ););
%%[[95
	gb_listForceEval( &n, (int*) &sz ) ;
%%][96
	GB_PassExc_GCSafe( gb_listForceEval( &n, (int*) &sz ) ) ;
%%]]
  	IF_GB_TR_ON(3,printf("gb_primStringToByteArray2 sz=%d n=%x\n", sz, n ););
	GB_NodeAlloc_Malloc2_In( sz, n2 ) ;
	GB_BytePtr s = Cast(GB_BytePtr,n2->content.bytearray.ptr) ;
	int bufInx = 0 ;
%%[[95
	GB_List_Iterate(n,sz,{GB_Word xx = gb_eval(GB_List_Head(n)); s[bufInx++] = GB_GBInt2Int(xx);}) ;
%%][96
	GB_List_Iterate(n,sz,{GB_Word xx ; GB_PassExc_GCSafe(xx = gb_eval(GB_List_Head(n))); s[bufInx++] = GB_GBInt2Int(xx);}) ;
%%]]
	// does not work: GB_List_Iterate(n,sz,{s[bufInx++] = GB_GBInt2Int(gb_eval(GB_List_Head(n)));}) ;
  	IF_GB_TR_ON(3,printf("gb_primStringToByteArray4 bufInx=%d, n=%x buf=", bufInx, n ););
  	IF_GB_TR_ON(3,{int i ; for (i = 0 ; i < bufInx ; i++) {printf(" %d",s[i]);};});
  	IF_GB_TR_ON(3,printf("\n"););
	GB_GC_SafeLeave ;
	return n2 ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95
PRIM GB_NodePtr gb_primShowInt( GB_Int intNd )
{
	char buf[sizeof(GB_Word)*10] ;
	char *s = buf ;
	// int i = GB_GBInt2Int(intNd) ;
	int i = (intNd) ;
	if ( i < 0 )
	{
		i = -i ;
		*(s++) = '-' ;
	}
#	if USE_64_BITS
		sprintf( s, "%ld"
#	else
		sprintf( s, "%d"
#	endif
				, i ) ;
	
  	IF_GB_TR_ON(3,printf("gb_primShowInt s(%d) %s\n", strlen(buf), buf ););
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_Malloc2_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}
%%]

%%[97

PRIM GB_NodePtr gb_primShowFloat( GB_Float w )
{
	char buf[sizeof(GB_Float)*10] ;
	char *s = buf ;
	sprintf( s, "%f", w ) ;
	
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_Malloc2_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}

PRIM GB_NodePtr gb_primShowDouble( GB_Double w )
{
	char buf[sizeof(GB_Double)*10] ;
	char *s = buf ;
	sprintf( s, "%lf", w ) ;
	
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_Malloc2_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}
%%]

%%[97
#if USE_GMP
PRIM GB_NodePtr gb_primShowInteger( GB_NodePtr integerNd )
{
	GB_NodePtr n ;
	int sz = mpz_sizeinbase( integerNd->content.mpz, 10 ) + 2 ;
	char* buf = alloca( sz ) ;

	mpz_get_str( buf, 10, integerNd->content.mpz ) ;
	sz = strlen(buf) ;
	GB_NodeAlloc_Malloc2_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;

  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}
#endif
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception handling, program running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
PRIM GB_Word gb_primCatchException( GB_Word e, GB_Word handler )
{
	return gb_intl_primCatchException( e, handler ) ;
}

PRIM GB_NodePtr gb_primThrowException( GB_Word exc )
{
	return gb_intl_throwException( exc ) ;
}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% I/O: auxiliary functions needed in the implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98

GB_NodePtr gb_throwChanInteractionException( GB_NodePtr chan, char* strErr )
{
	GB_NodePtr ioe_handle ;
	GB_NodePtr ioe_filename ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(chan) ;
	GB_GC_Safe2_Zeroed(ioe_handle,ioe_filename) ;
	
	GB_MkMaybeJust( ioe_filename, chan->content.chan.name ) ;
	GB_MkMaybeJust( ioe_handle, chan ) ;

	GB_GC_SafeLeave ;
	return gb_intl_throwIOExceptionFromPrim( ioe_handle, gb_EOF, ioe_filename, strErr ) ;
}

GB_NodePtr gb_getChanEOFOrThrowExc( GB_NodePtr chan, Bool throwExcForEOF, Bool* isEof )
{
	FILE *f = chan->content.chan.file ;
	if ( feof( f ) ) {
		if ( throwExcForEOF ) {
			return gb_throwChanInteractionException( chan, "EOF reached" ) ;
		}
		*isEof = True ;
		return NULL ;
	} else {
		*isEof = False ;
		return NULL ;
	}
}

/*
 * Read+return a char,
 * unless at EOF which:
 *   throws an exc if throwExcForEOF
 *   or otherwise returns EOF in *isEof
 */

GB_NodePtr gb_ChanGetChar( GB_NodePtr chan, Bool throwExcForEOF, Bool* isEof, int* pc )
{
	FILE *f = chan->content.chan.file ;
	int c ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(chan) ;
	
	// printf( "%d ", feof( f ) ) ;
	GB_PassExc_GCSafe( gb_getChanEOFOrThrowExc( chan, throwExcForEOF, isEof ) ) ;
	if ( *isEof ) {
		c == EOF ;
	} else {
		c = getc( f ) ;
		if ( c == EOF ) {
			GB_PassExc_GCSafe( gb_getChanEOFOrThrowExc( chan, throwExcForEOF, isEof ) ) ;
		} else if ( c == '\r' && chan->content.chan.isText ) {
			int c2 = getc( f ) ;
			if ( c2 != '\n' && c2 != EOF ) {
				ungetc( c2, f ) ;
			}
			c = '\n' ;
		}
	}
	// printf( "%d %d %d\n", feof( f ), *isEof, c ) ;
	*pc = c ;
	GB_GC_SafeLeave ;
	return NULL ;
}


GB_NodePtr gb_ThrowWriteChanError( GB_NodePtr chan )
{
	GB_NodePtr ioe_handle ;
	GB_Word    ioe_type ;
	GB_NodePtr ioe_filename ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(chan) ;
	GB_GC_Safe2_Zeroed(ioe_handle,ioe_filename) ;

	GB_MkMaybeJust( ioe_handle, chan ) ;
	GB_MkMaybeJust( ioe_filename, chan->content.chan.name ) ;
	
	switch( errno ) {
		case ENOMEM :
		case ENOSPC :
			ioe_type = gb_FullError ;
			break ;
		case EFBIG :
		case EIO :
			ioe_type = gb_PermissionDenied ;
			break ;
		default :
			ioe_type = gb_PermissionDenied ;
			break ;
	}
	
	GB_GC_SafeLeave ;
	return gb_intl_throwIOExceptionFromPrim( ioe_handle, ioe_type, ioe_filename, strerror( errno ) ) ;
}
%%]




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% I/O: basic primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PRIM GB_NodePtr gb_primStdin()
{
  	return gb_chan_stdin ;
}

PRIM GB_NodePtr gb_primStdout()
{
  	IF_GB_TR_ON(3,printf("gb_primStdout\n" ););
  	return gb_chan_stdout ;
}

PRIM GB_NodePtr gb_primStderr()
{
  	return gb_chan_stderr ;
}


%%[98

PRIM GB_Word gb_primHFileno( GB_NodePtr chan )
{
	return ( fileno(chan->content.chan.file) ) ;
}

PRIM GB_NodePtr gb_primOpenFileOrStd( GB_NodePtr nmNd, GB_Word modeEnum, GB_NodePtr mbHandleNr )   
{
    /* mbHandleNr to be used only for std{in,out,err}, ignoring the opening mode. */
	int nmSz = 0 ;
	GB_Word mbHandleNrFromJust = 0 ;
	GB_GC_SafeEnter ;
	GB_GC_Safe4(mbHandleNrFromJust,nmNd,modeEnum,mbHandleNr) ;
	GB_PassExc_GCSafe( gb_listForceEval( &nmNd, &nmSz ) ) ;
	char* nm = alloca( nmSz + 1 ) ;
	GB_PassExc_GCSafe( gb_copyCStringFromEvalString( nm, nmNd, nmSz ) ) ;	
	nm[ nmSz ] = 0 ;

	GB_PassExc_GCSafe( mbHandleNr = Cast( GB_NodePtr, gb_eval( Cast(GB_Word,mbHandleNr) ) ) ) ;
	Bool mbHandleNrIsJust = False ;
	if ( GB_NH_Fld_Tag(mbHandleNr->header) == GB_Tag_Maybe_Just ) {
		mbHandleNrIsJust = True ;
		GB_PassExc_GCSafe( mbHandleNrFromJust = gb_eval( mbHandleNr->content.fields[0] ) ) ;
	}
	
	char *mode ;
	Bool isText = True ;
	if ( GB_EnumIsEqual( modeEnum, gb_ReadMode ) ) {
		mode = "r" ;
	} else if ( GB_EnumIsEqual( modeEnum, gb_ReadBinaryMode ) ) {
		mode = "rb" ;
		isText = False ;
	} else if ( GB_EnumIsEqual( modeEnum, gb_WriteMode ) ) {
		mode = "w" ;
	} else if ( GB_EnumIsEqual( modeEnum, gb_WriteBinaryMode ) ) {
		mode = "wb" ;
		isText = False ;
	} else if ( GB_EnumIsEqual( modeEnum, gb_ReadWriteMode ) ) {
		mode = "r+" ;
	} else if ( GB_EnumIsEqual( modeEnum, gb_ReadWriteBinaryMode ) ) {
		mode = "r+b" ;
		isText = False ;
	} else if ( GB_EnumIsEqual( modeEnum, gb_AppendMode ) ) {
		mode = "a" ;
	} else if ( GB_EnumIsEqual( modeEnum, gb_AppendBinaryMode ) ) {
		mode = "ab" ;
		isText = False ;
	}
	
	FILE *f = NULL ;
	if ( mbHandleNrIsJust ) {
		switch( GB_GBInt2Int( mbHandleNrFromJust ) ) {
			case 0: f = stdin  ; break ;
			case 1: f = stdout ; break ;
			case 2: f = stderr ; break ;
		}
	} else {
		f = fopen( nm, mode ) ;
	}
	if ( f == NULL )
	{
		GB_NodePtr ioe_handle ;
		GB_Word    ioe_type ;
		GB_NodePtr ioe_filename ;
		GB_GC_Safe2_Zeroed(ioe_handle,ioe_filename) ;

		GB_MkMaybeNothing( ioe_handle ) ;
		GB_MkMaybeJust( ioe_filename, nmNd ) ;
		
		switch ( errno ) {
			case ENODEV :
			case ENOENT :
				ioe_type = gb_DoesNotExist ;
				break ;
			case EPERM   :
			case EACCES  :
			case ENOTDIR :
			case EMFILE :
				ioe_type = gb_PermissionDenied ;
				break ;
			case EBUSY :
				ioe_type = gb_AlreadyInUse ;
				break ;
			default :
				ioe_type = gb_PermissionDenied ;
				break ;
		}

		GB_GC_SafeLeave ;
		return gb_intl_throwIOExceptionFromPrim( ioe_handle, ioe_type, ioe_filename, strerror( errno ) ) ;
	}
	
	GB_NodePtr chan ;
	GB_NodeAlloc_Chan_In(chan) ;
	chan->content.chan.file = f ;
	chan->content.chan.name = nmNd ;
	chan->content.chan.isText = isText ;
	
	GB_GC_SafeLeave ;
	return chan ;
}

PRIM GB_NodePtr gb_primHClose( GB_NodePtr chan )
{
	fclose(chan->content.chan.file) ;
	return gb_Unit ;
}

PRIM GB_NodePtr gb_primHFlush( GB_NodePtr chan )
{
	fflush( chan->content.chan.file ) ;
	return gb_Unit ;
}

PRIM GB_Word gb_primHGetChar( GB_NodePtr chan )
{
	Bool isEof ;
	int c ;
	GB_PassExc_CastAsWord( gb_ChanGetChar( chan, True, &isEof, &c ) ) ;
	// return Cast(GB_NodePtr,GB_Int2GBInt(c)) ;
	return Cast(GB_Word,c) ;
}

PRIM GB_NodePtr gb_primHPutChar( GB_NodePtr chan, GB_Word c )
{	
	// int c2 = putc( GB_GBInt2Int(c), chan->content.chan.file ) ;
	int c2 = putc( (c), chan->content.chan.file ) ;
	if (c2 == EOF) {
		GB_PassExc( gb_ThrowWriteChanError( chan ) ) ;
	}
	return gb_Unit ;
}


%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% I/O: additional primitives for enhanced performance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98

PRIM GB_NodePtr gb_primHGetContents( GB_NodePtr chan )
{
	Bool isEof ;
	GB_NodePtr res, n ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(chan) ;
	GB_GC_Safe2_Zeroed(n,res) ;

	int c ;
	GB_PassExc_GCSafe( gb_ChanGetChar( chan, False, &isEof, &c ) ) ;
	if ( isEof ) {
		GB_MkListNil( res ) ;
	} else if ( c == EOF ) {
		GB_GC_SafeLeave ;
		return gb_throwChanInteractionException( chan, strerror( errno ) ) ;
	} else {
		GB_MkCFunNode1In(n,&gb_primHGetContents,chan) ;
		GB_MkListCons(res,GB_Int2GBInt(c),n) ;
	}
	
	GB_GC_SafeLeave ;
	return res ;
}

PRIM GB_NodePtr gb_primHPutByteArray( GB_NodePtr chan, GB_NodePtr a )
{
  	IF_GB_TR_ON(3,printf("gb_primWriteChan sz %d\n", a->content.bytearray.size ););
  	size_t szWritten ;
	szWritten = fwrite( a->content.bytearray.ptr, 1, a->content.bytearray.size, chan->content.chan.file ) ;
	if (szWritten != a->content.bytearray.size) {
		GB_PassExc( gb_ThrowWriteChanError( chan ) ) ;
	}
	return gb_Unit ;
}


%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM GB_NodePtr gb_primGetProgArgv( )
{
	GB_NodePtr res ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(res) ;
	GB_MkListNil( res ) ;
	
	int i ;
	for ( i = rtsArgC - 1 ; i >= 0 ; i-- ) {
		GB_NodePtr n1, n2 ;
		n2 = gb_primCStringToString( rtsArgV[i] ) ;
		GB_MkListCons(n1,n2,res) ;
		res = n1 ;
	}
	
	GB_GC_SafeLeave ;
	return res ;
}
%%]

%%[99
PRIM GB_Word gb_primGetArgC()
{
	return Cast(GB_Word,primGetArgC()) ;
}

PRIM GB_Word gb_primGetArgVAt( GB_Word argc )
{
	return Cast(GB_Word,primGetArgVAt(argc)) ;
}

%%]
