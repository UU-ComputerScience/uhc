%%[8
#include "../rts.h"
#include "interpreter.h"
%%]

%%[97
#if USE_GMP
#include "gmp.h"
#endif
%%]

%%[97
#include <alloca.h>
%%]

%%[8.dummyForLinker
int dummy_integer ;
%%]

%%[97 -8.dummyForLinker
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives for grin bytecode interpreter, those related to Integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GMP specific definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
#define GB_NodeMpzSize						(EntierUpDivBy(sizeof(mpz_t),sizeof(Word)) + 1)
#define GB_MkMpzHeader						GB_MkHeader(GB_NodeMpzSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_GMP_mpz)

#if ! USE_EHC_MM
#define GB_NodeGMPSize(nBytes)				(EntierUpDivBy(nBytes,sizeof(Word)) + 1)
#define GB_MkGMPHeader(sz)					GB_MkHeader(sz, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_GMP_intl)
#endif
#endif
%%]

%%[97
#if USE_GMP
#define GB_NodeAlloc_Mpz_In(n)				{ GB_NodeAlloc_Hdr_In(GB_NodeMpzSize,GB_MkMpzHeader,n) ; mpz_init(MPZ(n)) ; }
#if ! USE_EHC_MM
#define GB_NodeAlloc_GMP_In(nBytes,n)		{ int sz = GB_NodeGMPSize(nBytes) ; GB_NodeAlloc_Hdr_In(sz,GB_MkGMPHeader(sz),n) ; }
#endif
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Access
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mpz_t is defined as an array, this makes casting problematic (compiler complains),
so a pointer based definition is used. However, __mpz_struct is an internal type which may
undergo name changes as GMP versions progress.

%%[97
#if USE_GMP
typedef __mpz_struct*  GB_mpz ;

#define MPZ(n)								(GB_mpz)((n)->content.fields)
#endif
%%]

%%[97
#if USE_GMP
#define GB_NodeAlloc_Mpz_SetSignedInt_In(n,x)		{ GB_NodeAlloc_Mpz_In(n) ; mpz_set_si( MPZ(n), (x) ) ; }
#define GB_NodeAlloc_Mpz_SetUnsignedInt_In(n,x)		{ GB_NodeAlloc_Mpz_In(n) ; mpz_set_ui( MPZ(n), (x) ) ; }
#define GB_NodeAlloc_Mpz_SetDbl_In(n,x)				{ GB_NodeAlloc_Mpz_In(n) ; mpz_set_d( MPZ(n), (x) ) ; }
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer via GMP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
#define GB_Integer_Op1_In1(op,z,x)			{ GB_GCSafe_Enter ; GB_GCSafe_1(x) ; GB_NodeAlloc_Mpz_In(z) ; op( MPZ(z), MPZ(x) ) ; GB_GCSafe_Leave ; }
#define GB_Integer_Op2_In1(op,z,x,y)		{ GB_GCSafe_Enter ; GB_GCSafe_2(x,y) ; GB_NodeAlloc_Mpz_In(z) ; op( MPZ(z), MPZ(x), MPZ(y) ) ; GB_GCSafe_Leave ; }
#define GB_Integer_Op2b_In1(op,z,x,y)		{ GB_GCSafe_Enter ; GB_GCSafe_1(x) ; GB_NodeAlloc_Mpz_In(z) ; op( MPZ(z), MPZ(x), y ) ; GB_GCSafe_Leave ; }
#define GB_Integer_Op2_In2(op,z1,z2,x,y)	{ GB_GCSafe_Enter ; GB_GCSafe_2(x,y) ; GB_NodeAlloc_Mpz_In(z1) ; GB_NodeAlloc_Mpz_In(z2) ; op( MPZ(z1), MPZ(z2), MPZ(x), MPZ(y) ) ; GB_GCSafe_Leave ;  }
#define GB_Integer_Add_In(z,x,y)			GB_Integer_Op2_In1(mpz_add,z,x,y)
#define GB_Integer_Sub_In(z,x,y)			GB_Integer_Op2_In1(mpz_sub,z,x,y)
#define GB_Integer_Mul_In(z,x,y)			GB_Integer_Op2_In1(mpz_mul,z,x,y)
#define GB_Integer_Div_In(z,x,y)			GB_Integer_Op2_In1(mpz_fdiv_q,z,x,y)
#define GB_Integer_Mod_In(z,x,y)			GB_Integer_Op2_In1(mpz_fdiv_r,z,x,y)
#define GB_Integer_DivMod_In(z1,z2,x,y)		GB_Integer_Op2_In2(mpz_fdiv_qr,z1,z2,x,y)
#define GB_Integer_Quot_In(z,x,y)			GB_Integer_Op2_In1(mpz_tdiv_q,z,x,y)
#define GB_Integer_Rem_In(z,x,y)			GB_Integer_Op2_In1(mpz_tdiv_r,z,x,y)
#define GB_Integer_QuotRem_In(z1,z2,x,y)	GB_Integer_Op2_In2(mpz_tdiv_qr,z1,z2,x,y)
#define GB_Integer_Neg_In(z,x)				GB_Integer_Op1_In1(mpz_neg,z,x)

#define GB_Integer_Cmp(x,y)					mpz_cmp(MPZ(x), MPZ(y))

#endif
%%]

%%[97
#if USE_GMP
#define GB_Integer_And_In(z,x,y)			GB_Integer_Op2_In1(mpz_and,z,x,y)
#define GB_Integer_Or_In(z,x,y)				GB_Integer_Op2_In1(mpz_ior,z,x,y)
#define GB_Integer_Xor_In(z,x,y)			GB_Integer_Op2_In1(mpz_eor,z,x,y)
#define GB_Integer_Complement_In(z,x)		GB_Integer_Op1_In1(mpz_com,z,x)
#define GB_Integer_ShiftLeft_In(z,x,y)		GB_Integer_Op2b_In1(mpz_mul_2exp,z,x,y)
#define GB_Integer_ShiftRight_In(z,x,y)		GB_Integer_Op2b_In1(mpz_fdiv_q_2exp,z,x,y)
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
PRIM Float primRationalToFloat( GB_NodePtr nr )
{
	// GB_NodePtr nf ;
	gb_assert_IsNotFreshMem( (Word)nr, "primRationalToFloat" ) ;
	GB_NodePtr numerator, divisor ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(nr) ;
	GB_GCSafe_2_Zeroed(numerator, divisor) ;
	GB_PassExc_Cast_GCSafe( Word, numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ) ;
	GB_PassExc_Cast_GCSafe( Word, divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ) ;
	Float res ;
	res = Cast( Float, mpz_get_d( MPZ(numerator) ) / mpz_get_d( MPZ(divisor) ) ) ;
	GB_GCSafe_Leave ;
	return res ;
}

PRIM Double primRationalToDouble( GB_NodePtr nr )
{
	// GB_NodePtr nf ;
	gb_assert_IsNotFreshMem( (Word)nr, "primRationalToDouble" ) ;
	GB_NodePtr numerator, divisor ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(nr) ;
	GB_GCSafe_2_Zeroed(numerator, divisor) ;
	GB_PassExc_Dflt_GCSafe( 0.0, numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ) ;
	GB_PassExc_Dflt_GCSafe( 0.0, divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ) ;
	// GB_NodeAlloc_Double_In(nf) ;
	// nf->content.dbl = mpz_get_d( MPZ(numerator) ) / mpz_get_d( MPZ(divisor) ) ;
	// return nf ;
	GB_GCSafe_Leave ;
	return Cast( Double, mpz_get_d( MPZ(numerator) ) / mpz_get_d( MPZ(divisor) ) ) ;
}

PRIM Float primIntegerToFloat( GB_NodePtr n )
{
	// GB_NodePtr nf ;
	// GB_NodeAlloc_Float_In(nf) ;
	// nf->content.flt = mpz_get_d( MPZ(n) ) ;		// not sure whether this works without explicit truncation or something like that...
	// return nf ;
	gb_assert_IsNotFreshMem( (Word)n, "primIntegerToFloat" ) ;
	return Cast( Float, mpz_get_d( MPZ(n) ) ) ;
}

PRIM Double primIntegerToDouble( GB_NodePtr n )
{
	// GB_NodePtr nf ;
	// GB_NodeAlloc_Double_In(nf) ;
	// nf->content.dbl = mpz_get_d( MPZ(n) ) ;
	// return nf ;
	gb_assert_IsNotFreshMem( (Word)n, "primIntegerToDouble" ) ;
	return Cast( Double, mpz_get_d( MPZ(n) ) ) ;
}

PRIM Word primIntegerToInt( GB_NodePtr n )
{
	// return GB_Int2GBInt( mpz_get_si( MPZ(n) ) ) ;
	gb_assert_IsNotFreshMem( (Word)n, "primIntegerToInt" ) ;
	return ( mpz_get_si( MPZ(n) ) ) ;
}

PRIM GB_NodePtr primCStringToInteger( char* s )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_set_str( MPZ(n), s, 10 ) ;
	return n ;
}

PRIM GB_NodePtr primIntToInteger( GB_Int x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetSignedInt_In( n, x ) ;
	return n ;
}

PRIM GB_NodePtr primFloatToInteger( Float x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetDbl_In( n, x ) ;
	return n ;
}

PRIM GB_NodePtr primDoubleToInteger( Double x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetDbl_In( n, x ) ;
	return n ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer, via GMP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!!!!! Todo: adapt allocation for GMP, GC works improperly here because can create race conditions.

%%[97
#if USE_GMP
PRIM Word primEqInteger( GB_NodePtr x, GB_NodePtr y )
{
	gb_assert_IsNotFreshMem( (Word)x, "primEqInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primEqInteger y" ) ;
	if ( GB_Integer_Cmp(x,y) == 0 )
		return gb_True ;
  	return gb_False ;
}

PRIM Word primCmpInteger( GB_NodePtr x, GB_NodePtr y )
{
	gb_assert_IsNotFreshMem( (Word)x, "primCmpInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primCmpInteger y" ) ;
	int c = GB_Integer_Cmp(x,y) ;
	if ( c < 0 )
		return gb_LT ;
	else if ( c == 0 )
		return gb_EQ ;
  	return gb_GT ;
}

PRIM GB_NodePtr primAddInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primAddInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primAddInteger y" ) ;
	GB_Integer_Add_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primSubInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primSubInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primSubInteger y" ) ;
	GB_Integer_Sub_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primMulInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primMulInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primMulInteger y" ) ;
	GB_Integer_Mul_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primDivInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primDivInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primDivInteger y" ) ;
	GB_Integer_Div_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primModInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primModInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primModInteger y" ) ;
	GB_Integer_Mod_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primQuotInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primQuotInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primQuotInteger y" ) ;
	GB_Integer_Quot_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primRemInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primRemInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primRemInteger y" ) ;
	GB_Integer_Rem_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primQuotRemInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n1, n2 ;
	gb_assert_IsNotFreshMem( (Word)x, "primQuotRemInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primQuotRemInteger y" ) ;
	GB_GCSafe_Enter ;
	GB_GCSafe_2(x,y) ;
	GB_GCSafe_3_Zeroed(n,n1,n2) ;
	GB_Integer_QuotRem_In(n1,n2,x,y) ;
	GB_MkTupNode2_In(n,n1,n2) ;
	GB_GCSafe_Leave ;
	return n ;
}

PRIM GB_NodePtr primDivModInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n1, n2 ;
	gb_assert_IsNotFreshMem( (Word)x, "primDivModInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primDivModInteger y" ) ;
	GB_GCSafe_Enter ;
	GB_GCSafe_2(x,y) ;
	GB_GCSafe_3_Zeroed(n,n1,n2) ;
	GB_Integer_DivMod_In(n1,n2,x,y) ;
	GB_MkTupNode2_In(n,n1,n2) ;
	GB_GCSafe_Leave ;
	return n ;
}
#endif
%%]

%%[97
#if USE_GMP
PRIM GB_NodePtr primNegInteger( GB_NodePtr x )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primNegInteger x" ) ;
	GB_Integer_Neg_In(n,x) ;
	return n ;
}
#endif
%%]

%%[99
PRIM GB_NodePtr primAndInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primAndInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primAndInteger y" ) ;
	GB_Integer_And_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primOrInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primOrInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primOrInteger y" ) ;
	GB_Integer_Or_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primXorInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primXorInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primXorInteger y" ) ;
	GB_Integer_Xor_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primComplementInteger( GB_NodePtr x )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primComplementInteger x" ) ;
	GB_Integer_Complement_In(n,x) ;
	return n ;
}

PRIM GB_NodePtr primShiftLeftInteger( GB_NodePtr x, Word y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primShiftLeftInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primShiftLeftInteger y" ) ;
	GB_Integer_ShiftLeft_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr primShiftRightInteger( GB_NodePtr x, Word y )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)x, "primShiftRightInteger x" ) ;
	gb_assert_IsNotFreshMem( (Word)y, "primShiftRightInteger y" ) ;
	GB_Integer_ShiftRight_In(n,x,y) ;	// with sign extend
	return n ;
}

PRIM GB_NodePtr primRotateLeftInteger( GB_NodePtr x, Word y )
{
	return primShiftLeftInteger( x, y ) ;
}

PRIM GB_NodePtr primRotateRightInteger( GB_NodePtr x, Word y )
{
	return primShiftRightInteger( x, y ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int8, Int16, Int32, Int64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#ifdef USE_32_BITS
PRIM GB_NodePtr primInt32ToInteger( Int32 x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetSignedInt_In( n, x ) ;
	return n ;
}

PRIM Int32 primIntegerToInt32( GB_NodePtr n )
{
	Int32 x = mpz_get_si( MPZ(n) ) ;
	return ( x ) ;
}
#endif
%%]

%%[97
PRIM GB_NodePtr primInt64ToInteger( Int64 x )
{
	GB_NodePtr n ;
	Int64 xpos = ( x < 0 ? -x : x ) ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_import( MPZ(n), 1, -1, 8, 0, 0, &xpos ) ;
	if ( x < 0 ) {
		mpz_neg( MPZ(n), MPZ(n) ) ;
	}
	return n ;
}

PRIM Int64 primIntegerToInt64( GB_NodePtr n )
{
	Int64 x ;
	gb_assert_IsNotFreshMem( (Word)n, "primIntegerToInt64" ) ;
	if ( sizeof(Int64) <= sizeof(unsigned long int) ) {
		x = mpz_get_si( MPZ(n) ) ;
	} else { // sizeof(Int32) == sizeof(unsigned long int)
		mpz_t mpz ;
		mpz_init_set( mpz, MPZ(n) ) ;
		int sign = mpz_sgn( mpz ) ;
		if ( sign < 0 ) {
			mpz_neg( mpz, mpz ) ;
		}
		Word64 i32a = mpz_get_ui( mpz ) ;
		mpz_fdiv_q_2exp( mpz, mpz, Word32_SizeInBits ) ;
		Word64 i32b = mpz_get_ui( mpz ) ;
		x = i32b << Word32_SizeInBits | i32a ;
		if ( sign < 0 ) {
			x = -x ;
		}
	}
	return ( x ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM Word primIntegerToWord( GB_NodePtr n )
{
	gb_assert_IsNotFreshMem( (Word)n, "primIntegerToWord" ) ;
	return ( mpz_get_ui( MPZ(n) ) ) ;
}

PRIM GB_NodePtr primWordToInteger( Word x )
{
	// printf( "primWordToInteger %x\n", x ) ;
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetUnsignedInt_In( n, x ) ;
	return n ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word8, Word16, Word32, Word64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#ifdef USE_32_BITS
PRIM GB_NodePtr primWord32ToInteger( Word32 x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetUnsignedInt_In( n, x ) ;
	return n ;
}

PRIM Word32 primIntegerToWord32( GB_NodePtr n )
{
	gb_assert_IsNotFreshMem( (Word)n, "primIntegerToWord32" ) ;
	Word32 x = mpz_get_ui( MPZ(n) ) ;
	return ( x ) ;
}
#endif
%%]

%%[97
PRIM GB_NodePtr primWord64ToInteger( Word64 x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_import( MPZ(n), 1, -1, 8, 0, 0, &x ) ;
	return n ;
}

PRIM Word64 primIntegerToWord64( GB_NodePtr n )
{
	Word64 x ;
	gb_assert_IsNotFreshMem( (Word)n, "primIntegerToWord64" ) ;
	if ( sizeof(Word64) <= sizeof(unsigned long int) ) {
		x = mpz_get_ui( MPZ(n) ) ;
	} else { // sizeof(Word32) == sizeof(unsigned long int)
		mpz_t mpz ;
		mpz_init_set( mpz, MPZ(n) ) ;
		Word64 i32a = mpz_get_ui( mpz ) ;
		mpz_fdiv_q_2exp( mpz, mpz, Word32_SizeInBits ) ;
		Word64 i32b = mpz_get_ui( mpz ) ;
		x = i32b << Word32_SizeInBits | i32a ;
	}
	return ( x ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
PRIM GB_NodePtr primShowInteger( GB_NodePtr integerNd )
{
	GB_NodePtr n ;
	gb_assert_IsNotFreshMem( (Word)integerNd, "primShowInteger" ) ;
	int sz = mpz_sizeinbase( MPZ(integerNd), 10 ) + 2 ;
	char* buf = alloca( sz ) ;

	mpz_get_str( buf, 10, MPZ(integerNd) ) ;
	sz = strlen(buf) ;
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;

  	return primByteArrayToString1Char( n, GB_Int0 ) ;
}
#endif
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Double/Float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#define gb_intlDecode(ty,x)																	\
{																							\
	int exp ;																				\
	int mantdig = ( sizeof(ty) == sizeof(Double) ? DBL_MANT_DIG : FLT_MANT_DIG ) ;			\
	ty mant = ( sizeof(ty) == sizeof(Double) ? frexp( x, &exp ) : frexpf( x, &exp) ) ;		\
	if ( fpclassify(x) == FP_ZERO ) {														\
		exp = 0 ;																			\
	} else {																				\
		exp -= mantdig ;																	\
	}																						\
	GB_NodePtr n, ni ;																		\
	GB_GCSafe_Enter ;																		\
	GB_GCSafe_2_Zeroed(n,ni) ;																\
	GB_NodeAlloc_Mpz_SetDbl_In( ni, ldexp( mant, mantdig ) ) ;								\
	GB_MkTupNode2_In(n,ni,GB_Int2GBInt(exp)) ;												\
	GB_GCSafe_Leave ;																		\
	return n ;																				\
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM Float primEncodeFloat( GB_NodePtr frac, Word exp )
{
	gb_assert_IsNotFreshMem( (Word)frac, "primEncodeFloat" ) ;
	Float d = ldexp( mpz_get_d( MPZ(frac) ), exp ) ;
	return d ;
}

PRIM GB_NodePtr primDecodeFloat( Float x )
	gb_intlDecode(Float,x)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Double
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM Double primEncodeDouble( GB_NodePtr frac, Word exp )
{
	gb_assert_IsNotFreshMem( (Word)frac, "primEncodeDouble" ) ;
	Double d = ldexp( mpz_get_d( MPZ(frac) ), exp ) ;
	return d ;
}

PRIM GB_NodePtr primDecodeDouble( Double x )
	gb_intlDecode(Double,x)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
void prim_integer_Initialize()
{
#	if USE_GMP
		mp_set_memory_functions( gb_Alloc_GMP, gb_ReAlloc_GMP, gb_Free_GMP ) ;
		// IF_GB_TR_ON(3,{printf("mp_set_memory_functions\n");}) ;
#	endif
}
%%]


