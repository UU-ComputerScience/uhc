%%[8
#include "../rts.h"
%%]

%%[97
#if USE_GMP
#include "gmp.h"
#endif
%%]

%%[97
#include <alloca.h>
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives for grin bytecode interpreter, those related to Integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GMP specific definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
#define GB_NodeMpzSize						(EntierUpDivBy(sizeof(mpz_t),sizeof(GB_Word)) + 1)
#define GB_NodeGMPSize(nBytes)				(EntierUpDivBy(nBytes,sizeof(GB_Word)) + 1)

#define GB_MkMpzHeader						GB_MkHeader(GB_NodeMpzSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_GMP_mpz)
#define GB_MkGMPHeader(sz)					GB_MkHeader(sz, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_GMP_intl)
#endif
%%]

%%[97
#if USE_GMP
#define GB_NodeAlloc_Mpz_In(n)				{ GB_NodeAlloc_Hdr_In(GB_NodeMpzSize,GB_MkMpzHeader,n) ; mpz_init(MPZ(n)) ; }
#define GB_NodeAlloc_GMP_In(nBytes,n)		{ int sz = GB_NodeGMPSize(nBytes) ; GB_NodeAlloc_Hdr_In(sz,GB_MkGMPHeader(sz),n) ; }
#endif
%%]

Access

%%[97
#define MPZ(n)								(__mpz_struct*)((n)->content.fields)
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

mpz_t is defined as an array, this makes casting problematic (compiler complains),
so a pointer based definition is used. However, __mpz_struct is an internal type which may
undergo name changes as GMP versions progress.

%%[97
#if USE_GMP
typedef __mpz_struct*  GB_mpz ;

#define GB_Integer_Op1_In1(op,z,x)			{ GB_NodeAlloc_Mpz_In(z) ; op( MPZ(z), MPZ(x) ) ; }
#define GB_Integer_Op2_In1(op,z,x,y)		{ GB_NodeAlloc_Mpz_In(z) ; op( MPZ(z), MPZ(x), MPZ(y) ) ; }
#define GB_Integer_Op2b_In1(op,z,x,y)		{ GB_NodeAlloc_Mpz_In(z) ; op( MPZ(z), MPZ(x), y ) ; }
#define GB_Integer_Op2_In2(op,z1,z2,x,y)	{ GB_NodeAlloc_Mpz_In(z1) ; GB_NodeAlloc_Mpz_In(z2) ; op( MPZ(z1), MPZ(z2), MPZ(x), MPZ(y) ) ; }
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
%%% GMP memory allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
void* gb_Alloc_GMP( size_t nBytes )
{
  GB_Node* n ;
  GB_NodeAlloc_GMP_In(nBytes,n) ;
  return n->content.fields ;		/* return ptr to usable area */
}

void* gb_ReAlloc_GMP( void *n, size_t nBytesOld, size_t nBytes )
{
  if ( nBytes > nBytesOld )
  {
	  GB_Node* nNew ;
	  GB_NodeAlloc_GMP_In(nBytes,nNew) ;
	  memcpy( nNew->content.fields, n, nBytesOld ) ;
	  return nNew->content.fields ;
  }
  return n ;
}

void gb_Free_GMP( void *n, size_t nBytesOld )
{
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
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
	res = Cast( GB_Float, mpz_get_d( MPZ(numerator) ) / mpz_get_d( MPZ(divisor) ) ) ;
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
	// nf->content.dbl = mpz_get_d( MPZ(numerator) ) / mpz_get_d( MPZ(divisor) ) ;
	// return nf ;
	GB_GC_SafeLeave ;
	return Cast( GB_Double, mpz_get_d( MPZ(numerator) ) / mpz_get_d( MPZ(divisor) ) ) ;
}

PRIM GB_Float gb_primIntegerToFloat( GB_NodePtr n )
{
	// GB_NodePtr nf ;
	// GB_NodeAlloc_Float_In(nf) ;
	// nf->content.flt = mpz_get_d( MPZ(n) ) ;		// not sure whether this works without explicit truncation or something like that...
	// return nf ;
	return Cast( GB_Float, mpz_get_d( MPZ(n) ) ) ;
}

PRIM GB_Double gb_primIntegerToDouble( GB_NodePtr n )
{
	// GB_NodePtr nf ;
	// GB_NodeAlloc_Double_In(nf) ;
	// nf->content.dbl = mpz_get_d( MPZ(n) ) ;
	// return nf ;
	return Cast( GB_Double, mpz_get_d( MPZ(n) ) ) ;
}

PRIM GB_Word gb_primIntegerToInt( GB_NodePtr n )
{
	// return GB_Int2GBInt( mpz_get_si( MPZ(n) ) ) ;
	return ( mpz_get_si( MPZ(n) ) ) ;
}

PRIM GB_NodePtr gb_primCStringToInteger( char* s )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_set_str( MPZ(n), s, 10 ) ;
	return n ;
}

PRIM GB_NodePtr gb_primIntToInteger( GB_Int x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetSignedInt_In( n, x ) ;
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

%%[99
PRIM GB_NodePtr gb_primAndInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_And_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primOrInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Or_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primXorInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Integer_Xor_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primComplementInteger( GB_NodePtr x )
{
	GB_NodePtr n ;
	GB_Integer_Complement_In(n,x) ;
	return n ;
}

PRIM GB_NodePtr gb_primShiftLeftInteger( GB_NodePtr x, GB_Word y )
{
	GB_NodePtr n ;
	GB_Integer_ShiftLeft_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primShiftRightInteger( GB_NodePtr x, GB_Word y )
{
	GB_NodePtr n ;
	GB_Integer_ShiftRight_In(n,x,y) ;	// with sign extend
	return n ;
}

PRIM GB_NodePtr gb_primRotateLeftInteger( GB_NodePtr x, GB_Word y )
{
	return gb_primShiftLeftInteger( x, y ) ;
}

PRIM GB_NodePtr gb_primRotateRightInteger( GB_NodePtr x, GB_Word y )
{
	return gb_primShiftRightInteger( x, y ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int8, Int16, Int32, Int64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#ifdef USE_32_BITS
INTLIKE_ARITH_PRIMS_CODE(gb_,Int32,Int32,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)

PRIM GB_NodePtr gb_primInt32ToInteger( Int32 x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetSignedInt_In( n, x ) ;
	return n ;
}

PRIM Int32 gb_primIntegerToInt32( GB_NodePtr n )
{
	Int32 x = mpz_get_si( MPZ(n) ) ;
	return ( x ) ;
}
#endif
%%]

%%[97
PRIM GB_NodePtr gb_primInt64ToInteger( Int64 x )
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

PRIM Int64 gb_primIntegerToInt64( GB_NodePtr n )
{
	Int64 x ;
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
PRIM Word gb_primIntegerToWord( GB_NodePtr n )
{
	return ( mpz_get_ui( MPZ(n) ) ) ;
}

PRIM GB_NodePtr gb_primWordToInteger( Word x )
{
	// printf( "gb_primWordToInteger %x\n", x ) ;
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
INTLIKE_ARITH_PRIMS_CODE(gb_,Word32,Word32,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)

PRIM GB_NodePtr gb_primWord32ToInteger( Word32 x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_SetUnsignedInt_In( n, x ) ;
	return n ;
}

PRIM Word32 gb_primIntegerToWord32( GB_NodePtr n )
{
	Word32 x = mpz_get_ui( MPZ(n) ) ;
	return ( x ) ;
}
#else
#endif
%%]

%%[97
PRIM GB_NodePtr gb_primWord64ToInteger( Word64 x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_import( MPZ(n), 1, -1, 8, 0, 0, &x ) ;
	return n ;
}

PRIM Word64 gb_primIntegerToWord64( GB_NodePtr n )
{
	Word64 x ;
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
PRIM GB_NodePtr gb_primShowInteger( GB_NodePtr integerNd )
{
	GB_NodePtr n ;
	int sz = mpz_sizeinbase( MPZ(integerNd), 10 ) + 2 ;
	char* buf = alloca( sz ) ;

	mpz_get_str( buf, 10, MPZ(integerNd) ) ;
	sz = strlen(buf) ;
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;

  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
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
PRIM GB_Float gb_primEncodeFloat( GB_NodePtr frac, GB_Word exp )
{
	GB_Float d = ldexp( mpz_get_d( MPZ(frac) ), exp ) ;
	return d ;
}

PRIM GB_NodePtr gb_primDecodeFloat( GB_Float x )
	gb_intlDecode(GB_Float,x)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Double
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Double gb_primEncodeDouble( GB_NodePtr frac, GB_Word exp )
{
	GB_Double d = ldexp( mpz_get_d( MPZ(frac) ), exp ) ;
	return d ;
}

PRIM GB_NodePtr gb_primDecodeDouble( GB_Double x )
	gb_intlDecode(GB_Double,x)

%%]


