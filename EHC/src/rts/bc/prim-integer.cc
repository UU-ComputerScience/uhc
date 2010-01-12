%%[8
#include "../rts.h"
#include "interpreter.h"
%%]

%%[97
#if USE_GMP
#include "gmp.h"
#endif
#if USE_LTM
#include "../ltm/tommath.h"
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
%%% Debugging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_LTM
#define prShowIntegerIn(n,buf)		 	int sz ;										\
										mp_radix_size_estim( n, 10, &sz ) ;				\
										buf = alloca( sz + 10 ) ;						\
										mp_toradix( n, buf, 10 ) ;						
#endif
%%]

%%[97
#if TRACE
void prLTM(GB_NodePtr x,char* msg) {
	gb_prWord((Word)(x)) ;
	char* buf ;
	prShowIntegerIn( x, buf ) ;
	printf("\n%s: alc=%x used=%x sign=%d digs=%p[%x,%x]: %s\n",msg,GB_LTM_Int_Alloc(x),GB_LTM_Int_Used(x),GB_LTM_Int_Sign(x),GB_LTM_Int_Digits(x),(GB_LTM_Int_Digits(x))[0],(GB_LTM_Int_Digits(x))[1],buf) ; 
}
#else
#define prLTM(x,msg)
#endif
%%]

%%[97
#if TRACE
static void gb_assert_IsOkInteger( GB_NodePtr x, char* msg ) {
#	if USE_LTM
		if ( GB_LTM_Int_Used(x) > GB_LTM_Int_Alloc(x) ) {
			prLTM( x, msg ) ;
			rts_panic1_1( "gb_assert_IsOkInteger", (Word)x ) ;
		}
#	endif
}
#else
#define gb_assert_IsOkInteger(n,m)
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GMP allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
#define GB_NodeGMPMpzSize					(EntierUpDivBy(sizeof(mpz_t),sizeof(Word)) + 1)
#define GB_MkGMPMpzHeader					GB_MkHeader(GB_NodeGMPMpzSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_GMP_mpz)

#if ! USE_EHC_MM
#define GB_NodeGMPSize(nBytes)				(EntierUpDivBy(nBytes,sizeof(Word)) + 1)
#define GB_MkGMPHeader(sz)					GB_MkHeader(sz, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_GMP_intl)
#endif

#define GB_AllocEnsure_GMPMpz(nrMpz)		GB_AllocEnsure_Words_Finalizer(nrMpz*GB_NodeGMPMpzSize, nrMpz)
#define GB_NodeAlloc_GMPMpz_In(n)			{ GB_AllocEnsure_GMPMpz(1) ; GB_NodeAlloc_Hdr_In(GB_NodeGMPMpzSize,GB_MkGMPMpzHeader,n) ; mpz_init(MPZ(n)) ; GB_Register_Finalizer(n,0) ; }
#define GB_NodeAlloc_GMPMpz_In_Ensured(n)	{ GB_NodeAlloc_Hdr_In(GB_NodeGMPMpzSize,GB_MkGMPMpzHeader,n) ; mpz_init(MPZ(n)) ; GB_Register_FinalizerEnsured(n,0) ; }

#if ! USE_EHC_MM
#define GB_NodeAlloc_GMP_In(nBytes,n)		{ int sz = GB_NodeGMPSize(nBytes) ; GB_NodeAlloc_Hdr_In(sz,GB_MkGMPHeader(sz),n) ; }
#endif
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LTM allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_LTM
#define GB_AllocEnsure_LTM1Mpz(d1,sz1,nr,extra)		sz1 = GB_NodeLTMMpzSize(d1) ; GB_AllocEnsure_Words(((nr)*(sz1))+(extra))
#define GB_AllocEnsure_LTM2Mpz(d1,sz1,d2,sz2,extra)	sz1 = GB_NodeLTMMpzSize(d1) ; sz2 = GB_NodeLTMMpzSize(d2) ; GB_AllocEnsure_Words((sz1)+(sz2)+(extra))

#define GB_NodeAlloc_LTMMpzDigs_In(nDigits,n)		{ Word _sz = GB_NodeLTMMpzSize(nDigits); \
													  GB_NodeAlloc_Hdr_In(_sz,GB_MkLTMMpzHeader(_sz),n) ; \
													  GB_Node_ZeroFields(n) ; \
													}
#define GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,n)	{ GB_NodeAlloc_Hdr_In_Ensured(sz,GB_MkLTMMpzHeader(sz),n) ; \
													  GB_Node_ZeroFields(n) ; \
													}

#endif
%%]
													  printf("GB_NodeAlloc_LTMMpzDigs_In dig=%x sz=%x p=%p\n",nDigits,_sz,n);prLTM(n,"GB_NodeAlloc_LTMMpzDigs_In") ; \

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GMP utils
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
#define GB_NodeAlloc_GMPMpz_SetSignedInt_In_Ensured(n,x)		{ GB_NodeAlloc_GMPMpz_In_Ensured(n) ; mpz_set_si( MPZ(n), (x) ) ; }
#define GB_NodeAlloc_GMPMpz_SetUnsignedInt_In_Ensured(n,x)		{ GB_NodeAlloc_GMPMpz_In_Ensured(n) ; mpz_set_ui( MPZ(n), (x) ) ; }
#define GB_NodeAlloc_GMPMpz_SetDbl_In(n,x)						{ GB_NodeAlloc_GMPMpz_In(n) ; mpz_set_d( MPZ(n), (x) ) ; }
#define GB_NodeAlloc_GMPMpz_SetDbl_In_Ensured(n,x)				{ GB_NodeAlloc_GMPMpz_In_Ensured(n) ; mpz_set_d( MPZ(n), (x) ) ; }
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LTM utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_LTM 
#	if TRACE
#		define GB_LTM_DoChecked(action)				{ int _err = action ; if (_err != MP_OKAY) {printf("MP err %s (DIGIT_BIT=%d)\n",mp_error_to_string(_err),DIGIT_BIT);} ; }
#	else
#		define GB_LTM_DoChecked(action)				action
#	endif
#endif
%%]

%%[97
#if USE_LTM

#define GB_NodeAlloc_LTMMpz_SetSignedInt_In(n,x)				{ GB_NodeAlloc_LTMMpzDigs_In(GB_LTM_Int_NrDigitsOfBytes(sizeof(Word64)),n) ; GB_LTM_DoChecked(mp_set_sint64  ( n, (x) )) ; }
#define GB_NodeAlloc_LTMMpz_SetUnsignedInt_In(n,x)				{ GB_NodeAlloc_LTMMpzDigs_In(GB_LTM_Int_NrDigitsOfBytes(sizeof(Word64)),n) ; GB_LTM_DoChecked(mp_set_uint64  ( n, (x) )) ; }
#define GB_NodeAlloc_LTMMpz_SetDbl64_In(n,x)					{ GB_NodeAlloc_LTMMpzDigs_In(GB_LTM_Int_NrDigitsOfBytes(sizeof(Word64)),n) ; GB_LTM_DoChecked(mp_set_double64( n, (x) )) ; }
#define GB_NodeAlloc_LTMMpz_SetDbl_In(n,exp,x)					{ GB_NodeAlloc_LTMMpzDigs_In(GB_LTM_Int_NrDigitsOfDoubleExp(exp)       ,n) ; GB_LTM_DoChecked(mp_set_double  ( n, (x) )) ; }
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer via GMP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
#define GB_GMPInteger_Op1_In1(op,z,x)					{ GB_GCSafe_Enter ; GB_GCSafe_1(x) ; GB_AllocEnsure_GMPMpz(1) ; GB_NodeAlloc_GMPMpz_In_Ensured(z) ; op( MPZ(z), MPZ(x) ) ; GB_GCSafe_Leave ; }
#define GB_GMPInteger_Op2_In1(op,z,x,y)					{ GB_GCSafe_Enter ; GB_GCSafe_2(x,y) ; GB_AllocEnsure_GMPMpz(1) ; GB_NodeAlloc_GMPMpz_In_Ensured(z) ; op( MPZ(z), MPZ(x), MPZ(y) ) ; GB_GCSafe_Leave ; }
#define GB_GMPInteger_Op2b_In1(op,z,x,y)				{ GB_GCSafe_Enter ; GB_GCSafe_1(x) ; GB_AllocEnsure_GMPMpz(1) ; GB_NodeAlloc_GMPMpz_In_Ensured(z) ; op( MPZ(z), MPZ(x), y ) ; GB_GCSafe_Leave ; }
#define GB_GMPInteger_Op2_In2(op,z1,z2,x,y)				{ GB_GCSafe_Enter ; GB_GCSafe_2(x,y) ; GB_AllocEnsure_GMPMpz(2) ; GB_NodeAlloc_GMPMpz_In_Ensured(z1) ; GB_NodeAlloc_GMPMpz_In_Ensured(z2) ; op( MPZ(z1), MPZ(z2), MPZ(x), MPZ(y) ) ; GB_GCSafe_Leave ;  }
#define GB_GMPInteger_Op2_In2_Ensured(op,z1,z2,x,y)		{ GB_NodeAlloc_GMPMpz_In_Ensured(z1) ; GB_NodeAlloc_GMPMpz_In_Ensured(z2) ; op( MPZ(z1), MPZ(z2), MPZ(x), MPZ(y) ) ; }
#define GB_GMPInteger_Add_In(z,x,y)						GB_GMPInteger_Op2_In1(mpz_add,z,x,y)
#define GB_GMPInteger_Sub_In(z,x,y)						GB_GMPInteger_Op2_In1(mpz_sub,z,x,y)
#define GB_GMPInteger_Mul_In(z,x,y)						GB_GMPInteger_Op2_In1(mpz_mul,z,x,y)
#define GB_GMPInteger_Div_In(z,x,y)						GB_GMPInteger_Op2_In1(mpz_fdiv_q,z,x,y)
#define GB_GMPInteger_Mod_In(z,x,y)						GB_GMPInteger_Op2_In1(mpz_fdiv_r,z,x,y)
#define GB_GMPInteger_DivMod_In(z1,z2,x,y)				GB_GMPInteger_Op2_In2(mpz_fdiv_qr,z1,z2,x,y)
#define GB_GMPInteger_DivMod_In_Ensured(z1,z2,x,y)		GB_GMPInteger_Op2_In2_Ensured(mpz_fdiv_qr,z1,z2,x,y)
#define GB_GMPInteger_Quot_In(z,x,y)					GB_GMPInteger_Op2_In1(mpz_tdiv_q,z,x,y)
#define GB_GMPInteger_Rem_In(z,x,y)						GB_GMPInteger_Op2_In1(mpz_tdiv_r,z,x,y)
#define GB_GMPInteger_QuotRem_In(z1,z2,x,y)				GB_GMPInteger_Op2_In2(mpz_tdiv_qr,z1,z2,x,y)
#define GB_GMPInteger_QuotRem_In_Ensured(z1,z2,x,y)		GB_GMPInteger_Op2_In2_Ensured(mpz_tdiv_qr,z1,z2,x,y)
#define GB_GMPInteger_Neg_In(z,x)						GB_GMPInteger_Op1_In1(mpz_neg,z,x)

#define GB_GMPInteger_Cmp(x,y)							mpz_cmp(MPZ(x), MPZ(y))

#endif
%%]

%%[97
#if USE_GMP
#define GB_GMPInteger_And_In(z,x,y)				GB_GMPInteger_Op2_In1(mpz_and,z,x,y)
#define GB_GMPInteger_Or_In(z,x,y)				GB_GMPInteger_Op2_In1(mpz_ior,z,x,y)
#define GB_GMPInteger_Xor_In(z,x,y)				GB_GMPInteger_Op2_In1(mpz_eor,z,x,y)
#define GB_GMPInteger_Complement_In(z,x)		GB_GMPInteger_Op1_In1(mpz_com,z,x)
#define GB_GMPInteger_ShiftLeft_In(z,x,y)		GB_GMPInteger_Op2b_In1(mpz_mul_2exp,z,x,y)
#define GB_GMPInteger_ShiftRight_In(z,x,y)		GB_GMPInteger_Op2b_In1(mpz_fdiv_q_2exp,z,x,y)
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer via LTM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_LTM

#if 1
#define GB_LTMInteger_Op1_In1(ndigs,op,z,x)				{ GB_GCSafe_Enter ; GB_GCSafe_1(x)   ; Word sz ; GB_AllocEnsure_LTM1Mpz(ndigs,sz,1,0) ; GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,z)  ; GB_LTM_DoChecked(op( x, z )) ; GB_GCSafe_Leave ; }
#define GB_LTMInteger_Op2_In1(ndigs,op,z,x,y)			{ GB_GCSafe_Enter ; GB_GCSafe_2(x,y) ; Word sz ; GB_AllocEnsure_LTM1Mpz(ndigs,sz,1,0) ; GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,z)  ; GB_LTM_DoChecked(op( x, y, z )) ; GB_GCSafe_Leave ; }
#define GB_LTMInteger_Op2b_In1(ndigs,op,z,x,y)			{ GB_GCSafe_Enter ; GB_GCSafe_1(x)   ; Word sz ; GB_AllocEnsure_LTM1Mpz(ndigs,sz,1,0) ; GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,z)  ; GB_LTM_DoChecked(op( x, y, z )) ; GB_GCSafe_Leave ; }
#define GB_LTMInteger_Op2_In2_Extra(ndigs,extra,op,z1,z2,x,y) \
														{ GB_GCSafe_Enter ; GB_GCSafe_2(x,y) ; Word sz ; GB_AllocEnsure_LTM1Mpz(ndigs,sz,2,extra) ; GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,z1) ; GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,z2) ; GB_LTM_DoChecked(op( x, y, z1, z2 )) ; GB_GCSafe_Leave ;  }
#define GB_LTMInteger_Op2_In2(ndigs,op,z1,z2,x,y)		GB_LTMInteger_Op2_In2_Extra(ndigs,0,op,z1,z2,x,y)
#define GB_LTMInteger_Op2b_In2(ndigs,op,z1,z2,x,y)		{ GB_GCSafe_Enter ; GB_GCSafe_1(x)   ; Word sz ; GB_AllocEnsure_LTM1Mpz(ndigs,sz,2,0) ; GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,z1) ; GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,z2) ; GB_LTM_DoChecked(op( x, y, z1, z2 )) ; GB_GCSafe_Leave ;  }
#define GB_LTMInteger_Op2b_In1of2(ndigs,op,z1,x,y)		{ GB_GCSafe_Enter ; GB_GCSafe_1(x)   ; 													\
														  Word sz ; GB_AllocEnsure_LTM1Mpz(ndigs,sz,1,0) ; 										\
														  GB_NodeAlloc_LTMMpzSize_In_Ensured(sz,z1) ; 											\
														  GB_NodePtr _z2 ;																		\
												  		  GB_NodeAlloc_LTMMpzDigs_In_Alloca(ndigs,_z2) ; 										\
												  		  op( x, y, z1, _z2 ) ; 																\
												  		  GB_GCSafe_Leave ; 																	\
														}
#else
#define GB_LTMInteger_Op1_In1(ndigs,op,z,x)				{ GB_GCSafe_Enter ; GB_GCSafe_1(x)   ; GB_NodeAlloc_LTMMpzDigs_In(ndigs,z)  ; GB_LTM_DoChecked(op( x, z )) ; GB_GCSafe_Leave ; }
#define GB_LTMInteger_Op2_In1(ndigs,op,z,x,y)			{ GB_GCSafe_Enter ; GB_GCSafe_2(x,y) ; GB_NodeAlloc_LTMMpzDigs_In(ndigs,z)  ; GB_LTM_DoChecked(op( x, y, z )) ; GB_GCSafe_Leave ; }
#define GB_LTMInteger_Op2b_In1(ndigs,op,z,x,y)			{ GB_GCSafe_Enter ; GB_GCSafe_1(x)   ; GB_NodeAlloc_LTMMpzDigs_In(ndigs,z)  ; GB_LTM_DoChecked(op( x, y, z )) ; GB_GCSafe_Leave ; }
#define GB_LTMInteger_Op2_In2(ndigs,op,z1,z2,x,y)		{ GB_GCSafe_Enter ; GB_GCSafe_2(x,y) ; GB_NodeAlloc_LTMMpzDigs_In(ndigs,z1) ; GB_NodeAlloc_LTMMpzDigs_In(ndigs,z2) ; GB_LTM_DoChecked(op( x, y, z1, z2 )) ; GB_GCSafe_Leave ;  }
#define GB_LTMInteger_Op2b_In2(ndigs,op,z1,z2,x,y)		{ GB_GCSafe_Enter ; GB_GCSafe_1(x)   ; GB_NodeAlloc_LTMMpzDigs_In(ndigs,z1) ; GB_NodeAlloc_LTMMpzDigs_In(ndigs,z2) ; GB_LTM_DoChecked(op( x, y, z1, z2 )) ; GB_GCSafe_Leave ;  }
#define GB_LTMInteger_Op2b_In1of2(ndigs,op,z1,x,y)		{ GB_GCSafe_Enter ; GB_GCSafe_1(x)   ; 													\
														  GB_NodeAlloc_LTMMpzDigs_In(ndigs,z1) ; 												\
														  GB_NodePtr _z2 ;																		\
												  		  GB_NodeAlloc_LTMMpzDigs_In_Alloca(ndigs,_z2) ; 										\
												  		  op( x, y, z1, _z2 ) ; 																\
												  		  GB_GCSafe_Leave ; 																	\
														}
#endif

#define GB_LTMInteger_Add_In(z,x,y)						GB_LTMInteger_Op2_In1(GB_LTM_Int_NrDigitsOfAddLike(x,y),mp_add,z,x,y)
#define GB_LTMInteger_Sub_In(z,x,y)						GB_LTMInteger_Op2_In1(GB_LTM_Int_NrDigitsOfAddLike(x,y),mp_sub,z,x,y)
#define GB_LTMInteger_Mul_In(z,x,y)						GB_LTMInteger_Op2_In1(GB_LTM_Int_NrDigitsOfMulLike(x,y),mp_mul,z,x,y)

// hardcoded: extra 2-tuple size ensured
#define GB_LTMInteger_DivMod_In(z1,z2,x,y)				{ GB_LTMInteger_Op2_In2_Extra(GB_LTM_Int_NrDigitsOfDivLike(x,y),3,mp_div,z1,z2,x,y) ;	\
														  Bool xneg = mp_isneg(x) == MP_YES ;													\
														  Bool yneg = mp_isneg(y) == MP_YES ;													\
														  if ( xneg && (! yneg) || yneg && (! xneg) ) {											\
														    GB_LTM_DoChecked(mp_sub_d( z1, 1, z1 )) ;											\
														    GB_LTM_DoChecked(mp_add( z2, y, z2 )) ;												\
														  }																						\
														}

// hardcoded: extra 2-tuple size ensured
#define GB_LTMInteger_QuotRem_In(z1,z2,x,y)				GB_LTMInteger_Op2_In2_Extra(GB_LTM_Int_NrDigitsOfDivLike(x,y),3,mp_div,z1,z2,x,y)

#define GB_LTMInteger_Neg_In(z,x)						GB_LTMInteger_Op1_In1(GB_LTM_Int_Used(x),mp_neg,z,x)

#define GB_LTMInteger_Cmp(x,y)							mp_cmp(x, y)

#endif
%%]

%%[97
#if USE_LTM
#define GB_LTMInteger_And_In(z,x,y)				GB_LTMInteger_Op2_In1(GB_LTM_Int_NrDigitsOfAndLike(x,y),mp_and,z,x,y)
#define GB_LTMInteger_Or_In(z,x,y)				GB_LTMInteger_Op2_In1(GB_LTM_Int_NrDigitsOfAndLike(x,y),mp_or,z,x,y)
#define GB_LTMInteger_Xor_In(z,x,y)				GB_LTMInteger_Op2_In1(GB_LTM_Int_NrDigitsOfAndLike(x,y),mp_xor,z,x,y)
#define GB_LTMInteger_Complement_In(z,x)		GB_LTMInteger_Op1_In1(GB_LTM_Int_Used(x)+1,mp_com,z,x)
#define GB_LTMInteger_ShiftLeft_In(z,x,y)		GB_LTMInteger_Op2b_In1(GB_LTM_Int_Used(x)+GB_LTM_Int_NrDigitsOfBits(y)+1,mp_mul_2d,z,x,y)
#define GB_LTMInteger_ShiftRight_In(z,x,y)		GB_LTMInteger_Op2b_In1of2(GB_LTM_Int_Used(x),mp_div_2d,z,x,y)
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP || USE_LTM
PRIM Float primRationalToFloat( GB_NodePtr nr )
{
	// GB_NodePtr nf ;
	gb_assert_IsNotDangling( (Word)nr, "primRationalToFloat" ) ;
	GB_NodePtr numerator, divisor ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(nr) ;
	GB_GCSafe_2_Zeroed(numerator, divisor) ;
	GB_PassExc_Cast_GCSafe( Word, numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ) ;
	GB_PassExc_Cast_GCSafe( Word, divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ) ;
	gb_assert_IsOkInteger( numerator, "primRationalToFloat numerator" ) ;
	gb_assert_IsOkInteger( divisor, "primRationalToFloat divisor" ) ;
	Float res ;
#	if USE_LTM
		res = Cast( Float, mp_get_double( numerator ) / mp_get_double( divisor ) ) ;
#	else
		res = Cast( Float, mpz_get_d( MPZ(numerator) ) / mpz_get_d( MPZ(divisor) ) ) ;
#	endif
	GB_GCSafe_Leave ;
	return res ;
}

PRIM Double primRationalToDouble( GB_NodePtr nr )
{
	// GB_NodePtr nf ;
	gb_assert_IsNotDangling( (Word)nr, "primRationalToDouble" ) ;
	GB_NodePtr numerator, divisor ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(nr) ;
	GB_GCSafe_2_Zeroed(numerator, divisor) ;
	GB_PassExc_Dflt_GCSafe( 0.0, numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ) ;
	GB_PassExc_Dflt_GCSafe( 0.0, divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ) ;
	GB_GCSafe_Leave ;
	gb_assert_IsOkInteger( numerator, "primRationalToFloat numerator" ) ;
	gb_assert_IsOkInteger( divisor, "primRationalToFloat divisor" ) ;
	Double res ;
#	if USE_LTM
		res = Cast( Double, mp_get_double( numerator ) / mp_get_double( divisor ) ) ;
#	else
		res = Cast( Double, mpz_get_d( MPZ(numerator) ) / mpz_get_d( MPZ(divisor) ) ) ;
#	endif
	return res ;
}

PRIM Float primIntegerToFloat( GB_NodePtr n )
{
	gb_assert_IsNotDangling( (Word)n, "primIntegerToFloat" ) ;
	gb_assert_IsOkInteger( n, "primIntegerToFloat" ) ;
	Float res ;
#	if USE_LTM
		res = Cast( Float, mp_get_double( n ) ) ;
#	else
		res = Cast( Float, mpz_get_d( MPZ(n) ) ) ;
#	endif
	return res ;
}

PRIM Double primIntegerToDouble( GB_NodePtr n )
{
	gb_assert_IsNotDangling( (Word)n, "primIntegerToDouble" ) ;
	gb_assert_IsOkInteger( n, "primIntegerToDouble" ) ;
	Double res ;
#	if USE_LTM
		res = Cast( Double, mp_get_double( n ) ) ;
#	else
		res = Cast( Double, mpz_get_d( MPZ(n) ) ) ;
#	endif
	return res ;
}

PRIM Word primIntegerToInt( GB_NodePtr n )
{
	gb_assert_IsNotDangling( (Word)n, "primIntegerToInt" ) ;
	gb_assert_IsOkInteger( n, "primIntegerToInt" ) ;
#	if USE_LTM
		return ( mp_get_sint64( n ) ) ;
#	else
		return ( mpz_get_si( MPZ(n) ) ) ;
#	endif
}

PRIM GB_NodePtr primCStringToInteger( char* s )
{
	GB_NodePtr n ;
	// GB_GCSafe_Enter ;
	// GB_GCSafe_1_Zeroed(n) ;
#	if USE_LTM
		GB_NodeAlloc_LTMMpzDigs_In(EntierUpDivBy(strlen(s)*4,DIGIT_BIT)+1,n) ;
		GB_LTM_DoChecked(mp_read_radix( n, s, 10 )) ;
		// printf("primCStringToInteger s=%s\n",s) ;
		// prLTM(n,"primCStringToInteger") ;
#	else
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_In_Ensured(n) ;
		mpz_set_str( MPZ(n), s, 10 ) ;
#	endif
	// GB_GCSafe_Leave ;
	return n ;
}

PRIM GB_NodePtr primIntToInteger( GB_Int x )
{
	GB_NodePtr n ;
	// GB_GCSafe_Enter ;
	// GB_GCSafe_1_Zeroed(n) ;
#	if USE_LTM
		GB_NodeAlloc_LTMMpz_SetSignedInt_In( n, x ) ;
		// printf("primIntToInteger x=%x\n",x) ;
		// prLTM(n,"primIntToInteger") ;
#	else
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_SetSignedInt_In_Ensured( n, x ) ;
#	endif
	// GB_GCSafe_Leave ;
	return n ;
}

PRIM GB_NodePtr primFloatToInteger( Float x )
{
	GB_NodePtr n ;
	// GB_GCSafe_Enter ;
	// GB_GCSafe_1_Zeroed(n) ;
#	if USE_LTM
		int exp ;
		frexpf( x, &exp ) ;
		GB_NodeAlloc_LTMMpz_SetDbl_In(n,exp - FLT_MANT_DIG,x) ;
#	else
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_SetDbl_In_Ensured( n, x ) ;
#	endif
	// GB_GCSafe_Leave ;
	return n ;
}

PRIM GB_NodePtr primDoubleToInteger( Double x )
{
	GB_NodePtr n ;
	// GB_GCSafe_Enter ;
	// GB_GCSafe_1_Zeroed(n) ;
#	if USE_LTM
		int exp ;
		frexp( x, &exp ) ;
		GB_NodeAlloc_LTMMpz_SetDbl_In(n,exp - DBL_MANT_DIG,x) ;
#	else
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_SetDbl_In_Ensured( n, x ) ;
#	endif
	// GB_GCSafe_Leave ;
	return n ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer, via GMP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!!!!! Todo: adapt allocation for GMP, GC works improperly here because can create race conditions.

%%[97
#if USE_GMP || USE_LTM
PRIM Word primEqInteger( GB_NodePtr x, GB_NodePtr y )
{
	gb_assert_IsNotDangling( (Word)x, "primEqInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primEqInteger y" ) ;
	gb_assert_IsOkInteger( x, "primEqInteger x" ) ;
	gb_assert_IsOkInteger( y, "primEqInteger y" ) ;
#	if USE_LTM
		return RTS_MkBool( GB_LTMInteger_Cmp(x,y) == 0 ) ;
#	else
		return RTS_MkBool( GB_GMPInteger_Cmp(x,y) == 0 ) ;
#	endif
}

PRIM Word primCmpInteger( GB_NodePtr x, GB_NodePtr y )
{
	gb_assert_IsNotDangling( (Word)x, "primCmpInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primCmpInteger y" ) ;
	gb_assert_IsOkInteger( x, "primCmpInteger x" ) ;
	gb_assert_IsOkInteger( y, "primCmpInteger y" ) ;
#	if USE_LTM
		int c = GB_LTMInteger_Cmp(x,y) ;
#	else
		int c = GB_GMPInteger_Cmp(x,y) ;
#	endif
	if ( c < 0 )
		return gb_LT ;
	else if ( c == 0 )
		return gb_EQ ;
  	return gb_GT ;
}

PRIM GB_NodePtr primAddInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primAddInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primAddInteger y" ) ;
	gb_assert_IsOkInteger( x, "primAddInteger x" ) ;
	gb_assert_IsOkInteger( y, "primAddInteger y" ) ;
#	if USE_LTM
		GB_LTMInteger_Add_In(n,x,y) ;
#	else
		GB_GMPInteger_Add_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primSubInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primSubInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primSubInteger y" ) ;
	gb_assert_IsOkInteger( x, "primSubInteger x" ) ;
	gb_assert_IsOkInteger( y, "primSubInteger y" ) ;
#	if USE_LTM
		GB_LTMInteger_Sub_In(n,x,y) ;
#	else
		GB_GMPInteger_Sub_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primMulInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primMulInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primMulInteger y" ) ;
	gb_assert_IsOkInteger( x, "primMulInteger x" ) ;
	gb_assert_IsOkInteger( y, "primMulInteger y" ) ;
#	if USE_LTM
		GB_LTMInteger_Mul_In(n,x,y) ;
#	else
		GB_GMPInteger_Mul_In(n,x,y) ;
#	endif
	gb_assert_IsOkInteger( n, "primMulInteger n" ) ;
	return n ;
}

PRIM GB_NodePtr primDivInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n2 ;
	gb_assert_IsNotDangling( (Word)x, "primDivInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primDivInteger y" ) ;
	gb_assert_IsOkInteger( x, "primDivInteger x" ) ;
	gb_assert_IsOkInteger( y, "primDivInteger y" ) ;
#	if USE_LTM
		// GB_GCSafe_Enter ;
		// GB_GCSafe_2_Zeroed(n,n2) ;
		GB_LTMInteger_DivMod_In(n,n2,x,y) ;
		// GB_GCSafe_Leave ;
#	else
		GB_GMPInteger_Div_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primModInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n2 ;
	gb_assert_IsNotDangling( (Word)x, "primModInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primModInteger y" ) ;
	gb_assert_IsOkInteger( x, "primModInteger x" ) ;
	gb_assert_IsOkInteger( y, "primModInteger y" ) ;
#	if USE_LTM
		// GB_GCSafe_Enter ;
		// GB_GCSafe_2_Zeroed(n,n2) ;
		GB_LTMInteger_DivMod_In(n2,n,x,y) ;
		// GB_GCSafe_Leave ;
#	else
		GB_GMPInteger_Mod_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primQuotInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n2 ;
	gb_assert_IsNotDangling( (Word)x, "primQuotInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primQuotInteger y" ) ;
	gb_assert_IsOkInteger( x, "primQuotInteger x" ) ;
	gb_assert_IsOkInteger( y, "primQuotInteger y" ) ;
#	if USE_LTM
		// GB_GCSafe_Enter ;
		// GB_GCSafe_2_Zeroed(n,n2) ;
		GB_LTMInteger_QuotRem_In(n,n2,x,y) ;
		// GB_GCSafe_Leave ;
#	else
		GB_GMPInteger_Quot_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primRemInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n2 ;
	gb_assert_IsNotDangling( (Word)x, "primRemInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primRemInteger y" ) ;
	gb_assert_IsOkInteger( x, "primRemInteger x" ) ;
	gb_assert_IsOkInteger( y, "primRemInteger y" ) ;
#	if USE_LTM
		// GB_GCSafe_Enter ;
		// GB_GCSafe_2_Zeroed(n,n2) ;
		GB_LTMInteger_QuotRem_In(n2,n,x,y) ;
		// GB_GCSafe_Leave ;
#	else
		GB_GMPInteger_Rem_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primQuotRemInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n1, n2 ;
	gb_assert_IsNotDangling( (Word)x, "primQuotRemInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primQuotRemInteger y" ) ;
	gb_assert_IsOkInteger( x, "primQuotRemInteger x" ) ;
	gb_assert_IsOkInteger( y, "primQuotRemInteger y" ) ;
#	if USE_LTM
		// GB_GCSafe_2_Zeroed(n1,n2) ;
		GB_LTMInteger_QuotRem_In(n1,n2,x,y) ;	/* ensures enough mem */
		GB_MkTupNode2_In(n,n1,n2) ;
#	else
		GB_GCSafe_Enter ;
		GB_GCSafe_2(x,y) ;
		// GB_GCSafe_3_Zeroed(n,n1,n2) ;
		GB_AllocEnsure_Words_Finalizer(2*GB_NodeGMPMpzSize+3, 2) ;
		GB_GMPInteger_QuotRem_In_Ensured(n1,n2,x,y) ;
		GB_GCSafe_Leave ;
#	endif
	GB_MkTupNode2_In_Ensured(n,n1,n2) ;
	return n ;
}

PRIM GB_NodePtr primDivModInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n1, n2 ;
	gb_assert_IsNotDangling( (Word)x, "primDivModInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primDivModInteger y" ) ;
	gb_assert_IsOkInteger( x, "primDivModInteger x" ) ;
	gb_assert_IsOkInteger( y, "primDivModInteger y" ) ;
#	if USE_LTM
		// GB_GCSafe_2_Zeroed(n1,n2) ;
		GB_LTMInteger_DivMod_In(n1,n2,x,y) ;	/* ensures enough mem */
		GB_MkTupNode2_In(n,n1,n2) ;
#	else
		GB_GCSafe_Enter ;
		GB_GCSafe_2(x,y) ;
		GB_AllocEnsure_Words_Finalizer(2*GB_NodeGMPMpzSize+3, 2) ;
		GB_GMPInteger_DivMod_In_Ensured(n1,n2,x,y) ;
		GB_MkTupNode2_In_Ensured(n,n1,n2) ;
		GB_GCSafe_Leave ;
#	endif
	return n ;
}
#endif
%%]

%%[97
#if USE_GMP || USE_LTM
PRIM GB_NodePtr primNegInteger( GB_NodePtr x )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primNegInteger x" ) ;
	gb_assert_IsOkInteger( x, "primNegInteger x" ) ;
#	if USE_LTM
		GB_LTMInteger_Neg_In(n,x) ;
#	else
		GB_GMPInteger_Neg_In(n,x) ;
#	endif
	return n ;
}
#endif
%%]

%%[99
PRIM GB_NodePtr primAndInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primAndInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primAndInteger y" ) ;
	gb_assert_IsOkInteger( x, "primAndInteger x" ) ;
	gb_assert_IsOkInteger( y, "primAndInteger y" ) ;
#	if USE_LTM
		GB_LTMInteger_And_In(n,x,y) ;
#	else
		GB_GMPInteger_And_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primOrInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primOrInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primOrInteger y" ) ;
	gb_assert_IsOkInteger( x, "primOrInteger x" ) ;
	gb_assert_IsOkInteger( y, "primOrInteger y" ) ;
#	if USE_LTM
		GB_LTMInteger_Or_In(n,x,y) ;
#	else
		GB_GMPInteger_Or_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primXorInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primXorInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primXorInteger y" ) ;
	gb_assert_IsOkInteger( x, "primXorInteger x" ) ;
	gb_assert_IsOkInteger( y, "primXorInteger y" ) ;
#	if USE_LTM
		GB_LTMInteger_Xor_In(n,x,y) ;
#	else
		GB_GMPInteger_Xor_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primComplementInteger( GB_NodePtr x )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primComplementInteger x" ) ;
	gb_assert_IsOkInteger( x, "primComplementInteger x" ) ;
#	if USE_LTM
		GB_LTMInteger_Complement_In(n,x) ;
#	else
		GB_GMPInteger_Complement_In(n,x) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primShiftLeftInteger( GB_NodePtr x, Word y )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primShiftLeftInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primShiftLeftInteger y" ) ;
	gb_assert_IsOkInteger( x, "primShiftLeftInteger x" ) ;
#	if USE_LTM
		GB_LTMInteger_ShiftLeft_In(n,x,y) ;
#	else
		GB_GMPInteger_ShiftLeft_In(n,x,y) ;
#	endif
	return n ;
}

PRIM GB_NodePtr primShiftRightInteger( GB_NodePtr x, Word y )
{
	GB_NodePtr n ;
	gb_assert_IsNotDangling( (Word)x, "primShiftRightInteger x" ) ;
	gb_assert_IsNotDangling( (Word)y, "primShiftRightInteger y" ) ;
	gb_assert_IsOkInteger( x, "primShiftRightInteger x" ) ;
#	if USE_LTM
		GB_LTMInteger_ShiftRight_In(n,x,y) ;
#	else
		GB_GMPInteger_ShiftRight_In(n,x,y) ;	// with sign extend
#	endif
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
#	if USE_LTM
		GB_NodeAlloc_LTMMpz_SetSignedInt_In( n, x ) ;
#	else
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_SetSignedInt_In_Ensured( n, x ) ;
#	endif
	return n ;
}

PRIM Int32 primIntegerToInt32( GB_NodePtr n )
{
	Int32 x ;
#	if USE_LTM
		Int64 xx = mp_get_sint64( n ) ;
		// prLTM(n,"primIntegerToInt32") ;
		// printf( "primIntegerToInt32 %x\n", xx ) ;
		x = xx ;
#	else
		x = mpz_get_si( MPZ(n) ) ;
#	endif
	return ( x ) ;
}
#endif
%%]

%%[97
PRIM GB_NodePtr primInt64ToInteger( Int64 x )
{
	GB_NodePtr n ;
	// GB_GCSafe_Enter ;
	// GB_GCSafe_1_Zeroed(n) ;
#	if USE_LTM
		GB_NodeAlloc_LTMMpz_SetSignedInt_In( n, x ) ;
#	else
		Int64 xpos = ( x < 0 ? -x : x ) ;
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_In_Ensured(n) ;
		mpz_import( MPZ(n), 1, -1, 8, 0, 0, &xpos ) ;
		if ( x < 0 ) {
			mpz_neg( MPZ(n), MPZ(n) ) ;
		}
#	endif
	// GB_GCSafe_Leave ;
	return n ;
}

PRIM Int64 primIntegerToInt64( GB_NodePtr n )
{
	Int64 x ;
	gb_assert_IsNotDangling( (Word)n, "primIntegerToInt64" ) ;
	gb_assert_IsOkInteger( n, "primIntegerToInt64" ) ;
#	if USE_LTM
		x = mp_get_sint64( n ) ;
#	else
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
#	endif
	return ( x ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM Word primIntegerToWord( GB_NodePtr n )
{
	gb_assert_IsNotDangling( (Word)n, "primIntegerToWord" ) ;
	gb_assert_IsOkInteger( n, "primIntegerToWord" ) ;
#	if USE_LTM
		return ( mp_get_uint64( n ) ) ;
#	else
		return ( mpz_get_ui( MPZ(n) ) ) ;
#	endif
}

PRIM GB_NodePtr primWordToInteger( Word x )
{
	// printf( "primWordToInteger %x\n", x ) ;
	GB_NodePtr n ;
#	if USE_LTM
		GB_NodeAlloc_LTMMpz_SetUnsignedInt_In( n, x ) ;
#	else
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_SetUnsignedInt_In_Ensured( n, x ) ;
#	endif
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
#	if USE_LTM
		GB_NodeAlloc_LTMMpz_SetUnsignedInt_In( n, x ) ;
#	else
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_SetUnsignedInt_In_Ensured( n, x ) ;
#	endif
	return n ;
}

PRIM Word32 primIntegerToWord32( GB_NodePtr n )
{
	gb_assert_IsNotDangling( (Word)n, "primIntegerToWord32" ) ;
	gb_assert_IsOkInteger( n, "primIntegerToWord32" ) ;
	Word32 x ;
#	if USE_LTM
		x = mp_get_uint64( n ) ;
#	else
		x = mpz_get_ui( MPZ(n) ) ;
#	endif
	return ( x ) ;
}
#endif
%%]

%%[97
PRIM GB_NodePtr primWord64ToInteger( Word64 x )
{
	GB_NodePtr n ;
	// GB_GCSafe_Enter ;
	// GB_GCSafe_1_Zeroed(n) ;
#	if USE_LTM
		GB_NodeAlloc_LTMMpz_SetUnsignedInt_In( n, x ) ;
#	else
		GB_AllocEnsure_GMPMpz(1) ;
		GB_NodeAlloc_GMPMpz_In_Ensured(n) ;
		mpz_import( MPZ(n), 1, -1, 8, 0, 0, &x ) ;
#	endif
	// GB_GCSafe_Leave ;
	return n ;
}

PRIM Word64 primIntegerToWord64( GB_NodePtr n )
{
	Word64 x ;
	gb_assert_IsNotDangling( (Word)n, "primIntegerToWord64" ) ;
	gb_assert_IsOkInteger( n, "primIntegerToWord64" ) ;
#	if USE_LTM
		x = mp_get_uint64( n ) ;
#	else
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
#	endif
	return ( x ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP || USE_LTM
PRIM GB_NodePtr primShowInteger( GB_NodePtr integerNd )
{
	GB_NodePtr n ;
	char* buf ;
	int sz ;
	gb_assert_IsNotDangling( (Word)integerNd, "primShowInteger" ) ;
	gb_assert_IsOkInteger( integerNd, "primShowInteger" ) ;
#	if USE_LTM
		// printf("primShowInteger %p\n",integerNd) ;
		// prLTM(integerNd,"primShowInteger") ;
		GB_LTM_DoChecked(mp_radix_size_estim( integerNd, 10, &sz )) ;
		// printf("primShowInteger bufsz(-2)=%d\n",sz) ;
		// sz *= 2 ;
		buf = alloca( sz + 10 ) ;
		GB_LTM_DoChecked(mp_toradix( integerNd, buf, 10 )) ;
		// printf("primShowInteger bufsz(-2)=%d buf=%s\n",sz,buf) ;
#	else
		sz = mpz_sizeinbase( MPZ(integerNd), 10 ) + 2 ;
		buf = alloca( sz ) ;
		mpz_get_str( buf, 10, MPZ(integerNd) ) ;
#	endif
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
#if USE_LTM
#	define gb_intlDecode_MkIn(n,x)			GB_NodeAlloc_LTMMpz_SetDbl64_In( n, x ) 
#else
#	define gb_intlDecode_MkIn(n,x)			GB_NodeAlloc_GMPMpz_SetDbl_In( n, x ) 
#endif

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
	gb_intlDecode_MkIn( ni, ldexp( mant, mantdig ) ) ;										\
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
	gb_assert_IsNotDangling( (Word)frac, "primEncodeFloat" ) ;
	gb_assert_IsOkInteger( frac, "primEncodeFloat" ) ;
	Float d ;
#	if USE_LTM
		d = ldexp( mp_get_double( frac ), exp ) ;
#	else
		d = ldexp( mpz_get_d( MPZ(frac) ), exp ) ;
#	endif
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
	gb_assert_IsNotDangling( (Word)frac, "primEncodeDouble" ) ;
	gb_assert_IsOkInteger( frac, "primEncodeDouble" ) ;
	// prLTM( frac, "primEncodeDouble" ) ;
	Double d ;
#	if USE_LTM
		d = ldexp( mp_get_double( frac ), exp ) ;
#	else
		d = ldexp( mpz_get_d( MPZ(frac) ), exp ) ;
#	endif
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Finalization of Integer object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_EHC_MM && USE_GMP
// specific Integer finalization, hiding the details
void gb_Node_FinalizeInteger( GB_NodePtr n ) {
	// printf( ">gb_Node_FinalizeInteger %p[%x,%x,%x]\n", n, n->content.fields[0], n->content.fields[1], n->content.fields[2] ) ; fflush( stdout ) ;
	// if (n->content.fields[2]) {
		mpz_clear( MPZ(n) ) ;
		// n->content.fields[2] = 0 ;
	// }
	// printf( "<gb_Node_FinalizeInteger %p[%x,%x,%x]\n", n, n->content.fields[0], n->content.fields[1], n->content.fields[2] ) ; fflush( stdout ) ;
}
#endif
%%]


