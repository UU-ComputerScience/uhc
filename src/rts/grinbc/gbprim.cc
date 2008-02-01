%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives for grin bytecode interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM GB_NodePtr gb_Unit ;

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
PRIM GB_NodePtr gb_primIntToFloat( GB_Int x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Float_In(n) ;
	n->content.flt = (float)GB_GBInt2Int(x) ;
	return n ;
}

PRIM GB_NodePtr gb_primDoubleToFloat( GB_NodePtr nd )
{
	GB_NodePtr nf ;
	GB_NodeAlloc_Float_In(nf) ;
	nf->content.flt = nd->content.dbl ;
	return nf ;
}

PRIM GB_NodePtr gb_primFloatToDouble( GB_NodePtr nf )
{
	GB_NodePtr nd ;
	GB_NodeAlloc_Double_In(nd) ;
	nd->content.dbl = nf->content.flt ;
	return nd ;
}

PRIM GB_NodePtr gb_primIntToDouble( GB_Int x )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Double_In(n) ;
	n->content.dbl = (double)GB_GBInt2Int(x) ;
	return n ;
}

#if USE_GMP
PRIM GB_NodePtr gb_primRationalToFloat( GB_NodePtr nr )
{
	GB_NodePtr nf, numerator, divisor ;
	GB_PassExc( numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ) ;
	GB_PassExc( divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ) ;
	GB_NodeAlloc_Float_In(nf) ;
	nf->content.flt = mpz_get_d( numerator->content.mpz ) / mpz_get_d( divisor->content.mpz ) ;
	return nf ;
}

PRIM GB_NodePtr gb_primRationalToDouble( GB_NodePtr nr )
{
	GB_NodePtr nf, numerator, divisor ;
	GB_PassExc( numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ) ;
	GB_PassExc( divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ) ;
	GB_NodeAlloc_Double_In(nf) ;
	nf->content.dbl = mpz_get_d( numerator->content.mpz ) / mpz_get_d( divisor->content.mpz ) ;
	return nf ;
}

PRIM GB_NodePtr gb_primIntegerToFloat( GB_NodePtr n )
{
	GB_NodePtr nf ;
	GB_NodeAlloc_Float_In(nf) ;
	nf->content.flt = mpz_get_d( n->content.mpz ) ;		// not sure whether this works without explicit truncation or something like that...
	return nf ;
}

PRIM GB_NodePtr gb_primIntegerToDouble( GB_NodePtr n )
{
	GB_NodePtr nf ;
	GB_NodeAlloc_Double_In(nf) ;
	nf->content.dbl = mpz_get_d( n->content.mpz ) ;
	return nf ;
}

PRIM GB_Word gb_primIntegerToInt( GB_NodePtr n )
{
	return GB_Int2GBInt( mpz_get_si( n->content.mpz ) ) ;
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
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_set_si( n->content.mpz, GB_GBInt2Int( x ) ) ;
	return n ;
}

PRIM GB_NodePtr gb_primFloatToInteger( GB_NodePtr nf )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_set_d( n->content.mpz, nf->content.flt ) ;	// not sure whether this works without explicit coercion...
	return n ;
}

PRIM GB_NodePtr gb_primDoubleToInteger( GB_NodePtr nf )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_set_d( n->content.mpz, nf->content.dbl ) ;
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
	IF_GB_TR_ON(3,printf("gb_primAddInt %d(%d)+%d(%d)=%d(%d)\n", GB_GBInt2Int(x), x, GB_GBInt2Int(y), y, GB_GBInt2Int(GB_Int_Add(x,y)), GB_Int_Add(x,y) );) ;
  	return GB_Int_Add(x,y);
}

PRIM GB_Word gb_priv_primAddInt( GB_Word x, GB_Word y )
{
  	return x+y;
}

PRIM GB_Word gb_primSubInt( GB_Word x, GB_Word y )
{
	IF_GB_TR_ON(3,printf("gb_primSubInt %d(%d)-%d(%d)=%d(%d)\n", GB_GBInt2Int(x), x, GB_GBInt2Int(y), y, GB_GBInt2Int(GB_Int_Sub(x,y)), GB_Int_Sub(x,y) );) ;
  	return GB_Int_Sub(x,y);
}

PRIM GB_Word gb_primMulInt( GB_Word x, GB_Word y )
{
	IF_GB_TR_ON(3,printf("gb_primMulInt %d(%d)*%d(%d)=%d(%d)\n", GB_GBInt2Int(x), x, GB_GBInt2Int(y), y, GB_GBInt2Int(GB_Int_Mul(x,y)), GB_Int_Mul(x,y) );) ;
  	return GB_Int_Mul(x,y);
}

/* DivInt and ModInt use euclidean division, ie.
   the modulus is always positive.
   
   These routines are taken from lvm.
*/

PRIM GB_Word gb_primDivInt( GB_Word x, GB_Word y )
{
	GB_Int numerator = GB_GBInt2Int(x) ;
	GB_Int divisor   = GB_GBInt2Int(y) ;
	GB_Int div = numerator / divisor ;
	GB_Int mod = numerator % divisor ;
	
	// todo: if ( divisor == 0 ) ...
	
	/* adjust to euclidean division */
	if ( mod < 0 ) {
		if ( divisor > 0 )
			div -= 1 ;
		else
			div += 1 ;
	}
	
  	return GB_Int2GBInt(div) ;
}
%%]

%%[8
PRIM GB_Word gb_primModInt( GB_Word x, GB_Word y )
{
	GB_Int divisor   = GB_GBInt2Int(y) ;
	GB_Int mod = GB_GBInt2Int(x) % divisor ;
	
	// todo: if ( divisor == 0 ) ...
	
	/* adjust to euclidean modulus */
	if ( mod < 0 ) {
		if ( divisor > 0 )
			mod += divisor ;
		else
			mod -= divisor ;
	}
	
  	return GB_Int2GBInt(mod) ;
}

/* QuotInt and RemInt use truncated division, ie.
   QuotInt D d = trunc(D/d)
   RemInt D d  = D - d*(QuotInt D d)
*/

PRIM GB_Word gb_primQuotInt( GB_Word x, GB_Word y )
{
	// todo: if ( divisor == 0 ) ...
	
  	return GB_Int_Quot(x,y);
}

PRIM GB_Word gb_primRemInt( GB_Word x, GB_Word y )
{
	// todo: if ( divisor == 0 ) ...
	
  	return GB_Int_Rem(x,y);
}
%%]

%%[8
PRIM GB_Int gb_primNegInt( GB_Int x )
{
	return GB_Int_Neg(x) ;
}
%%]

%%[8
PRIM GB_Word gb_primEqInt( GB_Word x, GB_Word y )
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
  	return GB_Int2GBInt(Bits_MaxSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
}

PRIM GB_Word gb_primMinInt()
{
  	return GB_Int2GBInt(Bits_MinSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)+1) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Word gb_primEqFloat( GB_NodePtr x, GB_NodePtr y )
{
	return GB_Float_Cmp(x,y, gb_False, gb_True, gb_False ) ;
}

PRIM GB_Word gb_primCmpFloat( GB_NodePtr x, GB_NodePtr y )
{
	return GB_Float_Cmp(x,y, gb_LT, gb_EQ, gb_GT ) ;
}

PRIM GB_NodePtr gb_primAddFloat( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Float_Add_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primSubFloat( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Float_Sub_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primMulFloat( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Float_Mul_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primDivFloat( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Float_Div_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primNegFloat( GB_NodePtr x )
{
	GB_NodePtr n ;
	GB_Float_Neg_In(n,x) ;
	return n ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Double
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Word gb_primEqDouble( GB_NodePtr x, GB_NodePtr y )
{
	return GB_Double_Cmp(x,y, gb_False, gb_True, gb_False ) ;
}

PRIM GB_Word gb_primCmpDouble( GB_NodePtr x, GB_NodePtr y )
{
	return GB_Double_Cmp(x,y, gb_LT, gb_EQ, gb_GT ) ;
}

PRIM GB_NodePtr gb_primAddDouble( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Double_Add_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primSubDouble( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Double_Sub_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primMulDouble( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Double_Mul_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primDivDouble( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n ;
	GB_Double_Div_In(n,x,y) ;
	return n ;
}

PRIM GB_NodePtr gb_primNegDouble( GB_NodePtr x )
{
	GB_NodePtr n ;
	GB_Double_Neg_In(n,x) ;
	return n ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer, via GMP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	GB_Integer_QuotRem_In(n1,n2,x,y) ;
	GB_MkTupNode2_In(n,n1,n2) ;
	return n ;
}

PRIM GB_NodePtr gb_primDivModInteger( GB_NodePtr x, GB_NodePtr y )
{
	GB_NodePtr n, n1, n2 ;
	GB_Integer_DivMod_In(n1,n2,x,y) ;
	GB_MkTupNode2_In(n,n1,n2) ;
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
	char c = GB_GBInt2Int( x ) ;
	if ( c >= 'A' && c <= 'Z' )
		return gb_True ;
  	return gb_False ;
}

PRIM GB_Word gb_primCharIsLower( GB_Int x )
{
	char c = GB_GBInt2Int( x ) ;
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
  	IF_GB_TR_ON(3,printf("gb_primPackedStringHead %x, %d, %s, %d\n", s, *s, s, GB_Int2GBInt(*s) ););
  	return Cast(GB_Word,GB_Int2GBInt(*s)) ;
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
  	IF_GB_TR_ON(3,printf("gb_primCStringToString1Char1 %x:'%s'[%d]\n", s, s, GB_GBInt2Int(goff) ););
	if ( c ) {
		GB_MkCFunNode2In(n2,&gb_primCStringToString1Char,s,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("gb_primCStringToString1Char2 n %x\n", n ););
  	return n ;
}

PRIM GB_NodePtr gb_primCStringToString( char* s )
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
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit1 n %x\n", n ););
%%[[8
	gb_listForceEval( &n, &sz ) ;
%%][96
	GB_PassExc( gb_listForceEval( &n, &sz ) ) ;
%%]]
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit2 n %x\n", n ););
	GB_List_Iterate(n,sz,{buf[bufInx++] = GB_GBInt2Int(GB_List_Head(n));}) ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit3 n %x\n", n ););
	buf[bufInx] = 0 ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit4 `%s'\n", buf ););
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
	gb_exit( GB_GBInt2Int( exitCode ) ) ;
	return exitCode ; // for now
}
%%]

Whereas this is left to the runtime wrapper (see EHC.Prelude.ehcRunMain) in this version

%%[99 -96.gb_primExitWith
PRIM GB_Word gb_primExitWith( GB_Word exitCode )
{
	rts_exit( GB_GBInt2Int( exitCode ) ) ;
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
  	IF_GB_TR_ON(3,printf("gb_primByteArrayToString1Char %x:'%s'[%d]\n", s, s, igoff ););
	if ( igoff < mn->content.bytearray.size && c ) {
		GB_MkCFunNode2In(n2,&gb_primByteArrayToString1Char,mn,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("gb_primByteArrayToString1Char n %x\n", n ););
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
	GB_PassExcAsWord( n = Cast( GB_NodePtr, gb_eval( a ) ) ) ;
%%]]
  	return GB_Int2GBInt(n->content.bytearray.size) ;
}

/*
  In the following function gb_eval(GB_List_Head(n)) must be stored into a local var (here: xx),
  inlining produces a faulty program.
  Reason unknown :-(.
*/

PRIM GB_NodePtr gb_primStringToByteArray( GB_NodePtr n, GB_Int sz )
{
	GB_NodePtr n2 ;
  	IF_GB_TR_ON(3,printf("gb_primStringToByteArray1 sz=%d n=%x\n", sz, n ););
%%[[95
	gb_listForceEval( &n, &sz ) ;
%%][96
	GB_PassExc( gb_listForceEval( &n, &sz ) ) ;
%%]]
  	IF_GB_TR_ON(3,printf("gb_primStringToByteArray2 sz=%d n=%x\n", sz, n ););
	GB_NodeAlloc_Malloc2_In( sz, n2 ) ;
	GB_BytePtr s = Cast(GB_BytePtr,n2->content.bytearray.ptr) ;
	int bufInx = 0 ;
%%[[95
	GB_List_Iterate(n,sz,{GB_Word xx = gb_eval(GB_List_Head(n)); s[bufInx++] = GB_GBInt2Int(xx);}) ;
%%][96
	GB_List_Iterate(n,sz,{GB_Word xx ; GB_PassExc(xx = gb_eval(GB_List_Head(n))); s[bufInx++] = GB_GBInt2Int(xx);}) ;
%%]]
	// does not work: GB_List_Iterate(n,sz,{s[bufInx++] = GB_GBInt2Int(gb_eval(GB_List_Head(n)));}) ;
  	IF_GB_TR_ON(3,printf("gb_primStringToByteArray4 bufInx=%d, n=%x buf=", bufInx, n ););
  	IF_GB_TR_ON(3,{int i ; for (i = 0 ; i < bufInx ; i++) {printf(" %d",s[i]);};});
  	IF_GB_TR_ON(3,printf("\n"););
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
	int i = GB_GBInt2Int(intNd) ;
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
#if USE_GMP
PRIM GB_NodePtr gb_primShowInteger( GB_NodePtr integerNd )
{
	int sz = mpz_sizeinbase( integerNd->content.mpz, 10 ) + 2 ;
	char* buf = alloca( sz ) ;

	mpz_get_str( buf, 10, integerNd->content.mpz ) ;
	GB_NodePtr n ;
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
%%% IO Channels
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

PRIM GB_Word gb_primEqChan( GB_NodePtr chan1, GB_NodePtr chan2 )
{
	if ( fileno(chan1->content.chan.file) == fileno(chan2->content.chan.file) )
		return gb_True ;
  	return gb_False ;
}

PRIM GB_Word gb_primChanNumber( GB_NodePtr chan )
{
	return GB_Int2GBInt( fileno(chan->content.chan.file) ) ;
}

/*
 * mbHandleNr to be used only for std{in,out,err}, ignoring the opening mode.
 */

PRIM GB_NodePtr gb_primOpenChan( GB_NodePtr nmNd, GB_Word modeEnum, GB_NodePtr mbHandleNr )
{
	int nmSz = 0 ;
	GB_PassExc( gb_listForceEval( &nmNd, &nmSz ) ) ;
	char* nm = alloca( nmSz + 1 ) ;
	GB_PassExc( gb_copyCStringFromEvalString( nm, nmNd, nmSz ) ) ;	
	nm[ nmSz ] = 0 ;

	GB_PassExc( mbHandleNr = Cast( GB_NodePtr, gb_eval( Cast(GB_Word,mbHandleNr) ) ) ) ;
	Bool mbHandleNrIsJust = False ;
	GB_Word mbHandleNrFromJust ;
	if ( GB_NH_Fld_Tag(mbHandleNr->header) == GB_Tag_Maybe_Just ) {
		mbHandleNrIsJust = True ;
		GB_PassExc( mbHandleNrFromJust = gb_eval( mbHandleNr->content.fields[0] ) ) ;
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

		return gb_intl_throwIOExceptionFromPrim( ioe_handle, ioe_type, ioe_filename, strerror( errno ) ) ;
	}
	
	GB_NodePtr chan ;
	GB_NodeAlloc_Chan_In(chan) ;
	chan->content.chan.file = f ;
	chan->content.chan.name = nmNd ;
	chan->content.chan.isText = isText ;
	
	return chan ;
}

PRIM GB_NodePtr gb_primCloseChan( GB_NodePtr chan )
{
	fclose(chan->content.chan.file) ;
	return gb_Unit ;
}

GB_NodePtr gb_throwChanInteractionException( GB_NodePtr chan, char* strErr )
{
	GB_NodePtr ioe_handle ;
	GB_Word    ioe_type ;
	GB_NodePtr ioe_filename ;
	
	GB_MkMaybeJust( ioe_filename, chan->content.chan.name ) ;
	GB_MkMaybeJust( ioe_handle, chan ) ;
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
	
	// printf( "%d ", feof( f ) ) ;
	GB_PassExc( gb_getChanEOFOrThrowExc( chan, throwExcForEOF, isEof ) ) ;
	if ( *isEof ) {
		c == EOF ;
	} else {
		c = getc( f ) ;
		if ( c == EOF ) {
			GB_PassExc( gb_getChanEOFOrThrowExc( chan, throwExcForEOF, isEof ) ) ;
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
	return NULL ;
}

PRIM GB_NodePtr gb_primChanGetChar( GB_NodePtr chan )
{
	Bool isEof ;
	int c ;
	GB_PassExc( gb_ChanGetChar( chan, True, &isEof, &c ) ) ;
	return Cast(GB_NodePtr,GB_Int2GBInt(c)) ;
}

PRIM GB_NodePtr gb_primChanGetContents( GB_NodePtr chan )
{
	Bool isEof ;
	GB_NodePtr res ;

	int c ;
	GB_PassExc( gb_ChanGetChar( chan, False, &isEof, &c ) ) ;
	if ( isEof ) {
		GB_MkListNil( res ) ;
	} else if ( c == EOF ) {
		return gb_throwChanInteractionException( chan, strerror( errno ) ) ;
	} else {
		GB_NodePtr n ;
		GB_MkCFunNode1In(n,&gb_primChanGetContents,chan) ;
		GB_MkListCons(res,GB_Int2GBInt(c),n) ;
	}
	
	return res ;
}

%%]

%%[98
GB_NodePtr gb_ThrowWriteChanError( GB_NodePtr chan )
{
	GB_NodePtr ioe_handle ;
	GB_Word    ioe_type ;
	GB_NodePtr ioe_filename ;

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
	
	return gb_intl_throwIOExceptionFromPrim( ioe_handle, ioe_type, ioe_filename, strerror( errno ) ) ;
}
%%]

%%[98
PRIM GB_NodePtr gb_primFlushChan( GB_NodePtr chan )
{
	fflush( chan->content.chan.file ) ;
	return gb_Unit ;
}

PRIM GB_NodePtr gb_primPutCharChan( GB_NodePtr chan, GB_Word c )
{	
	int c2 = putc( GB_GBInt2Int(c), chan->content.chan.file ) ;
	if (c2 == EOF) {
		GB_PassExc( gb_ThrowWriteChanError( chan ) ) ;
	}
	return gb_Unit ;
}

PRIM GB_NodePtr gb_primWriteChan( GB_NodePtr chan, GB_NodePtr a )
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

