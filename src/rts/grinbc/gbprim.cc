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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
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
	GB_NodePtr nf ;
	GB_NodePtr numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ;
	GB_NodePtr divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ;
	GB_NodeAlloc_Float_In(nf) ;
	nf->content.flt = mpz_get_d( numerator->content.mpz ) / mpz_get_d( divisor->content.mpz ) ;
	return nf ;
}

PRIM GB_NodePtr gb_primRationalToDouble( GB_NodePtr nr )
{
	GB_NodePtr nf ;
	GB_NodePtr numerator = Cast(GB_NodePtr,gb_eval(nr->content.fields[0])) ;
	GB_NodePtr divisor   = Cast(GB_NodePtr,gb_eval(nr->content.fields[1])) ;
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

%%[97
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

%%[97
PRIM GB_Int gb_primNegInt( GB_Int x )
{
	return GB_Int_Neg(x) ;
}
%%]

%%[8
PRIM GB_Word gb_primEqInt( GB_Word x, GB_Word y )
{
	if ( x == y )
		return Cast(GB_Word,gb_True) ;
  	return Cast(GB_Word,gb_False) ;
}

PRIM GB_Word gb_primGtInt( GB_Int x, GB_Int y )
{
	if ( x > y )
		return Cast(GB_Word,gb_True) ;
  	return Cast(GB_Word,gb_False) ;
}

PRIM GB_Word gb_primLtInt( GB_Int x, GB_Int y )
{
	if ( x < y )
		return Cast(GB_Word,gb_True) ;
  	return Cast(GB_Word,gb_False) ;
}

PRIM GB_Word gb_primCmpInt( GB_Int x, GB_Int y )
{
	if ( x < y )
		return Cast(GB_Word,gb_LT) ;
	else if ( x == y )
		return Cast(GB_Word,gb_EQ) ;
  	return Cast(GB_Word,gb_GT) ;
}

%%]

%%[95
PRIM GB_Word gb_primMaxInt()
{
  	return GB_Int2GBInt(Bits_MaxSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
}

PRIM GB_Word gb_primMinInt()
{
  	return GB_Int2GBInt(Bits_MinSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Word gb_primEqFloat( GB_NodePtr x, GB_NodePtr y )
{
	return GB_Float_Cmp(x,y, Cast(GB_Word,gb_False), Cast(GB_Word,gb_True), Cast(GB_Word,gb_False) ) ;
}

PRIM GB_Word gb_primCmpFloat( GB_NodePtr x, GB_NodePtr y )
{
	return GB_Float_Cmp(x,y, Cast(GB_Word,gb_LT), Cast(GB_Word,gb_EQ), Cast(GB_Word,gb_GT) ) ;
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
	return GB_Double_Cmp(x,y, Cast(GB_Word,gb_False), Cast(GB_Word,gb_True), Cast(GB_Word,gb_False) ) ;
}

PRIM GB_Word gb_primCmpDouble( GB_NodePtr x, GB_NodePtr y )
{
	return GB_Double_Cmp(x,y, Cast(GB_Word,gb_LT), Cast(GB_Word,gb_EQ), Cast(GB_Word,gb_GT) ) ;
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
		return Cast(GB_Word,gb_True) ;
  	return Cast(GB_Word,gb_False) ;
}

PRIM GB_Word gb_primCmpInteger( GB_NodePtr x, GB_NodePtr y )
{
	int c = GB_Integer_Cmp(x,y) ;
	if ( c < 0 )
		return Cast(GB_Word,gb_LT) ;
	else if ( c == 0 )
		return Cast(GB_Word,gb_EQ) ;
  	return Cast(GB_Word,gb_GT) ;
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
		return Cast(GB_Word,gb_True) ;
  	return Cast(GB_Word,gb_False) ;
}

PRIM GB_Word gb_primCharIsLower( GB_Int x )
{
	char c = GB_GBInt2Int( x ) ;
	if ( c >= 'a' && c <= 'z' )
		return Cast(GB_Word,gb_True) ;
  	return Cast(GB_Word,gb_False) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% String
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	gb_listForceEval( &n, &sz ) ;
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
	GB_NodePtr n = Cast( GB_NodePtr, gb_eval( a ) ) ;
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}

PRIM GB_Word gb_primByteArrayLength( GB_Word a )
{
	GB_NodePtr n = Cast( GB_NodePtr, gb_eval( a ) ) ;
  	return GB_Int2GBInt(n->content.bytearray.size) ;
}

PRIM GB_NodePtr gb_primStringToByteArray( GB_NodePtr n, GB_Int sz )
{
	GB_NodePtr n2 ;
	int bufInx = 0 ;
	gb_listForceEval( &n, &sz ) ;
	GB_NodeAlloc_Malloc2_In( sz, n2 ) ;
	char* s = Cast(char*,n2->content.bytearray.ptr) ;
	GB_List_Iterate(n,sz,{s[bufInx++] = GB_GBInt2Int(GB_List_Head(n));}) ;
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
	int sz = strlen(buf) + 1 ;
	GB_NodeAlloc_Malloc2_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception handling, program running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
PRIM GB_Word gb_primCatchException( GB_Word e, GB_Word handler )
{
	return gb_intl_primCatchException( e, handler ) ;
}

PRIM GB_Word gb_primThrowException( GB_Word exc )
{
	return gb_intl_primThrowException( exc ) ;
}
%%]

%%[96
PRIM GB_Word gb_primExitWith( GB_Word e )
{
	gb_exit( GB_GBInt2Int( e ) ) ;
	return e ; // for now
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO Channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98
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

PRIM GB_Word gb_primEqChan( GB_NodePtr c1, GB_NodePtr c2 )
{
	if ( fileno(c1->content.chan.file) == fileno(c2->content.chan.file) )
		return Cast(GB_Word,gb_True) ;
  	return Cast(GB_Word,gb_False) ;
}

PRIM GB_Word gb_primChanNumber( GB_NodePtr c )
{
	return GB_Int2GBInt( fileno(c->content.chan.file) ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO Actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98
PRIM GB_Word gb_primFlushChan( GB_NodePtr c )
{
	fflush( c->content.chan.file ) ;
	return Cast(GB_Word,gb_Unit) ;
}

PRIM GB_Word gb_primWriteChan( GB_NodePtr c, GB_NodePtr a )
{
  	IF_GB_TR_ON(3,printf("gb_primWriteChan sz %d\n", a->content.bytearray.size ););
	fwrite( a->content.bytearray.ptr, 1, a->content.bytearray.size, c->content.chan.file ) ;
	return Cast(GB_Word,gb_Unit) ;
}

%%]

