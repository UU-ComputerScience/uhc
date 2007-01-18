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

PRIM GB_Word gb_primDivInt( GB_Word x, GB_Word y )
{
	IF_GB_TR_ON(3,printf("gb_primDivInt %d(%d)/%d(%d)=%d(%d)\n", GB_GBInt2Int(x), x, GB_GBInt2Int(y), y, GB_GBInt2Int(GB_Int_Div(x,y)), GB_Int_Div(x,y) );) ;
  	return GB_Int_Div(x,y);
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
%%% Integer, via GMP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
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

PRIM GB_NodePtr gb_primCString2Integer( char* s )
{
	GB_NodePtr n ;
	GB_NodeAlloc_Mpz_In(n) ;
	mpz_set_str( n->content.mpz, s, 10 ) ;
	return n ;
}

PRIM GB_Word gb_primInteger2Int( GB_NodePtr n )
{
	return GB_Int2GBInt( mpz_get_si( n->content.mpz ) ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% String
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
GB_NodePtr gb_primCString2String1Char( char* s, GB_Int goff )
{
	char c = s[ GB_GBInt2Int(goff) ] ;
  	GB_NodePtr n, n2 ;
  	IF_GB_TR_ON(3,printf("gb_primCString2String1Char1 %x:'%s'[%d]\n", s, s, GB_GBInt2Int(goff) ););
	if ( c ) {
		GB_MkCFunNode2(n2,&gb_primCString2String1Char,s,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("gb_primCString2String1Char2 n %x\n", n ););
  	return n ;
}

PRIM GB_NodePtr gb_primCString2String( char* s )
{
  	return gb_primCString2String1Char( s, GB_Int0 ) ;
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
	gb_listForceEval( &n, sz ) ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit2 n %x\n", n ););
	GB_List_Iterate(n,sz,{buf[bufInx++] = GB_GBInt2Int(GB_List_Head(n));}) ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit3 n %x\n", n ););
	buf[bufInx] = 0 ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit4 `%s'\n", buf ););
	rts_error( buf ) ;
	return n ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95
GB_NodePtr gb_primMallocCString2String1Char( GB_NodePtr mn, GB_Int goff )
{
	char* s = Cast(char*,mn->content.ptr) ;
	char c = s[ GB_GBInt2Int(goff) ] ;
  	GB_NodePtr n, n2 ;
  	IF_GB_TR_ON(3,printf("gb_primMallocCString2String1Char %x:'%s'[%d]\n", s, s, GB_GBInt2Int(goff) ););
	if ( c ) {
		GB_MkCFunNode2(n2,&gb_primMallocCString2String1Char,mn,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("gb_primMallocCString2String1Char n %x\n", n ););
  	return n ;
}

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
	GB_NodeAlloc_Malloc_In( sz, n ) ;
	memcpy( n->content.ptr, buf, sz ) ;
	
  	return gb_primMallocCString2String1Char( n, GB_Int0 ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception handling, program running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
PRIM GB_Word gb_primCatchException( GB_Word e, GB_Word handler )
{
	return e ; // for now
}

PRIM GB_Word gb_primExitWith( GB_Word e )
{
	exit( GB_GBInt2Int( e ) ) ;
	return e ; // for now
}
%%]
