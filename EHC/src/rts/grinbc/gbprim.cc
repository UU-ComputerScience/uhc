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
PRIM GB_Node gb_False
	= GB_MkConEnumNode( 0 ) ;
PRIM GB_Node gb_True
	= GB_MkConEnumNode( 1 ) ;

PRIM GB_Node gb_Nil
    = GB_MkConEnumNode( GB_Tag_List_Nil ) ;

PRIM GB_Node gb_EQ
	= GB_MkConEnumNode( 0 ) ;
PRIM GB_Node gb_GT
	= GB_MkConEnumNode( 1 ) ;
PRIM GB_Node gb_LT
	= GB_MkConEnumNode( 2 ) ;

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
		return Cast(GB_Word,&gb_True) ;
  	return Cast(GB_Word,&gb_False) ;
}

PRIM GB_Word gb_primGtInt( GB_Int x, GB_Int y )
{
	if ( x > y )
		return Cast(GB_Word,&gb_True) ;
  	return Cast(GB_Word,&gb_False) ;
}

PRIM GB_Word gb_primCmpInt( GB_Int x, GB_Int y )
{
	if ( x < y )
		return Cast(GB_Word,&gb_LT) ;
	else if ( x == y )
		return Cast(GB_Word,&gb_EQ) ;
  	return Cast(GB_Word,&gb_GT) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% String
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
GB_NodePtr gb_primCString2String1Char( char* s, GB_Int goff )
{
	char c = s[ GB_GBInt2Int(goff) ] ;
  	GB_NodePtr n, n2 ;
  	IF_GB_TR_ON(3,printf("gb_primCString2String1Char %x:'%s'[%d]\n", s, s, GB_GBInt2Int(goff) ););
	if ( c ) {
		GB_MkCFunNode2(n2,&gb_primCString2String1Char,s,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,c,n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	return n ;
}

PRIM GB_NodePtr gb_primCString2String( char* s )
{
  	return gb_primCString2String1Char( s, GB_Int0 ) ;
}

PRIM GB_NodePtr gb_primTraceStringExit( GB_NodePtr n )
{
	char buf[100] ;
	int bufInx = 0 ;
	int sz = 99 ;
	gb_listForceEval( n, sz ) ;
	GB_List_Iterate(n,sz,buf[bufInx++] = GB_List_Head(n)) ;
	buf[bufInx] = 0 ;
	rts_error( buf ) ;
	return n ;
}
%%]


