%%[8
#include "../rts.h"
%%]

%%[99
#include "errno.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives + plain functions for grin bytecode interpreter, those related to arrays
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Byte array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95
PRIM GB_NodePtr gb_primByteArrayToString1Char( GB_NodePtr mn, GB_Int goff )
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
	GB_NodeAlloc_ByteArray_In( sz, n2 ) ;
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
%%% MutableByteArray interface to ByteArray
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Currently, that is using Boehm GC, a byte array is a node with size + pointer to malloc'ed mem + finalizer

%%[99
PRIM GB_NodePtr gb_primNewByteArray( GB_Word sz )
{
	GB_NodePtr bytearray ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1_Zeroed(bytearray) ;
	GB_NodeAlloc_ByteArray_In( sz, bytearray ) ;
	GB_GC_SafeLeave ;
	return bytearray ;
}

PRIM GB_NodePtr gb_primNewPinnedByteArray( GB_Word sz )
{
	// for now the same
	return gb_primNewByteArray( sz ) ;
}

PRIM GB_Word gb_primByteArrayContents( GB_NodePtr bytearray )
{
  	return (GB_Word)(bytearray->content.bytearray.ptr) ;
}

%%]

%%[99
PRIM GB_Word gb_primSameByteArray( GB_NodePtr bytearray1, GB_NodePtr bytearray2 )
{
	if ( bytearray1->content.bytearray.ptr == bytearray2->content.bytearray.ptr )
		return gb_True ;
	else
		return gb_False ;
}

%%]

%%[99
PRIM Word8 gb_primIndexWord8Array( GB_NodePtr bytearray, GB_Word inx )
{
	return gb_primReadWord8OffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM Word16 gb_primIndexWord16Array( GB_NodePtr bytearray, GB_Word inx )
{
	return gb_primReadWord16OffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM Word32 gb_primIndexWord32Array( GB_NodePtr bytearray, GB_Word inx )
{
	return gb_primReadWord32OffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM Word64 gb_primIndexWord64Array( GB_NodePtr bytearray, GB_Word inx )
{
	return gb_primReadWord64OffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM double gb_primIndexDoubleArray( GB_NodePtr bytearray, GB_Word inx )
{
	return gb_primReadDoubleOffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM float gb_primIndexFloatArray( GB_NodePtr bytearray, GB_Word inx )
{
	return gb_primReadFloatOffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

%%]

%%[99
PRIM GB_Word gb_primWriteWord8Array( GB_NodePtr bytearray, GB_Word inx, Word8 val )
{
	return gb_primWriteWord8OffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM GB_Word gb_primWriteWord16Array( GB_NodePtr bytearray, GB_Word inx, Word16 val )
{
	return gb_primWriteWord16OffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM GB_Word gb_primWriteWord32Array( GB_NodePtr bytearray, GB_Word inx, Word32 val )
{
	return gb_primWriteWord32OffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM GB_Word gb_primWriteWord64Array( GB_NodePtr bytearray, GB_Word inx, Word64 val )
{
	return gb_primWriteWord64OffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM GB_Word gb_primWriteFloatArray( GB_NodePtr bytearray, GB_Word inx, float val )
{
	return gb_primWriteFloatOffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM GB_Word gb_primWriteDoubleArray( GB_NodePtr bytearray, GB_Word inx, double val )
{
	return gb_primWriteDoubleOffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

%%]

%%[95
PRIM GB_Word gb_primSizeofByteArray( GB_NodePtr bytearray )
{
  	return (bytearray->content.bytearray.size) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MutableArray interface to Array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
// allocate array of sz words,
// for all lower level purposes it is a tuple of size sz.
#define GB_NodeAlloc_Array_In(sz,n)				GB_MkConNodeN(n,sz,0)
%%]

%%[99
PRIM GB_NodePtr gb_primNewArray( GB_Word nWords, GB_Word initVal )
{
	// printf( "gb_primNewArray n=%d\n", nWords ) ;
	GB_NodePtr array ;
	GB_NodeAlloc_Array_In( nWords, array ) ;
	int i ;
	for ( i = 0 ; i < nWords ; i++ ) {
		array->content.fields[i] = initVal ;
	}
	return array ;
}

%%]

%%[99
PRIM GB_Word gb_primSameArray( GB_NodePtr array1, GB_NodePtr array2 )
{
	if ( array1 == array2 )
		return gb_True ;
	else
		return gb_False ;
}

%%]

%%[99
PRIM GB_Word gb_primIndexArray( GB_NodePtr array, GB_Word inx )
{
	// printf( "gb_primIndexArray i=%d\n", inx ) ;
	return array->content.fields[inx] ;
}

PRIM GB_Word gb_primWriteArray( GB_NodePtr array, GB_Word inx, GB_Word val )
{
	// printf( "gb_primWriteArray i=%d v=%x\n", inx, val ) ;
	array->content.fields[inx] = val ;
	return (GB_Word)gb_Unit ;
}

%%]

