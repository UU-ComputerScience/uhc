%%[8
#include "../rts.h"
#include "interpreter.h"
%%]

%%[99
#include "errno.h"
%%]

%%[8.dummyForLinker
int dummy_array ;
%%]

%%[99 -8.dummyForLinker
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives + plain functions for grin bytecode interpreter, those related to arrays
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Byte array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95
PRIM GB_NodePtr primByteArrayToString1Char( GB_NodePtr mn, GB_Int goff )
{
	gb_assert_IsNotIndirection(Cast(Word,mn),"primByteArrayToString1Char BEF") ;
	char* s = Cast(char*,mn->content.bytearray.ptr) ;
	int   igoff = GB_GBInt2Int(goff) ;
	char  c = s[ igoff ] ;
  	GB_NodePtr n, n2 ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(mn) ;
	GB_GCSafe_2_Zeroed(n,n2) ;
  	IF_GB_TR_ON(3,printf("primByteArrayToString1Char %x:'%s'[%d]\n", s, s, igoff ););
	if ( igoff < mn->content.bytearray.size && c ) {
		GB_MkCFunNode2In(n2,&primByteArrayToString1Char,mn,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
		IF_GB_TR_ON(3,printf("primByteArrayToString1Char Cons n=%p h(n)=%x, n2=%p h(n2)=%x\n", n, n->header, n2, n2->header ););
	} else {
  		GB_MkListNil(n) ;
		IF_GB_TR_ON(3,printf("primByteArrayToString1Char Nil n=%p h(n)=%x\n", n, n->header ););
	}
	GB_GCSafe_Leave ;
	IF_GB_TR_ON(3,printf("primByteArrayToString1Char AFT n=%p\n", n ););
  	return n ;
}

PRIM GB_NodePtr primByteArrayToString( Word a )
{
	GB_NodePtr n ;
%%[[95
	n = Cast( GB_NodePtr, gb_eval( a ) ) ;
%%][96
	GB_PassExc( n = Cast( GB_NodePtr, gb_eval( a ) ) ) ;
%%]]
  	return primByteArrayToString1Char( n, GB_Int0 ) ;
}

PRIM Word primByteArrayLength( Word a )
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

PRIM GB_NodePtr primStringToByteArray( GB_NodePtr n, GB_Int sz )
{
	GB_NodePtr n2 ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(n) ;
	GB_GCSafe_1_Zeroed(n2) ;
  	IF_GB_TR_ON(3,printf("primStringToByteArray1 sz=%d n=%p\n", sz, n ););
%%[[95
	// gb_listForceEval( &n, (int*) &sz ) ;
	gb_listForceEval2( n, (int*) &sz ) ;
%%][96
	// GB_PassExc_GCSafe( gb_listForceEval( &n, (int*) &sz ) ) ;
	GB_PassExc_GCSafe( gb_listForceEval2( n, (int*) &sz ) ) ;
%%]]
	n = (GB_NodePtr)gb_Indirection_FollowObject( (Word)n ) ;
	gb_assert_IsNotIndirection(Cast(Word,n),"primStringToByteArray") ;
  	IF_GB_TR_ON(3,printf("primStringToByteArray2 sz=%d n=%p\n", sz, n ););
	GB_NodeAlloc_ByteArray_In( sz, n2 ) ;
	GB_BytePtr s = Cast(GB_BytePtr,n2->content.bytearray.ptr) ;
	int bufInx = 0 ;
%%[[95
	GB_List_Iterate(n,Cast(GB_NodePtr,gb_Indirection_FollowObject(Cast(Word,n))),sz,{Word xx = gb_eval(GB_List_Head(n)); s[bufInx++] = GB_GBInt2Int(xx);}) ;
%%][96
	GB_List_Iterate(n,Cast(GB_NodePtr,gb_Indirection_FollowObject(Cast(Word,n))),sz,{Word xx ; GB_PassExc_GCSafe(xx = gb_eval(GB_List_Head(n))); s[bufInx++] = GB_GBInt2Int(xx);}) ;
%%]]
	// does not work: GB_List_Iterate(n,sz,{s[bufInx++] = GB_GBInt2Int(gb_eval(GB_List_Head(n)));}) ;
  	IF_GB_TR_ON(3,printf("primStringToByteArray4 bufInx=%d, n=%p buf=", bufInx, n ););
  	IF_GB_TR_ON(3,{int i ; for (i = 0 ; i < bufInx ; i++) {printf(" %d",s[i]);};});
  	IF_GB_TR_ON(3,printf("\n"););
	GB_GCSafe_Leave ;
	return n2 ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MutableByteArray interface to ByteArray
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Currently, that is using Boehm GC, a byte array is a node with size + pointer to malloc'ed mem + finalizer

%%[99
PRIM GB_NodePtr primNewByteArray( Word sz )
{
	GB_NodePtr bytearray ;
	// GB_GCSafe_Enter ;
	// GB_GCSafe_1_Zeroed(bytearray) ;
	GB_NodeAlloc_ByteArray_In( sz, bytearray ) ;
	// GB_GCSafe_Leave ;
	return bytearray ;
}

PRIM GB_NodePtr primNewPinnedByteArray( Word sz )
{
	// for now the same
	return primNewByteArray( sz ) ;
}

PRIM Word primByteArrayContents( GB_NodePtr bytearray )
{
  	return (Word)(bytearray->content.bytearray.ptr) ;
}

%%]

%%[99
PRIM Word primSameByteArray( GB_NodePtr bytearray1, GB_NodePtr bytearray2 )
{
	return RTS_MkBool( bytearray1->content.bytearray.ptr == bytearray2->content.bytearray.ptr ) ;
}

%%]

%%[99
PRIM Word8 primIndexWord8Array( GB_NodePtr bytearray, Word inx )
{
	return primReadWord8OffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM Word16 primIndexWord16Array( GB_NodePtr bytearray, Word inx )
{
	return primReadWord16OffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM Word32 primIndexWord32Array( GB_NodePtr bytearray, Word inx )
{
	return primReadWord32OffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM Word64 primIndexWord64Array( GB_NodePtr bytearray, Word inx )
{
	return primReadWord64OffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM double primIndexDoubleArray( GB_NodePtr bytearray, Word inx )
{
	return primReadDoubleOffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

PRIM float primIndexFloatArray( GB_NodePtr bytearray, Word inx )
{
	return primReadFloatOffAddr( bytearray->content.bytearray.ptr, inx ) ;
}

%%]

%%[99
PRIM Word primWriteWord8Array( GB_NodePtr bytearray, Word inx, Word8 val )
{
	return primWriteWord8OffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM Word primWriteWord16Array( GB_NodePtr bytearray, Word inx, Word16 val )
{
	return primWriteWord16OffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM Word primWriteWord32Array( GB_NodePtr bytearray, Word inx, Word32 val )
{
	return primWriteWord32OffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM Word primWriteWord64Array( GB_NodePtr bytearray, Word inx, Word64 val )
{
	return primWriteWord64OffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM Word primWriteFloatArray( GB_NodePtr bytearray, Word inx, float val )
{
	return primWriteFloatOffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

PRIM Word primWriteDoubleArray( GB_NodePtr bytearray, Word inx, double val )
{
	return primWriteDoubleOffAddr( bytearray->content.bytearray.ptr, inx, val ) ;
}

%%]

%%[95
PRIM Word primSizeofByteArray( GB_NodePtr bytearray )
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
PRIM GB_NodePtr primNewArray( Word nWords, Word initVal )
{
	// printf( "primNewArray n=%d\n", nWords ) ;
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
PRIM Word primSameArray( GB_NodePtr array1, GB_NodePtr array2 )
{
	return RTS_MkBool( array1 == array2 ) ;
}

%%]

%%[99
PRIM Word primIndexArray( GB_NodePtr array, Word inx )
{
	// printf( "primIndexArray i=%d\n", inx ) ;
	return array->content.fields[inx] ;
}

PRIM Word primWriteArray( GB_NodePtr array, Word inx, Word val )
{
	// printf( "primWriteArray i=%d v=%x\n", inx, val ) ;
	array->content.fields[inx] = val ;
	return gb_Unit ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO into ByteArray
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
%%]
PRIM Word readIntoByteArray( int fd, GB_NodePtr bytearray, Word off, int sz )
{
	void* ptr = bytearray->content.bytearray.ptr ;
	return readInto(fd,(char *)ptr + off, sz);
}

PRIM Word writeFromByteArray( int fd, GB_NodePtr bytearray, Word off, int sz )
{
	void* ptr = bytearray->content.bytearray.ptr ;
	return writeFrom(fd,(char *)ptr + off, sz);
}


