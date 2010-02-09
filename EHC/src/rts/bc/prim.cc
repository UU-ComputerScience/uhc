%%[8
#include "../rts.h"
#include "interpreter.h"
%%]

%%[97
#include <alloca.h>
%%]

%%[99
#include "errno.h"
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives for bc backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM Word gb_Unit 
	= GB_MkConEnumNodeAsTag( 0 ) ;

PRIM Word gb_False
	= GB_MkConEnumNodeAsTag( 0 ) ;
PRIM Word gb_True
	= GB_MkConEnumNodeAsTag( 1 ) ;

#if ! USE_EHC_MM
GB_Node gb_Nil_Node
    = GB_MkConEnumNode( GB_Tag_List_Nil ) ;
#endif

PRIM GB_Node* gb_Nil
#if USE_EHC_MM
    = NULL ;
#else
    = &gb_Nil_Node ;
#endif

PRIM Word gb_EQ
	= GB_MkConEnumNodeAsTag( 0 ) ;
PRIM Word gb_GT
	= GB_MkConEnumNodeAsTag( 1 ) ;
PRIM Word gb_LT
	= GB_MkConEnumNodeAsTag( 2 ) ;

%%]

%%[98
#if ! USE_EHC_MM
GB_Node gb_Nothing_Node
    = GB_MkConEnumNode( GB_Tag_Maybe_Nothing ) ;
#endif

PRIM GB_Node* gb_Nothing
#if USE_EHC_MM
    = NULL ;
#else
    = &gb_Nothing_Node ;
#endif
%%]

The definition of IOErrorType must coincide with the one in Prelude.hs

%%[98
PRIM Word gb_AlreadyExists
	= GB_MkConEnumNodeAsTag( 0 ) ;
PRIM Word gb_AlreadyInUse
	= GB_MkConEnumNodeAsTag( 1 ) ;
PRIM Word gb_DoesNotExist	
	= GB_MkConEnumNodeAsTag( 2 ) ;
PRIM Word gb_EOF
	= GB_MkConEnumNodeAsTag( 3 ) ;
PRIM Word gb_FullError
	= GB_MkConEnumNodeAsTag( 4 ) ;
PRIM Word gb_IllegalOperation
	= GB_MkConEnumNodeAsTag( 5 ) ;
PRIM Word gb_InappropriateType
	= GB_MkConEnumNodeAsTag( 6 ) ;
PRIM Word gb_InvalidArgument
	= GB_MkConEnumNodeAsTag( 7 ) ;
PRIM Word gb_NoSuchThing
	= GB_MkConEnumNodeAsTag( 8 ) ;
PRIM Word gb_OtherError
	= GB_MkConEnumNodeAsTag( 9 ) ;
PRIM Word gb_PermissionDenied
	= GB_MkConEnumNodeAsTag( 10 ) ;
PRIM Word gb_ResourceBusy
	= GB_MkConEnumNodeAsTag( 11 ) ;
PRIM Word gb_ResourceExhausted
	= GB_MkConEnumNodeAsTag( 12 ) ;
PRIM Word gb_UnsupportedOperation
	= GB_MkConEnumNodeAsTag( 13 ) ;
PRIM Word gb_UserError
	= GB_MkConEnumNodeAsTag( 14 ) ;
%%]

The definition of IOMode must coincide with the one in Prelude.hs

%%[98
PRIM Word gb_AppendBinaryMode
	= GB_MkConEnumNodeAsTag( 0 ) ;
PRIM Word gb_AppendMode
	= GB_MkConEnumNodeAsTag( 1 ) ;
PRIM Word gb_ReadBinaryMode
	= GB_MkConEnumNodeAsTag( 2 ) ;
PRIM Word gb_ReadMode
	= GB_MkConEnumNodeAsTag( 3 ) ;
PRIM Word gb_ReadWriteBinaryMode
	= GB_MkConEnumNodeAsTag( 4 ) ;
PRIM Word gb_ReadWriteMode
	= GB_MkConEnumNodeAsTag( 5 ) ;
PRIM Word gb_WriteBinaryMode
	= GB_MkConEnumNodeAsTag( 6 ) ;
PRIM Word gb_WriteMode
	= GB_MkConEnumNodeAsTag( 7 ) ;
%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Garbage Collection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM Word primGC(  )
{
#	if USE_EHC_MM
		return RTS_MkBool( mm_itf_gc() ) ;
#	else
		return gb_False ;
#	endif
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Weak ptr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Weak pointers are not implemented, so these primitives provide an interface only.
No functionality.
Conceptually:
- a weak ptr is created using a key/value pair, with a finalizer
- a weak ptr is dereferenced, but may be finalized, encoded in a Maybe

%%[99
PRIM Word primMakeWeakPtr( Word key, Word val, Word finalizer )
{
#	if USE_EHC_MM
		return mm_itf_NewWeakPtr( key, val, finalizer ) ;
#	else
		return val ;
#	endif
}

PRIM Word primMakeWeakPtrWOFinalizer( Word key, Word val )
{
#	if USE_EHC_MM
		return mm_itf_NewWeakPtr( key, val, MM_Itf_WeakPtr_NoFinalizer ) ;
#	else
		return val ;
#	endif
}

PRIM GB_NodePtr primDeRefWeakPtr( Word wp )
{
	GB_NodePtr wpDeref ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(wp) ;
#	if USE_EHC_MM
		wp = mm_itf_DerefWeakPtr( wp ) ;
		if ( wp == 0 ) {
			GB_MkMaybeNothing( wpDeref ) ;
		} else {
			GB_MkMaybeJust( wpDeref, wp ) ;
		}
#	else
		GB_MkMaybeJust( wpDeref, wp ) ;
#	endif
	GB_GCSafe_Leave ;
	return wpDeref ;
}

PRIM GB_NodePtr primFinalizeWeakPtr( Word wp )
{
	GB_NodePtr fin ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(wp) ;
#	if USE_EHC_MM
		wp = mm_itf_FinalizeWeakPtr( wp ) ;
		if ( wp == 0 ) {
			GB_MkMaybeNothing( fin ) ;
		} else {
			GB_MkMaybeJust( fin, wp ) ;
		}
#	else
		GB_MkMaybeNothing( fin ) ;
#	endif
	GB_GCSafe_Leave ;
	return fin ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%[8
PRIM GB_NodePtr primQuotRemInt( GB_Int x, GB_Int y )
{
	GB_NodePtr n ;
	GB_Int q = x / y ;
	GB_Int r = x % y ;
  	// printf( "primQuotRemInt %d %d %d %d\n", x, y, q, r ) ;
	GB_MkTupNode2_In(n,GB_Int2GBInt(q),GB_Int2GBInt(r)) ;
	return n ;
}

PRIM GB_NodePtr primDivModInt( GB_Int x, GB_Int y )
{
	GB_NodePtr n ;
	GB_MkTupNode2_In(n, GB_Int2GBInt( primDivInt(x,y) ), GB_Int2GBInt( primModInt(x,y) )) ;
	return n ;
}

%%]

%%[95
PRIM Word primMaxInt()
{
  	// return GB_Int2GBInt(Bits_MaxSInt(Word,Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
  	return (Bits_MaxSInt(Word,Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
}

PRIM Word primMinInt()
{
  	// return GB_Int2GBInt(Bits_MinSInt(Word,Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)+1) ;
  	return (Bits_MinSInt(Word,Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)+1) ;
}
%%]

%%[97
PRIM Word primMaxWord()
{
	// printf( "primMaxWord %x\n", Word32_MaxValue >> GB_Word_SizeOfWordTag ) ;
  	return Word32_MaxValue >> GB_Word_SizeOfWordTag ;
}

PRIM Word primMinWord()
{
  	return Word32_MinValue ;
}
%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% bitwise logical operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIMS_BITLOGIC_CODE((Word_SizeInBits-GB_Word_SizeOfWordTag),Word,Word)
PRIMS_BITLOGIC_CODE(64,Int64,Int64)
PRIMS_BITLOGIC_CODE(64,Word64,Word64)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% bitshift and rotate operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Assume the 8, or 16, bits are put into a larger word. Same for 32 bits but this depends on main word size.
For these we need to use the shift/rotate variants which keep zero the most significant bits

%%[99
PRIMS_BITSHIFT_MASKCODE((Word_SizeInBits-GB_Word_SizeOfWordTag),Int,Int,SWord)
PRIMS_BITSHIFT_MASKCODE(8,Int8,Int8,SWord)
PRIMS_BITSHIFT_MASKCODE(16,Int16,Int16,SWord)
PRIMS_BITSHIFT_MASKCODE(8,Word8,Word8,Word)
PRIMS_BITSHIFT_MASKCODE(16,Word16,Word16,Word)
#ifdef USE_32_BITS
PRIMS_BITSHIFT_CODE(32,Int32,Int32,SWord)
PRIMS_BITSHIFT_CODE(32,Word32,Word32,Word)
#else
PRIMS_BITSHIFT_MASKCODE(32,Int32,Int32,SWord)
PRIMS_BITSHIFT_MASKCODE(32,Word32,Word32,Word)
#endif

PRIMS_BITSHIFT_CODE(64,Int64,Int64,SWord64)
PRIMS_BITSHIFT_CODE(64,Word64,Word64,Word64)
PRIMS_BITSHIFT_MASKCODE((Word_SizeInBits-GB_Word_SizeOfWordTag),Word,Word,Word)


%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% String
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Old implementation of packedStringToString
   deprecated because it allocates cells, and introduces a new thunk tag,
   which is not possible in the Grin Compiler.
   In order to make BC an GrinC use the same prelude, we replaced
   the PackedString handling by the functions above
*/

The implementation is left here for the GB based impl.

%%[8
GB_NodePtr primCStringToString1Char( char* s, GB_Int goff )
{
	char c = s[ GB_GBInt2Int(goff) ] ;
  	GB_NodePtr n, n2 ;
	GB_GCSafe_Enter ;
  	GB_GCSafe_2_Zeroed(n,n2) ;
  	IF_GB_TR_ON(3,printf("primCStringToString1Char1 %p:'%s'[%d]\n", s, s, GB_GBInt2Int(goff) ););
	if ( c ) {
		GB_MkCFunNode2In(n2,&primCStringToString1Char,s,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("primCStringToString1Char2 n %p\n", n ););
  	GB_GCSafe_Leave ;
  	return n ;
}

PRIM GB_NodePtr primCStringToString( char* s )
{
  	return primCStringToString1Char( s, GB_Int0 ) ;
}

// temporary:
PRIM GB_NodePtr gb_priv_primCStringToString( char* s )
{
  	return primCStringToString1Char( s, GB_Int0 ) ;
}
%%]







In the following function GB_List_Iterate causes a Bus error when:
  - compiled on MacIntel, gcc 4.01
  - with -O3
  - without additional trace statements.
  Sigh...

%%[8

PRIM GB_NodePtr primTraceStringExit( GB_NodePtr n )
{
	char buf[100] ;
	int bufInx = 0 ;
	int sz = 99 ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(n) ;
  	IF_GB_TR_ON(3,printf("primTraceStringExit1 n %p\n", n ););
%%[[8
	gb_listForceEval2( n, &sz ) ;
%%][96
	GB_PassExc_GCSafe( gb_listForceEval2( n, &sz ) ) ;
%%]]
  	IF_GB_TR_ON(3,printf("primTraceStringExit2 n %p\n", n ););
	GB_List_Iterate(n,Cast(GB_NodePtr,gb_Indirection_FollowObject(Cast(Word,n))),sz,{buf[bufInx++] = GB_GBInt2Int(GB_List_Head(n));}) ;
  	IF_GB_TR_ON(3,printf("primTraceStringExit3 n %p\n", n ););
	buf[bufInx] = 0 ;
  	IF_GB_TR_ON(3,printf("primTraceStringExit4 `%s'\n", buf ););
	GB_GCSafe_Leave ;
	gb_error( buf ) ;
	return n ;
}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exiting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This version also prints a stacktrace

%%[96.primExitWith
PRIM Word primExitWith( Word exitCode )
{
	// gb_exit( GB_GBInt2Int( exitCode ) ) ;
	gb_exit( exitCode ) ;
	return exitCode ; // for now
}
%%]

Whereas this is left to the runtime wrapper (see UHC.Prelude.ehcRunMain) in this version

%%[99 -96.primExitWith
PRIM Word primExitWith( Word exitCode )
{
	// rts_exit( GB_GBInt2Int( exitCode ) ) ;
	rts_exit( exitCode ) ;
	gb_panic( "impossible: exit failed" ) ;
	return exitCode ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95
PRIM GB_NodePtr primShowInt( GB_Int intNd )
{
	char buf[sizeof(Word)*10] ;
	char *s = buf ;
	// int i = GB_GBInt2Int(intNd) ;
	int i = (intNd) ;
	if ( i < 0 )
	{
		i = -i ;
		*(s++) = '-' ;
	}
	sprintf( s, "%d" , i ) ;
	
  	IF_GB_TR_ON(3,printf("primShowInt s(%d) %s\n", strlen(buf), buf ););
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return primByteArrayToString1Char( n, GB_Int0 ) ;
}
%%]

%%[97

PRIM GB_NodePtr primShowFloat( Float w )
{
	char buf[sizeof(Float)*10] ;
	char *s = buf ;
	sprintf( s, "%f", w ) ;
	
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return primByteArrayToString1Char( n, GB_Int0 ) ;
}

PRIM GB_NodePtr primShowDouble( Double w )
{
	char buf[sizeof(Double)*10] ;
	char *s = buf ;
	sprintf( s, "%lf", w ) ;
	
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return primByteArrayToString1Char( n, GB_Int0 ) ;
}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception handling, program running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
PRIM Word primCatchException( Word e, Word handler )
{
	return gb_intl_primCatchException( e, handler ) ;
}

PRIM GB_NodePtr primThrowException( Word exc )
{
	return gb_intl_throwException( exc ) ;
}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% I/O: MutVar: mutable variables for a State
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM GB_NodePtr primNewMutVar( Word init, Word state )
{
	GB_NodePtr mutVar ;
	GB_NodePtr res ;
	GB_GCSafe_Enter ;
	GB_GCSafe_2(init,state) ;
	GB_GCSafe_1_Zeroed(mutVar) ;
	
	// printf("primNewMutVar\n") ;

	GB_MkTupNode1_In(mutVar,init) ;
	GB_MkTupNode2_In(res,state,mutVar) ;
	
	GB_GCSafe_Leave ;
	return res ;
}

PRIM GB_NodePtr primReadMutVar( GB_NodePtr mutVar, Word state )
{
	GB_NodePtr res ;
	GB_GCSafe_Enter ;
	GB_GCSafe_2(mutVar,state) ;

	// printf("primReadMutVar\n") ;

	GB_MkTupNode2_In(res,state,mutVar->content.fields[0]) ;
	
	GB_GCSafe_Leave ;
	return res ;
}

PRIM Word primWriteMutVar( GB_NodePtr mutVar, Word newVal, Word state )
{
	// printf("primWriteMutVar\n") ;

	mutVar->content.fields[0] = newVal ;
	return state ;
}

PRIM Word primSameMutVar( Word v1, Word v2 )
{
	return RTS_MkBool( v1 == v2 ) ;
}

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
extern char **environ;

PRIM char** getEnviron()
{
	return environ ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Program arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM void getProgArgv( int *argc, char ***argv )
{
	*argc = rtsArgC ;
	*argv = rtsArgV ;
}

PRIM void setProgArgv( int argc, char **argv )
{
	rtsArgC = argc ;
	rtsArgV = argv ;
}
%%]




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Error number
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM int _getErrno()
{
	return errno ;
}

PRIM int _setErrno( int e )
{
	return errno = e ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internals of interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
PRIM Word primCallInfoKindIsVisible( Word kind )
{
	return RTS_MkBool( gb_CallInfo_Kind_IsVisible(kind) ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dummy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.dummyForLinker
int dummy_C ;
%%]

%%[99 -8.dummyForLinker
%%]

