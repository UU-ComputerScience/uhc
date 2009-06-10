%%[8
#include "../rts.h"

%%]

%%[97
#include <alloca.h>
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives for grin bytecode interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM GB_NodePtr gb_Unit 
	= NULL ;

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
PRIM GB_Word gb_InappropriateType
	= GB_MkConEnumNodeAsTag( 6 ) ;
PRIM GB_Word gb_InvalidArgument
	= GB_MkConEnumNodeAsTag( 7 ) ;
PRIM GB_Word gb_NoSuchThing
	= GB_MkConEnumNodeAsTag( 8 ) ;
PRIM GB_Word gb_OtherError
	= GB_MkConEnumNodeAsTag( 9 ) ;
PRIM GB_Word gb_PermissionDenied
	= GB_MkConEnumNodeAsTag( 10 ) ;
PRIM GB_Word gb_ResourceBusy
	= GB_MkConEnumNodeAsTag( 11 ) ;
PRIM GB_Word gb_ResourceExhausted
	= GB_MkConEnumNodeAsTag( 12 ) ;
PRIM GB_Word gb_UnsupportedOperation
	= GB_MkConEnumNodeAsTag( 13 ) ;
PRIM GB_Word gb_UserError
	= GB_MkConEnumNodeAsTag( 14 ) ;
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
PRIM Float gb_primIntToFloat( GB_Int x )
{
	// GB_NodePtr n ;
	// GB_NodeAlloc_Float_In(n) ;
	// n->content.flt = (float)GB_GBInt2Int(x) ;
	// n->content.flt = (float)x ;
	return Cast(Float,x) ;
}

PRIM Float gb_primDoubleToFloat( Double /*nd*/ x )
{
	//GB_NodePtr nf ;
	//GB_NodeAlloc_Float_In(nf) ;
	//nf->content.flt = nd->content.dbl ;
	//return nf ;
	return Cast(Float,x) ;
}

PRIM Double gb_primFloatToDouble( Float x )
{
	//GB_NodePtr nd ;
	//GB_NodeAlloc_Double_In(nd) ;
	//nd->content.dbl = nf->content.flt ;
	//return nd ;
	return Cast(Double,x) ;
}

PRIM Double gb_primIntToDouble( GB_Int x )
{
	// GB_NodePtr n ;
	// GB_NodeAlloc_Double_In(n) ;
	// n->content.dbl = (double)GB_GBInt2Int(x) ;
	// n->content.dbl = (double)x ;
	// return n ;
	return Cast(Double,x) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Double/Float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

We do not know for sure whether we use IEEE or not.
This should be dependending on some compile time C info, but not yet sorted out.

%%[97
PRIM GB_Word gb_primIsIEEE( )
{
	return gb_True ;
}

%%]

%%[97
PRIM GB_Word gb_primRadixDoubleFloat( )
{
	return FLT_RADIX ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Float
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Word gb_primEqFloat( Float x, Float y )
{
	return GB_CmpBasic(x,y, gb_False, gb_True, gb_False ) ;
}

PRIM GB_Word gb_primCmpFloat( Float x, Float y )
{
	return GB_CmpBasic(x,y, gb_LT, gb_EQ, gb_GT ) ;
}

PRIM Float gb_primAddFloat( Float x, Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Add_In(n,x,y) ;
	// return n ;
	return x + y ;
}

PRIM Float gb_primSubFloat( Float x, Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Sub_In(n,x,y) ;
	// return n ;
	return x - y ;
}

PRIM Float gb_primMulFloat( Float x, Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Mul_In(n,x,y) ;
	// return n ;
	return x * y ;
}

PRIM Float gb_primDivFloat( Float x, Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Div_In(n,x,y) ;
	// return n ;
	return x / y ;
}

PRIM Float gb_primNegFloat( Float x )
{
	// GB_NodePtr n ;
	// GB_Float_Neg_In(n,x) ;
	// return n ;
	return -x ;
}

PRIM GB_Word gb_primIsNaNFloat( Float x )
{
	if ( isnan(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsDenormalizedFloat( Float x )
{
	if ( ! isnormal(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsInfiniteFloat( Float x )
{
	if ( isinf(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsNegativeZeroFloat( Float x )
{
	 
	if ( fpclassify(x) == FP_ZERO && signbit(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primDigitsFloat( )
{
	return FLT_MANT_DIG ;
}

PRIM GB_Word gb_primMaxExpFloat( )
{
	return FLT_MAX_EXP ;
}

PRIM GB_Word gb_primMinExpFloat( )
{
	return FLT_MIN_EXP ;
}


%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Double
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
PRIM GB_Word gb_primEqDouble( Double x, Double y )
{
	return GB_CmpBasic(x,y, gb_False, gb_True, gb_False ) ;
}

PRIM GB_Word gb_primCmpDouble( Double x, Double y )
{
	return GB_CmpBasic(x,y, gb_LT, gb_EQ, gb_GT ) ;
}

PRIM Double gb_primAddDouble( Double x, Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Add_In(n,x,y) ;
	// return n ;
	return x + y ;
}

PRIM Double gb_primSubDouble( Double x, Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Sub_In(n,x,y) ;
	// return n ;
	return x - y ;
}

PRIM Double gb_primMulDouble( Double x, Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Mul_In(n,x,y) ;
	// return n ;
	return x * y ;
}

PRIM Double gb_primDivDouble( Double x, Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Div_In(n,x,y) ;
	// return n ;
	return x / y ;
}

PRIM Double gb_primNegDouble( Double x )
{
	// GB_NodePtr n ;
	// GB_Double_Neg_In(n,x) ;
	// return n ;
	return -x ;
}

PRIM GB_Word gb_primIsNaNDouble( Double x )
{
	if ( isnan(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsDenormalizedDouble( Double x )
{
	if ( ! isnormal(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsInfiniteDouble( Double x )
{
	if ( isinf(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsNegativeZeroDouble( Double x )
{
	 
	if ( fpclassify(x) == FP_ZERO && signbit(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primDigitsDouble( )
{
	return DBL_MANT_DIG ;
}

PRIM GB_Word gb_primMaxExpDouble( )
{
	return DBL_MAX_EXP ;
}

PRIM GB_Word gb_primMinExpDouble( )
{
	return DBL_MIN_EXP ;
}


%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Addr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM GB_Word gb_primNullAddr( )
{
	return (GB_Word)NULL ;
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
PRIM GB_Word gb_primMakeWeakPtr( GB_Word key, GB_Word val, GB_Word finalizer )
{
	return val ;
}

PRIM GB_NodePtr gb_primDeRefWeakPtr( GB_Word wp )
{
	GB_NodePtr wpDeref ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1_Zeroed(wpDeref) ;
	GB_MkMaybeJust( wpDeref, wp ) ;
	GB_GC_SafeLeave ;
	return wpDeref ;
}

PRIM GB_NodePtr gb_primFinalizeWeakPtr( GB_Word wp )
{
	GB_NodePtr fin ;
	GB_MkMaybeNothing( fin ) ;
	return fin ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stable ptr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Stable pointers are guaranteed not to be moved by garbage collection.
The primitives below assum the use of the Boehm garbage collector (BGC),
making their implementation simple because nothing is ever moved using BGC.

%%[99
#if USE_BOEHM_GC
PRIM GB_Word gb_primMakeStableAddr( GB_Word a )
{
	return a ;
}

PRIM GB_Word gb_primDeRefStableAddr( GB_Word a )
{
	return a ;
}

PRIM GB_Word gb_primFreeStableAddr( GB_Word a )
{
	return (GB_Word)gb_Unit ;
}

PRIM GB_Word gb_primEqStableAddr( GB_Word x, GB_Word y )
{
	if ( x == y )
		return gb_True ;
  	return gb_False ;
}

#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
INTLIKE_ARITH_PRIMS_CODE(gb_,Int,Int,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,(Word_SizeInBits-GB_Word_SizeOfWordTag),Int,Int,SWord,GB_Word)
%%]

%%[8
PRIM GB_NodePtr gb_primQuotRemInt( GB_Int x, GB_Int y )
{
	GB_NodePtr n ;
	GB_Int q = x / y ;
	GB_Int r = x % y ;
  	// printf( "gb_primQuotRemInt %d %d %d %d\n", x, y, q, r ) ;
	GB_MkTupNode2_In(n,GB_Int2GBInt(q),GB_Int2GBInt(r)) ;
	return n ;
}

PRIM GB_NodePtr gb_primDivModInt( GB_Int x, GB_Int y )
{
	GB_NodePtr n ;
	GB_MkTupNode2_In(n, GB_Int2GBInt( gb_primDivInt(x,y) ), GB_Int2GBInt( gb_primModInt(x,y) )) ;
	return n ;
}

%%]

%%[95
PRIM GB_Word gb_primMaxInt()
{
  	// return GB_Int2GBInt(Bits_MaxSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
  	return (Bits_MaxSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)) ;
}

PRIM GB_Word gb_primMinInt()
{
  	// return GB_Int2GBInt(Bits_MinSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)+1) ;
  	return (Bits_MinSInt(GB_Word,GB_Word_SizeInBits,GB_Word_SizeInBits-GB_Word_SizeOfWordTag)+1) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int8, Int16, Int32, Int64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

See remarks on fitting in word, at code for Word8 etc.

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Int8,Int8)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Int8,Int8,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,8,Int8,Int8,SWord,GB_Word)
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Int16,Int16)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Int16,Int16,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,16,Int16,Int16,SWord,GB_Word)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Int32,Int32)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Int32,Int32,GB_Word)

%%]

%%[99
#ifdef USE_32_BITS
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(gb_,32,Int32,Int32,SWord,GB_Word)
#else
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,32,Int32,Int32,SWord,GB_Word)
#endif
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Int64,Int64)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Int64,Int64,GB_Word)
INTLIKE_ARITH_PRIMS_CODE(gb_,Int64,Int64,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_CODE(gb_,64,Int64,Int64,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(gb_,64,Int64,Int64,SWord64,GB_Word)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
INTLIKE_ARITH_PRIMS_CODE(gb_,Word,Word,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Word,Word,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_CODE(gb_,(Word_SizeInBits-GB_Word_SizeOfWordTag),Word,Word,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,(Word_SizeInBits-GB_Word_SizeOfWordTag),Word,Word,Word,GB_Word)
%%]

%%[97
PRIM Word gb_primMaxWord()
{
	// printf( "gb_primMaxWord %x\n", Word32_MaxValue >> GB_Word_SizeOfWordTag ) ;
  	return Word32_MaxValue >> GB_Word_SizeOfWordTag ;
}

PRIM Word gb_primMinWord()
{
  	return Word32_MinValue ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word8, Word16, Word32, Word64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Assume the 8, or 16, bits are put into a larger word. Same for 32 bits but this depends on main word size.
For these we need to use the shift/rotate variants which keep zero the most significant bits

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Word8,Word8)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Word8,Word8,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,8,Word8,Word8,Word,GB_Word)
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Word16,Word16)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Word16,Word16,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,16,Word16,Word16,Word,GB_Word)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Word32,Word32)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Word32,Word32,GB_Word)

%%]

%%[99
#ifdef USE_32_BITS
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(gb_,32,Word32,Word32,Word,GB_Word)
#else
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,32,Word32,Word32,Word,GB_Word)
#endif
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Word64,Word64)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Word64,Word64,GB_Word)
INTLIKE_ARITH_PRIMS_CODE(gb_,Word64,Word64,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_CODE(gb_,64,Word64,Word64,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(gb_,64,Word64,Word64,Word64,GB_Word)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Char
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM GB_Word gb_primCharIsUpper( GB_Word x )
{
	// char c = GB_GBInt2Int( x ) ;
	char c = ( x ) ;
	if ( c >= 'A' && c <= 'Z' )
		return gb_True ;
  	return gb_False ;
}

PRIM GB_Word gb_primCharIsLower( GB_Word x )
{
	// char c = GB_GBInt2Int( x ) ;
	char c = ( x ) ;
	if ( c >= 'a' && c <= 'z' )
		return gb_True ;
  	return gb_False ;
}
%%]

%%[99
PRIM GB_Word gb_primCharToUpper( GB_Word x )
{
  	return x - 'a' + 'A' ;
}

PRIM GB_Word gb_primCharToLower( GB_Word x )
{
  	return x - 'A' + 'a' ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% String
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

PRIM GB_Word gb_primPackedStringNull( char *s )
{
  	IF_GB_TR_ON(3,printf("gb_primPackedStringNull %p, %d, %s\n", s, *s, s ););
	if ( *s )
		return gb_False ;
  	return gb_True ;
}

PRIM GB_Word gb_primPackedStringTail( char *s )
{
  	IF_GB_TR_ON(3,printf("gb_primPackedStringTail %p, %d, %s\n", s, *s, s ););
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
  	// IF_GB_TR_ON(3,printf("gb_primPackedStringHead %x, %d, %s, %d\n", s, *s, s, GB_Int2GBInt(*s) ););
  	// return Cast(GB_Word,GB_Int2GBInt(*s)) ;
  	return Cast(GB_Word,(*s)) ;
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
	GB_GC_SafeEnter ;
  	GB_GC_Safe2_Zeroed(n,n2) ;
  	IF_GB_TR_ON(3,printf("gb_primCStringToString1Char1 %p:'%s'[%d]\n", s, s, GB_GBInt2Int(goff) ););
	if ( c ) {
		GB_MkCFunNode2In(n2,&gb_primCStringToString1Char,s,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("gb_primCStringToString1Char2 n %p\n", n ););
  	GB_GC_SafeLeave ;
  	return n ;
}

PRIM GB_NodePtr gb_primCStringToString( char* s )
{
  	return gb_primCStringToString1Char( s, GB_Int0 ) ;
}

// temporary:
PRIM GB_NodePtr gb_priv_primCStringToString( char* s )
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
	GB_GC_SafeEnter ;
	GB_GC_Safe1(n) ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit1 n %p\n", n ););
%%[[8
	gb_listForceEval( &n, &sz ) ;
%%][96
	GB_PassExc_GCSafe( gb_listForceEval( &n, &sz ) ) ;
%%]]
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit2 n %p\n", n ););
	GB_List_Iterate(n,sz,{buf[bufInx++] = GB_GBInt2Int(GB_List_Head(n));}) ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit3 n %p\n", n ););
	buf[bufInx] = 0 ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit4 `%s'\n", buf ););
	GB_GC_SafeLeave ;
	gb_error( buf ) ;
	return n ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Storable read/write (peek/poke)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
STORABLE_PEEKPOKE_PRIMS_CODE(gb_,Word8 ,Word8 ,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_CODE(gb_,Word16,Word16,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_CODE(gb_,Word32,Word32,GB_Word) 
// STORABLE_PEEKPOKE_PRIMS_CODE(gb_,Word64,Word64,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_CODE(gb_,Word  ,Word  ,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_CODE(gb_,Float ,float ,GB_Word) 
STORABLE_PEEKPOKE_PRIMS_CODE(gb_,Double,double,GB_Word) 
%%]

Using the above macros for gb_primWriteWord64OffAddr make gcc crash because not enough registers are available.
Hence the handcoded variant below:

%%[99
PRIM Word64 gb_primReadWord64OffAddr( Word64* ptr, GB_Word off )
{					
	return ptr[ off ] ;																						
}																															
																															
PRIM GB_Word gb_primWriteWord64OffAddr( Word32* ptr, GB_Word off, Word64 val )
{	
	ptr += (off << 1) ;
	Word32* p = (Word32*)(&val) ;
	*ptr++ = *p++ ;
	*ptr   = *p   ;
	return (GB_Word)gb_Unit ;																								
}																															
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exiting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This version also prints a stacktrace

%%[96.gb_primExitWith
PRIM GB_Word gb_primExitWith( GB_Word exitCode )
{
	// gb_exit( GB_GBInt2Int( exitCode ) ) ;
	gb_exit( exitCode ) ;
	return exitCode ; // for now
}
%%]

Whereas this is left to the runtime wrapper (see UHC.Prelude.ehcRunMain) in this version

%%[99 -96.gb_primExitWith
PRIM GB_Word gb_primExitWith( GB_Word exitCode )
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
PRIM GB_NodePtr gb_primShowInt( GB_Int intNd )
{
	char buf[sizeof(GB_Word)*10] ;
	char *s = buf ;
	// int i = GB_GBInt2Int(intNd) ;
	int i = (intNd) ;
	if ( i < 0 )
	{
		i = -i ;
		*(s++) = '-' ;
	}
	sprintf( s, "%d" , i ) ;
	
  	IF_GB_TR_ON(3,printf("gb_primShowInt s(%d) %s\n", strlen(buf), buf ););
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}
%%]

%%[97

PRIM GB_NodePtr gb_primShowFloat( Float w )
{
	char buf[sizeof(Float)*10] ;
	char *s = buf ;
	sprintf( s, "%f", w ) ;
	
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}

PRIM GB_NodePtr gb_primShowDouble( Double w )
{
	char buf[sizeof(Double)*10] ;
	char *s = buf ;
	sprintf( s, "%lf", w ) ;
	
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
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

PRIM GB_NodePtr gb_primThrowException( GB_Word exc )
{
	return gb_intl_throwException( exc ) ;
}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% I/O: MutVar: mutable variables for a State
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM GB_NodePtr gb_primNewMutVar( GB_Word init, GB_Word state )
{
	GB_NodePtr mutVar ;
	GB_NodePtr res ;
	GB_GC_SafeEnter ;
	GB_GC_Safe2(init,state) ;
	GB_GC_Safe1_Zeroed(mutVar) ;
	
	// printf("gb_primNewMutVar\n") ;

	GB_MkTupNode1_In(mutVar,init) ;
	GB_MkTupNode2_In(res,state,mutVar) ;
	
	GB_GC_SafeLeave ;
	return res ;
}

PRIM GB_NodePtr gb_primReadMutVar( GB_NodePtr mutVar, GB_Word state )
{
	GB_NodePtr res ;
	GB_GC_SafeEnter ;
	GB_GC_Safe2(mutVar,state) ;

	// printf("gb_primReadMutVar\n") ;

	GB_MkTupNode2_In(res,state,mutVar->content.fields[0]) ;
	
	GB_GC_SafeLeave ;
	return res ;
}

PRIM GB_Word gb_primWriteMutVar( GB_NodePtr mutVar, GB_Word newVal, GB_Word state )
{
	// printf("gb_primWriteMutVar\n") ;

	mutVar->content.fields[0] = newVal ;
	return state ;
}

PRIM GB_Word gb_primSameMutVar( GB_Word v1, GB_Word v2 )
{
	if ( v1 == v2 )
		return gb_True ;
	else
		return gb_False ;
}

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
%%]
PRIM GB_NodePtr gb_primGetProgArgv( )
{
	GB_NodePtr res ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(res) ;
	GB_MkListNil( res ) ;
	
	int i ;
	for ( i = rtsArgC - 1 ; i >= 0 ; i-- ) {
		GB_NodePtr n1, n2 ;
		n2 = gb_primCStringToString( rtsArgV[i] ) ;
		GB_MkListCons(n1,n2,res) ;
		res = n1 ;
	}
	
	GB_GC_SafeLeave ;
	return res ;
}

%%[99
%%]
PRIM GB_Word gb_primGetArgC()
{
	return Cast(GB_Word,primGetArgC()) ;
}

PRIM GB_Word gb_primGetArgVAt( GB_Word argc )
{
	return Cast(GB_Word,primGetArgVAt(argc)) ;
}

