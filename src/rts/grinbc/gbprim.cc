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
PRIM GB_Word gb_NoSuchThing
	= GB_MkConEnumNodeAsTag( 6 ) ;
PRIM GB_Word gb_PermissionDenied
	= GB_MkConEnumNodeAsTag( 7 ) ;
PRIM GB_Word gb_ResourceBusy
	= GB_MkConEnumNodeAsTag( 8 ) ;
PRIM GB_Word gb_ResourceExhausted
	= GB_MkConEnumNodeAsTag( 9 ) ;
PRIM GB_Word gb_UserError
	= GB_MkConEnumNodeAsTag( 10 ) ;
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
PRIM GB_Float gb_primIntToFloat( GB_Int x )
{
	// GB_NodePtr n ;
	// GB_NodeAlloc_Float_In(n) ;
	// n->content.flt = (float)GB_GBInt2Int(x) ;
	// n->content.flt = (float)x ;
	return Cast(GB_Float,x) ;
}

PRIM GB_Float gb_primDoubleToFloat( GB_Double /*nd*/ x )
{
	//GB_NodePtr nf ;
	//GB_NodeAlloc_Float_In(nf) ;
	//nf->content.flt = nd->content.dbl ;
	//return nf ;
	return Cast(GB_Float,x) ;
}

PRIM GB_Double gb_primFloatToDouble( GB_Float x )
{
	//GB_NodePtr nd ;
	//GB_NodeAlloc_Double_In(nd) ;
	//nd->content.dbl = nf->content.flt ;
	//return nd ;
	return Cast(GB_Double,x) ;
}

PRIM GB_Double gb_primIntToDouble( GB_Int x )
{
	// GB_NodePtr n ;
	// GB_NodeAlloc_Double_In(n) ;
	// n->content.dbl = (double)GB_GBInt2Int(x) ;
	// n->content.dbl = (double)x ;
	// return n ;
	return Cast(GB_Double,x) ;
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
PRIM GB_Word gb_primEqFloat( GB_Float x, GB_Float y )
{
	return GB_CmpBasic(x,y, gb_False, gb_True, gb_False ) ;
}

PRIM GB_Word gb_primCmpFloat( GB_Float x, GB_Float y )
{
	return GB_CmpBasic(x,y, gb_LT, gb_EQ, gb_GT ) ;
}

PRIM GB_Float gb_primAddFloat( GB_Float x, GB_Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Add_In(n,x,y) ;
	// return n ;
	return x + y ;
}

PRIM GB_Float gb_primSubFloat( GB_Float x, GB_Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Sub_In(n,x,y) ;
	// return n ;
	return x - y ;
}

PRIM GB_Float gb_primMulFloat( GB_Float x, GB_Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Mul_In(n,x,y) ;
	// return n ;
	return x * y ;
}

PRIM GB_Float gb_primDivFloat( GB_Float x, GB_Float y )
{
	// GB_NodePtr n ;
	// GB_Float_Div_In(n,x,y) ;
	// return n ;
	return x / y ;
}

PRIM GB_Float gb_primNegFloat( GB_Float x )
{
	// GB_NodePtr n ;
	// GB_Float_Neg_In(n,x) ;
	// return n ;
	return -x ;
}

PRIM GB_Word gb_primIsNaNFloat( GB_Float x )
{
	if ( isnan(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsDenormalizedFloat( GB_Float x )
{
	if ( ! isnormal(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsInfiniteFloat( GB_Float x )
{
	if ( isinf(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsNegativeZeroFloat( GB_Float x )
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
PRIM GB_Word gb_primEqDouble( GB_Double x, GB_Double y )
{
	return GB_CmpBasic(x,y, gb_False, gb_True, gb_False ) ;
}

PRIM GB_Word gb_primCmpDouble( GB_Double x, GB_Double y )
{
	return GB_CmpBasic(x,y, gb_LT, gb_EQ, gb_GT ) ;
}

PRIM GB_Double gb_primAddDouble( GB_Double x, GB_Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Add_In(n,x,y) ;
	// return n ;
	return x + y ;
}

PRIM GB_Double gb_primSubDouble( GB_Double x, GB_Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Sub_In(n,x,y) ;
	// return n ;
	return x - y ;
}

PRIM GB_Double gb_primMulDouble( GB_Double x, GB_Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Mul_In(n,x,y) ;
	// return n ;
	return x * y ;
}

PRIM GB_Double gb_primDivDouble( GB_Double x, GB_Double y )
{
	// GB_NodePtr n ;
	// GB_Double_Div_In(n,x,y) ;
	// return n ;
	return x / y ;
}

PRIM GB_Double gb_primNegDouble( GB_Double x )
{
	// GB_NodePtr n ;
	// GB_Double_Neg_In(n,x) ;
	// return n ;
	return -x ;
}

PRIM GB_Word gb_primIsNaNDouble( GB_Double x )
{
	if ( isnan(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsDenormalizedDouble( GB_Double x )
{
	if ( ! isnormal(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsInfiniteDouble( GB_Double x )
{
	if ( isinf(x) )
		return gb_True ;
	else
		return gb_False ;
}

PRIM GB_Word gb_primIsNegativeZeroDouble( GB_Double x )
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
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,(Word_SizeInBits-GB_Word_SizeOfWordTag),Int,Int,GB_Word)
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
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,8,Int8,Int8,GB_Word)
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Int16,Int16)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Int16,Int16,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,16,Int16,Int16,GB_Word)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Int32,Int32)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Int32,Int32,GB_Word)

%%]

%%[99
#ifdef USE_32_BITS
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(gb_,32,Int32,Int32,GB_Word)
#else
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,32,Int32,Int32,GB_Word)
#endif
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Int64,Int64)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Int64,Int64,GB_Word)
INTLIKE_ARITH_PRIMS_CODE(gb_,Int64,Int64,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_CODE(gb_,64,Int64,Int64,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(gb_,64,Int64,Int64,GB_Word)
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
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,(Word_SizeInBits-GB_Word_SizeOfWordTag),Word,Word,GB_Word)
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
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,8,Word8,Word8,GB_Word)
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Word16,Word16)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Word16,Word16,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,16,Word16,Word16,GB_Word)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Word32,Word32)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Word32,Word32,GB_Word)

%%]

%%[99
#ifdef USE_32_BITS
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(gb_,32,Word32,Word32,GB_Word)
#else
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE2(gb_,32,Word32,Word32,GB_Word)
#endif
%%]

%%[97
INTLIKE_BOUNDED_PRIMS_CODE(gb_,Word64,Word64)
INTLIKE_INT_CONVERSION_PRIMS_CODE(gb_,Word64,Word64,GB_Word)
INTLIKE_ARITH_PRIMS_CODE(gb_,Word64,Word64,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
%%]

%%[99
INTLIKE_BITS_PRIMS_CODE(gb_,64,Word64,Word64,gb_False,gb_True,gb_LT,gb_EQ,gb_GT,GB_Word)
INTLIKE_BITS_PRIMS_BITSIZE_DPD_CODE1(gb_,64,Word64,Word64,GB_Word)
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
  	IF_GB_TR_ON(3,printf("gb_primCStringToString1Char1 %x:'%s'[%d]\n", s, s, GB_GBInt2Int(goff) ););
	if ( c ) {
		GB_MkCFunNode2In(n2,&gb_primCStringToString1Char,s,GB_Int_Add(goff,GB_Int1)) ;
		GB_MkListCons(n,GB_Int2GBInt(c),n2) ;
	} else {
  		GB_MkListNil(n) ;
	}
  	IF_GB_TR_ON(3,printf("gb_primCStringToString1Char2 n %x\n", n ););
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
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit1 n %x\n", n ););
%%[[8
	gb_listForceEval( &n, &sz ) ;
%%][96
	GB_PassExc_GCSafe( gb_listForceEval( &n, &sz ) ) ;
%%]]
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit2 n %x\n", n ););
	GB_List_Iterate(n,sz,{buf[bufInx++] = GB_GBInt2Int(GB_List_Head(n));}) ;
  	IF_GB_TR_ON(3,printf("gb_primTraceStringExit3 n %x\n", n ););
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

Whereas this is left to the runtime wrapper (see EHC.Prelude.ehcRunMain) in this version

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
#	if USE_64_BITS
		sprintf( s, "%ld"
#	else
		sprintf( s, "%d"
#	endif
				, i ) ;
	
  	IF_GB_TR_ON(3,printf("gb_primShowInt s(%d) %s\n", strlen(buf), buf ););
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}
%%]

%%[97

PRIM GB_NodePtr gb_primShowFloat( GB_Float w )
{
	char buf[sizeof(GB_Float)*10] ;
	char *s = buf ;
	sprintf( s, "%f", w ) ;
	
	GB_NodePtr n ;
	int sz = strlen(buf) + 1 ; // ??? why +1
	GB_NodeAlloc_ByteArray_In( sz, n ) ;
	memcpy( n->content.bytearray.ptr, buf, sz ) ;
	
  	return gb_primByteArrayToString1Char( n, GB_Int0 ) ;
}

PRIM GB_NodePtr gb_primShowDouble( GB_Double w )
{
	char buf[sizeof(GB_Double)*10] ;
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
%%% I/O: auxiliary functions needed in the implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98

GB_NodePtr gb_throwChanInteractionException( GB_NodePtr chan, char* strErr )
{
	GB_NodePtr ioe_handle ;
	GB_NodePtr ioe_filename ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(chan) ;
	GB_GC_Safe2_Zeroed(ioe_handle,ioe_filename) ;
	
	GB_MkMaybeJust( ioe_filename, chan->content.chan.name ) ;
	GB_MkMaybeJust( ioe_handle, chan ) ;

	GB_GC_SafeLeave ;
	return gb_intl_throwIOErrorFromPrim( ioe_handle, gb_EOF, ioe_filename, strErr ) ;
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
	GB_GC_SafeEnter ;
	GB_GC_Safe1(chan) ;
	
	// printf( "%d ", feof( f ) ) ;
	GB_PassExc_GCSafe( gb_getChanEOFOrThrowExc( chan, throwExcForEOF, isEof ) ) ;
	if ( *isEof ) {
		c == EOF ;
	} else {
		c = getc( f ) ;
		if ( c == EOF ) {
			GB_PassExc_GCSafe( gb_getChanEOFOrThrowExc( chan, throwExcForEOF, isEof ) ) ;
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
	GB_GC_SafeLeave ;
	return NULL ;
}


GB_NodePtr gb_ThrowWriteChanError( GB_NodePtr chan )
{
	GB_NodePtr ioe_handle ;
	GB_Word    ioe_type ;
	GB_NodePtr ioe_filename ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(chan) ;
	GB_GC_Safe2_Zeroed(ioe_handle,ioe_filename) ;

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
	
	GB_GC_SafeLeave ;
	return gb_intl_throwIOErrorFromPrim( ioe_handle, ioe_type, ioe_filename, strerror( errno ) ) ;
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

	GB_MkTupNode2_In(res,state,mutVar->content.fields[0]) ;
	
	GB_GC_SafeLeave ;
	return res ;
}

PRIM GB_Word gb_primWriteMutVar( GB_NodePtr mutVar, GB_Word newVal, GB_Word state )
{
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
%%% I/O: basic primitives
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

PRIM GB_Word gb_primHFileno( GB_NodePtr chan )
{
	return ( fileno(chan->content.chan.file) ) ;
}

PRIM GB_Word gb_primEqHandle( GB_NodePtr chan1, GB_NodePtr chan2 )
{
	return ( fileno(chan1->content.chan.file) == fileno(chan2->content.chan.file) ? gb_True : gb_False ) ;
}

PRIM GB_Word gb_primHEqFileno( GB_NodePtr chan, GB_Word fno )
{
	return ( fno == fileno(chan->content.chan.file) ? gb_True : gb_False ) ;
}

PRIM GB_NodePtr gb_primOpenFileOrStd( GB_NodePtr nmNd, GB_Word modeEnum, GB_NodePtr mbHandleNr )   
{
    /* mbHandleNr to be used only for std{in,out,err}, ignoring the opening mode. */
	int nmSz = 0 ;
	GB_Word mbHandleNrFromJust = 0 ;
	GB_GC_SafeEnter ;
	GB_GC_Safe4(mbHandleNrFromJust,nmNd,modeEnum,mbHandleNr) ;
	GB_PassExc_GCSafe( gb_listForceEval( &nmNd, &nmSz ) ) ;
	char* nm = alloca( nmSz + 1 ) ;
	GB_PassExc_GCSafe( gb_copyCStringFromEvalString( nm, nmNd, nmSz ) ) ;	
	nm[ nmSz ] = 0 ;

	GB_PassExc_GCSafe( mbHandleNr = Cast( GB_NodePtr, gb_eval( Cast(GB_Word,mbHandleNr) ) ) ) ;
	Bool mbHandleNrIsJust = False ;
	if ( GB_NH_Fld_Tag(mbHandleNr->header) == GB_Tag_Maybe_Just ) {
		mbHandleNrIsJust = True ;
		GB_PassExc_GCSafe( mbHandleNrFromJust = gb_eval( mbHandleNr->content.fields[0] ) ) ;
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
		GB_GC_Safe2_Zeroed(ioe_handle,ioe_filename) ;

		GB_MkMaybeNothing( ioe_handle ) ;
		GB_MkMaybeJust( ioe_filename, nmNd ) ;
		
		switch ( errno ) {
			case ENODEV :
			case ENOENT :
				ioe_type = gb_NoSuchThing ;
				break ;
			case EPERM   :
			case EACCES  :
			case ENOTDIR :
			case EMFILE :
				ioe_type = gb_PermissionDenied ;
				break ;
			case EBUSY :
				ioe_type = gb_ResourceBusy ;
				break ;
			default :
				ioe_type = gb_PermissionDenied ;
				break ;
		}

		GB_GC_SafeLeave ;
		return gb_intl_throwIOErrorFromPrim( ioe_handle, ioe_type, ioe_filename, strerror( errno ) ) ;
	}
	
	GB_NodePtr chan ;
	GB_NodeAlloc_Chan_In(chan) ;
	chan->content.chan.file = f ;
	chan->content.chan.name = nmNd ;
	chan->content.chan.isText = isText ;
	
	GB_GC_SafeLeave ;
	return chan ;
}

PRIM GB_NodePtr gb_primHClose( GB_NodePtr chan )
{
	fclose(chan->content.chan.file) ;
	return gb_Unit ;
}

PRIM GB_NodePtr gb_primHFlush( GB_NodePtr chan )
{
	fflush( chan->content.chan.file ) ;
	return gb_Unit ;
}

PRIM GB_Word gb_primHGetChar( GB_NodePtr chan )
{
	Bool isEof ;
	int c ;
	GB_PassExc_CastAsWord( gb_ChanGetChar( chan, True, &isEof, &c ) ) ;
	// return Cast(GB_NodePtr,GB_Int2GBInt(c)) ;
	return Cast(GB_Word,c) ;
}

PRIM GB_NodePtr gb_primHPutChar( GB_NodePtr chan, GB_Word c )
{	
	// int c2 = putc( GB_GBInt2Int(c), chan->content.chan.file ) ;
	int c2 = putc( (c), chan->content.chan.file ) ;
	if (c2 == EOF) {
		GB_PassExc( gb_ThrowWriteChanError( chan ) ) ;
	}
	return gb_Unit ;
}


%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% I/O: additional primitives for enhanced performance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98

PRIM GB_NodePtr gb_primHGetContents( GB_NodePtr chan )
{
	Bool isEof ;
	GB_NodePtr res, n ;
	GB_GC_SafeEnter ;
	GB_GC_Safe1(chan) ;
	GB_GC_Safe2_Zeroed(n,res) ;

	int c ;
	GB_PassExc_GCSafe( gb_ChanGetChar( chan, False, &isEof, &c ) ) ;
	if ( isEof ) {
		GB_MkListNil( res ) ;
	} else if ( c == EOF ) {
		GB_GC_SafeLeave ;
		return gb_throwChanInteractionException( chan, strerror( errno ) ) ;
	} else {
		GB_MkCFunNode1In(n,&gb_primHGetContents,chan) ;
		GB_MkListCons(res,GB_Int2GBInt(c),n) ;
	}
	
	GB_GC_SafeLeave ;
	return res ;
}

PRIM GB_NodePtr gb_primHPutByteArray( GB_NodePtr chan, GB_NodePtr a )
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
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
%%]

%%[99
PRIM GB_Word gb_primGetArgC()
{
	return Cast(GB_Word,primGetArgC()) ;
}

PRIM GB_Word gb_primGetArgVAt( GB_Word argc )
{
	return Cast(GB_Word,primGetArgVAt(argc)) ;
}

%%]
