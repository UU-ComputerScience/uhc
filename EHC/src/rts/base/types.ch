%%[8
#ifndef __BASE_TYPES_H__
#define __BASE_TYPES_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Very basic types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// identity macro
#define ID(x)		x
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Words and Pointers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// Requirement: sizeof(GB_Ptr) == sizeof(GB_Word)

typedef Word  GB_Word ;
typedef SWord GB_SWord ;

#define GB_Word_SizeInBits		Word_SizeInBits

typedef GB_Word* 	GB_WordPtr ;
typedef GB_WordPtr 	GB_Ptr ;
typedef GB_Ptr*  	GB_PtrPtr ;
typedef uint8_t* 	GB_BytePtr ;
typedef GB_SWord 	GB_Int ;
typedef HalfWord 	GB_NodeTag ;
typedef HalfWord 	GB_NodeSize ;
typedef uint8_t  	GB_Byte ;
%%]


%%[8
#define GB_DerefCast(ty,x)				(*Cast(ty*,x))
#define GB_Deref(x)						GB_DerefCast(GB_Word,x)	
#define GB_Word2Bytes(w)				Cast(GB_Byte,Bits_ExtrFromToSh(GB_Word,w,0,7)) \
										, Cast(GB_Byte,Bits_ExtrFromToSh(GB_Word,w,8,15)) \
										, Cast(GB_Byte,Bits_ExtrFromToSh(GB_Word,w,16,23)) \
										, Cast(GB_Byte,Bits_ExtrFromSh(GB_Word,w,24))
%%]


%%[97
#if USE_64_BITS
typedef Word 	GB_Float 	;
#else
typedef Float 	GB_Float 	;
#endif
typedef Double 	GB_Double 	;
%%]


%%[8
typedef union GB_WordEquiv {
  Word 		w ;
%%[[97
#if USE_64_BITS
#if LITTLEENDIAN
  struct {
    Float 	lo ;
    Float 	hi ;
  } __attribute__ ((__packed__)) f ;
#else /* LITTLEENDIAN */
  struct {
    Float 	hi ;
    Float 	lo ;
  } __attribute__ ((__packed__)) f ;
  Float		f ;
#endif
#else /* USE_64_BITS */
  struct {
    Float 	lo ;
  } f ;
#endif
%%]]
} GB_WordEquiv ;
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GB_Ptr and GB_Int are tagged values, stored in a GB_Word, GB_Ints are shifted left
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Word_SizeOfWordTag 		1
#define GB_Word_TagMask 			1
#define GB_Word_IntMask 			(~ GB_Word_TagMask)
#define GB_Word_TagInt 				1
#define GB_Word_TagPtr 				0

#define GB_Int_ShiftPow2			Bits_Pow2(GB_Int,GB_Word_SizeOfWordTag)

#define GB_Word_IsInt(x)			((x) & GB_Word_TagMask)
#define GB_Word_IsPtr(x)			(! GB_Word_IsInt(x))
%%]




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8


// Node header layout

#if USE_64_BITS
#define GB_NodeHeader_Size_BitSz		32
#define GB_NodeHeader_TagCat_BitSz		2
#define GB_NodeHeader_GC_BitSz			2
#define GB_NodeHeader_Tag_BitSz			26
#define GB_NodeHeader_NdEv_BitSz		2
#else
#define GB_NodeHeader_Size_BitSz		16
#define GB_NodeHeader_TagCat_BitSz		2
#define GB_NodeHeader_GC_BitSz			2
#define GB_NodeHeader_Tag_BitSz			10
#define GB_NodeHeader_NdEv_BitSz		2
#endif



// Node header

#if NODEHEADER_VIA_STRUCT
typedef struct GB_NodeHeader {
  unsigned 	size 		: GB_NodeHeader_Size_BitSz 		;			/* size, incl header, in words 					*/
  unsigned 	tagCateg 	: GB_NodeHeader_TagCat_BitSz 	;			/* kind of tag, dpd on needsEval 				*/
  unsigned 	gc 			: GB_NodeHeader_GC_BitSz		;			/* garbage collection info (unused currently)	*/
  unsigned 	tag 		: GB_NodeHeader_Tag_BitSz 		;			/* tag, or additional size dpd on tagCateg 		*/
  unsigned 	needsEval 	: GB_NodeHeader_NdEv_BitSz 		;			/* possibly needs eval? 						*/
} GB_NodeHeader ;

#define GB_NH_Fld_Size(x)				((x)->size)
#define GB_NH_Fld_TagCat(x)				((x)->tagCateg)
#define GB_NH_Fld_GC(x)					((x)->gc)
#define GB_NH_Fld_Tag(x)				((x)->tag)
#define GB_NH_Fld_NdEv(x)				((x)->needsEval)

#else

typedef GB_Word GB_NodeHeader ;

/*
#define GB_NH_Tag_Shift					0			
#define GB_NH_GC_Shift					(GB_NH_Tag_Shift + GB_NodeHeader_Tag_BitSz)
#define GB_NH_TagCat_Shift				(GB_NH_GC_Shift + GB_NodeHeader_GC_BitSz)
#define GB_NH_NdEv_Shift				(GB_NH_TagCat_Shift + GB_NodeHeader_TagCat_BitSz)
#define GB_NH_Size_Shift				(GB_NH_NdEv_Shift + GB_NodeHeader_NdEv_BitSz)
#define GB_NH_Full_Shift				(GB_NH_Size_Shift + GB_NodeHeader_Size_BitSz)
*/

// NdEv must be in the 2 least significant bits, as the rest will be interpreted as a 4 byte aligned forwarding pointer during GC (when USE_EHC_MM is defined)
#define GB_NH_NdEv_Shift				0
#define GB_NH_Tag_Shift					(GB_NH_NdEv_Shift + GB_NodeHeader_NdEv_BitSz)		
#define GB_NH_GC_Shift					(GB_NH_Tag_Shift + GB_NodeHeader_Tag_BitSz)
#define GB_NH_TagCat_Shift				(GB_NH_GC_Shift + GB_NodeHeader_GC_BitSz)
#define GB_NH_Size_Shift				(GB_NH_TagCat_Shift + GB_NodeHeader_TagCat_BitSz)
#define GB_NH_Full_Shift				(GB_NH_Size_Shift + GB_NodeHeader_Size_BitSz)

#define GB_NH_MkFld_Size(x)				(Cast(GB_Word,x)<<GB_NH_Size_Shift)
#define GB_NH_MkFld_NdEv(x)				(Cast(GB_Word,x)<<GB_NH_NdEv_Shift)
#define GB_NH_MkFld_TagCat(x)			(Cast(GB_Word,x)<<GB_NH_TagCat_Shift)
#define GB_NH_MkFld_GC(x)				(Cast(GB_Word,x)<<GB_NH_GC_Shift)
#define GB_NH_MkFld_Tag(x)				(Cast(GB_Word,x)<<GB_NH_Tag_Shift)

#define GB_NH_FldBits(x,f,t)			Bits_ExtrFromToSh(GB_Word,x,f,t)
#define GB_NH_FldBitsFr(x,f)			Bits_ExtrFromSh(GB_Word,x,f)
#define GB_NH_FldMask(f,t)				Bits_MaskFromTo(GB_Word,f,t)
#define GB_NH_FldMaskFr(f)				Bits_MaskFrom(GB_Word,f)

/*
#define GB_NH_Mask_Size					GB_NH_FldMaskFr(GB_NH_Size_Shift)
#define GB_NH_Mask_NdEv					GB_NH_FldMask(GB_NH_NdEv_Shift,GB_NH_Size_Shift-1)
#define GB_NH_Mask_TagCat				GB_NH_FldMask(GB_NH_TagCat_Shift,GB_NH_NdEv_Shift-1)
#define GB_NH_Mask_GC					GB_NH_FldMask(GB_NH_GC_Shift,GB_NH_TagCat_Shift-1)
#define GB_NH_Mask_Tag					GB_NH_FldMask(GB_NH_Tag_Shift,GB_NH_GC_Shift-1)
*/

#define GB_NH_Mask_Size					GB_NH_FldMaskFr(GB_NH_Size_Shift)
#define GB_NH_Mask_TagCat				GB_NH_FldMask(GB_NH_TagCat_Shift,GB_NH_Size_Shift-1)
#define GB_NH_Mask_GC					GB_NH_FldMask(GB_NH_GC_Shift,GB_NH_TagCat_Shift-1)
#define GB_NH_Mask_Tag					GB_NH_FldMask(GB_NH_Tag_Shift,GB_NH_GC_Shift-1)
#define GB_NH_Mask_NdEv					GB_NH_FldMask(GB_NH_NdEv_Shift,GB_NH_Tag_Shift-1)

#define GB_NH_SetFld(h,m,x)				(((h) & (~ (m))) | (x))
#define GB_NH_SetFld_Size(h,x)			GB_NH_SetFld(h,GB_NH_Mask_Size,GB_NH_MkFld_Size(x))
#define GB_NH_SetFld_NdEv(h,x)			GB_NH_SetFld(h,GB_NH_Mask_NdEv,GB_NH_MkFld_NdEv(x))
#define GB_NH_SetFld_TagCat(h,x)		GB_NH_SetFld(h,GB_NH_Mask_TagCat,GB_NH_MkFld_TagCat(x))
#define GB_NH_SetFld_GC(h,x)			GB_NH_SetFld(h,GB_NH_Mask_GC,GB_NH_MkFld_GC(x))
#define GB_NH_SetFld_Tag(h,x)			GB_NH_SetFld(h,GB_NH_Mask_Tag,GB_NH_MkFld_Tag(x))

/*
#define GB_NH_Fld_Size(x)				GB_NH_FldBitsFr(x,GB_NH_Size_Shift)
#define GB_NH_Fld_NdEv(x)				GB_NH_FldBits(x,GB_NH_NdEv_Shift,GB_NH_Size_Shift-1)
#define GB_NH_Fld_TagCat(x)				GB_NH_FldBits(x,GB_NH_TagCat_Shift,GB_NH_NdEv_Shift-1)
#define GB_NH_Fld_GC(x)					GB_NH_FldBits(x,GB_NH_GC_Shift,GB_NH_TagCat_Shift-1)
#define GB_NH_Fld_Tag(x)				GB_NH_FldBits(x,GB_NH_Tag_Shift,GB_NH_GC_Shift-1)
*/

#define GB_NH_Fld_Size(x)				GB_NH_FldBitsFr(x,GB_NH_Size_Shift)
#define GB_NH_Fld_TagCat(x)				GB_NH_FldBits(x,GB_NH_TagCat_Shift,GB_NH_Size_Shift-1)
#define GB_NH_Fld_GC(x)					GB_NH_FldBits(x,GB_NH_GC_Shift,GB_NH_TagCat_Shift-1)
#define GB_NH_Fld_Tag(x)				GB_NH_FldBits(x,GB_NH_Tag_Shift,GB_NH_GC_Shift-1)
#define GB_NH_Fld_NdEv(x)				GB_NH_FldBits(x,GB_NH_NdEv_Shift,GB_NH_Tag_Shift-1)

#endif
%%]




Evaluation need: Yes, No, or blackhole (no evaluation, but when requested indicates a loop)

%%[8
#define GB_NodeNdEv_Fwd					3			/* forwarding, only used during GC								*/
#define GB_NodeNdEv_BlH					2			/* black hole													*/
#define GB_NodeNdEv_Yes					1
#define GB_NodeNdEv_No					0
%%]

Node categories. Two groups, the first require evaluation, the second not.

%%[8
#define GB_NodeTagCat_Fun				0			/* saturated function call closure 		*/
#define GB_NodeTagCat_App				1			/* general purpose application 			*/
#define GB_NodeTagCat_Ind				2			/* indirection 							*/
#define GB_NodeTagCat_CFun				3			/* saturated C function call closure 	*/

#define GB_NodeTagCat_Con				0			/* data, constructor 											*/
#define GB_NodeTagCat_PAp				1			/* partial application, tag is size of missing 					*/
%%[[95
#define GB_NodeTagCat_Intl				3			/* other internal structures, further described by tag 			*/
%%]]
%%]


%%[95
#define GB_NodeTag_Intl_Malloc			0			// Internal node: A malloc'ed ptr, requiring finalisation
#define GB_NodeTag_Intl_Malloc2			1			// Internal node: A malloc'ed ptr with length info, requiring finalisation
%%]

%%[97
#define GB_NodeTag_Intl_Float			2			// Internal node: Float
#define GB_NodeTag_Intl_Double			3			// Internal node: Double
%%]

%%[97
// #if USE_GMP
#define GB_NodeTag_Intl_GMP_mpz			4			// Internal node: A GMP mpz_t 
#define GB_NodeTag_Intl_GMP_intl		5			// Internal node: GMP internal allocated 
// #endif
%%]

%%[98
#define GB_NodeTag_Intl_Chan			6			// Internal node: GB_Chan
%%]

%%[8
#if NODEHEADER_VIA_STRUCT
#define GB_NH_NrFlds(h)					((h).size-1)
#else
#define GB_NH_NrFlds(h)					(GB_NH_Fld_Size(h)-1)
#endif

#define GB_Node_NrFlds(n)				GB_NH_NrFlds((n)->header)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if NODEHEADER_VIA_STRUCT
#define GB_MkHeader(sz,ev,cat,tg)			{sz, ev, cat, 0, tg}
#else
#define GB_MkHeader(sz,ev,cat,tg)			(GB_NH_MkFld_Size(sz) | GB_NH_MkFld_NdEv(ev) | GB_NH_MkFld_TagCat(cat) | GB_NH_MkFld_GC(0) | GB_NH_MkFld_Tag(tg))
#endif

#define GB_MkFunHeader(nArg)				GB_MkHeader((nArg)+2, GB_NodeNdEv_Yes, GB_NodeTagCat_Fun, 0)
#define GB_MkCFunHeader(nArg)				GB_MkHeader((nArg)+2, GB_NodeNdEv_Yes, GB_NodeTagCat_CFun, 0)
#define GB_MkCAFHeader						GB_MkFunHeader(0)
#define GB_MkConHeader(sz,tg)				GB_MkHeader((sz)+1, GB_NodeNdEv_No, GB_NodeTagCat_Con, tg)
#define GB_MkAppHeader(nArg)				GB_MkHeader((nArg)+2, GB_NodeNdEv_Yes, GB_NodeTagCat_App, 0)

#if USE_EHC_MM
#	define GB_MkConEnumNode(tg)				{ GB_MkConHeader(1,tg), 0 }	// place for forwarding pointer
#else
#	define GB_MkConEnumNode(tg)				{ GB_MkConHeader(0,tg) }
#endif
#define GB_MkConEnumNodeAsTag(tg)			(tg /* GB_Int2GBInt(tg) */)

#define GB_FillNodeFlds1(n,x1)				{                                (n)->content.fields[0] = Cast(GB_Word,x1);}
#define GB_FillNodeFlds2(n,x1,x2)			{GB_FillNodeFlds1(n,x1         );(n)->content.fields[1] = Cast(GB_Word,x2);}
#define GB_FillNodeFlds3(n,x1,x2,x3)		{GB_FillNodeFlds2(n,x1,x2      );(n)->content.fields[2] = Cast(GB_Word,x3);}
#define GB_FillNodeFlds4(n,x1,x2,x3,x4)		{GB_FillNodeFlds3(n,x1,x2,x3   );(n)->content.fields[3] = Cast(GB_Word,x4);}
#define GB_FillNodeFlds5(n,x1,x2,x3,x4,x5)	{GB_FillNodeFlds4(n,x1,x2,x3,x4);(n)->content.fields[4] = Cast(GB_Word,x5);}

#define GB_FillNodeHdr(h,n)						{(n)->header = h;}
#define GB_FillConNodeN(n,sz,tg)				{GB_NodeHeader _h = GB_MkConHeader(GB_GC_MinAlloc_Field_Words(sz),tg); GB_FillNodeHdr(_h,n);}
#define GB_FillConNode0(n,tg)					{GB_NodeHeader _h = GB_MkConHeader(GB_GC_MinAlloc_Field_Words(0 ),tg); GB_FillNodeHdr(_h,n);}
#define GB_FillConNode1(n,tg,x1)				{GB_NodeHeader _h = GB_MkConHeader(1,tg); GB_FillNodeHdr(_h,n); GB_FillNodeFlds1(n,x1);}
#define GB_FillConNode2(n,tg,x1,x2)				{GB_NodeHeader _h = GB_MkConHeader(2,tg); GB_FillNodeHdr(_h,n); GB_FillNodeFlds2(n,x1,x2);}
#define GB_FillConNode3(n,tg,x1,x2,x3)			{GB_NodeHeader _h = GB_MkConHeader(3,tg); GB_FillNodeHdr(_h,n); GB_FillNodeFlds3(n,x1,x2,x3);}
#define GB_FillConNode4(n,tg,x1,x2,x3,x4)		{GB_NodeHeader _h = GB_MkConHeader(4,tg); GB_FillNodeHdr(_h,n); GB_FillNodeFlds4(n,x1,x2,x3,x4);}
#define GB_FillConNode5(n,tg,x1,x2,x3,x4,x5)	{GB_NodeHeader _h = GB_MkConHeader(5,tg); GB_FillNodeHdr(_h,n); GB_FillNodeFlds5(n,x1,x2,x3,x4,x5);}

#define GB_MkConNodeN(n,sz,tg)					{GB_NodeAlloc_In(GB_GC_MinAlloc_Node_Words(sz),n); GB_FillConNodeN(n,sz,tg); }
#define GB_MkConNodeN_Rooted(n,sz,tg)			{GB_MkConNodeN(n,sz,tg) ; GB_GC_RegisterRoot(n); }
#define GB_MkConNodeN_Fixed(n,sz,tg)			{GB_NodeAlloc_In_Fixed(GB_GC_MinAlloc_Node_Words(sz),n); GB_FillConNodeN(n,sz,tg); }
#define GB_MkConNode0(n,tg)						{GB_NodeAlloc_In(GB_GC_MinAlloc_Node_Words(0),n); GB_FillConNode0(n,tg); }
#define GB_MkConNode1(n,tg,x1)					{GB_NodeAlloc_In(2,n); GB_FillConNode1(n,tg,x1); }
#define GB_MkConNode2(n,tg,x1,x2)				{GB_NodeAlloc_In(3,n); GB_FillConNode2(n,tg,x1,x2); }
#define GB_MkConNode3(n,tg,x1,x2,x3)			{GB_NodeAlloc_In(4,n); GB_FillConNode3(n,tg,x1,x2,x3); }
#define GB_MkConNode4(n,tg,x1,x2,x3,x4)			{GB_NodeAlloc_In(5,n); GB_FillConNode4(n,tg,x1,x2,x3,x4); }
#define GB_MkConNode5(n,tg,x1,x2,x3,x4,x5)		{GB_NodeAlloc_In(6,n); GB_FillConNode5(n,tg,x1,x2,x3,x4,x5); }

#define GB_MkTupNode1_In(n,x1)				GB_MkConNode1(n,0,x1)
#define GB_MkTupNode2_In(n,x1,x2)			GB_MkConNode2(n,0,x1,x2)

#define GB_FillCFunNode0(n,f)				{GB_NodeHeader _h = GB_MkCFunHeader(0); GB_FillNodeHdr(_h,n);GB_FillNodeFlds1(n,f);}
#define GB_FillCFunNode1(n,f,x1)			{GB_NodeHeader _h = GB_MkCFunHeader(1); GB_FillNodeHdr(_h,n);GB_FillNodeFlds2(n,f,x1);}
#define GB_FillCFunNode2(n,f,x1,x2)			{GB_NodeHeader _h = GB_MkCFunHeader(2); GB_FillNodeHdr(_h,n);GB_FillNodeFlds3(n,f,x1,x2);}

#define GB_MkCFunNode0In(n,f)				{GB_NodeAlloc_In(2,n); GB_FillCFunNode0(n,f); }
#define GB_MkCFunNode1In(n,f,x1)			{GB_NodeAlloc_In(3,n); GB_FillCFunNode1(n,f,x1); }
#define GB_MkCFunNode2In(n,f,x1,x2)			{GB_NodeAlloc_In(4,n); GB_FillCFunNode2(n,f,x1,x2); }

#define GB_FillCafNode(n,f)					{GB_NodeHeader _h = GB_MkCAFHeader; GB_FillNodeHdr(_h,n);GB_FillNodeFlds1(n,f);}
#if USE_BOEHM_GC
#	define GB_MkCafNodeIn(n,f)				{GB_NodeAlloc_In_Fixed(2,n); GB_FillCafNode(n,f); }
#else
#	define GB_MkCafNodeIn(n,f)				{GB_NodeAlloc_In(2,n); GB_FillCafNode(n,f); }
#endif

#define GB_FillAppNode1(n,f,x1)				{GB_NodeHeader _h = GB_MkAppHeader(1); GB_FillNodeHdr(_h,n);GB_FillNodeFlds2(n,f,x1);}

#define GB_MkAppNode1In(n,f,x1)				{GB_NodeAlloc_In(3,n); GB_FillAppNode1(n,f,x1); }

#if USE_BOEHM_GC
#	define GB_MkExpNodeIn(n,sz)				GB_MkConNodeN_Fixed( n, GB_GC_MinAlloc_Field_Words(sz), 0 ) ;
#elif USE_EHC_MM
#	define GB_MkExpNodeIn(n,sz)				{ GB_MkConNodeN( n, GB_GC_MinAlloc_Field_Words(sz), 0 ); GB_GC_RegisterRoot(n); }
#else
#	define GB_MkExpNodeIn(n,sz)				GB_MkConNodeN( n, GB_GC_MinAlloc_Field_Words(sz), 0 )
#endif

%%]
extern GB_NodePtr gb_MkCAF( GB_BytePtr pc ) ;

%%[95
#define GB_NodeMallocSize					(EntierUpDivBy(sizeof(void*),sizeof(GB_Word)) + 1)
#define GB_NodeMallocSize_ByteArray			(EntierUpDivBy(sizeof(GB_ByteArray),sizeof(GB_Word)) + 1)

#define GB_MkMallocHeader					GB_MkHeader(GB_NodeMallocSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Malloc)
#define GB_MkMallocHeader_ByteArray			GB_MkHeader(GB_NodeMallocSize_ByteArray, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Malloc2)
%%]

%%[97
#define GB_NodeFloatSize					(EntierUpDivBy(sizeof(float),sizeof(GB_Word)) + 1)
#define GB_MkFloatHeader					GB_MkHeader(GB_NodeFloatSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Float)

#define GB_NodeDoubleSize					(EntierUpDivBy(sizeof(float),sizeof(GB_Word)) + 1)
#define GB_MkDoubleHeader					GB_MkHeader(GB_NodeDoubleSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Double)

%%]

%%[98
#define GB_NodeChanSize						(EntierUpDivBy(sizeof(GB_Chan),sizeof(GB_Word)) + 1)
#define GB_MkChanHeader						GB_MkHeader(GB_NodeChanSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Chan)
%%]

%%[8
#define GB_Node_ZeroFields(n)				{ memset( ((GB_NodePtr)n)->content.fields, 0, (GB_NH_Fld_Size(((GB_NodePtr)n)->header) << Word_SizeInBytes_Log) - sizeof(GB_NodeHeader) ) ; }
%%]

%%[99
#define GB_MkNode_Handle_GBHandle(n,chan)	GB_MkConNode1(n,2,chan)
%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Byte array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95
typedef struct GB_ByteArray {
  GB_Word	size ;
  void* 	ptr	;
} __attribute__ ((__packed__)) GB_ByteArray ;
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO Channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98
typedef struct GB_Chan {
  FILE*				file ;
  struct GB_Node*	name ;
  Bool				isText ;
} __attribute__ ((__packed__)) GB_Chan ;
%%]




%%[8
typedef struct GB_Node {
  GB_NodeHeader	header ;
  union {
    GB_Word 		fields[1] ;			/* size 1 is ok for a CAF, but not for other static node initializers */
%%[[95
    void*			ptr ;				/* when GB_NodeTag_Intl_Malloc */ /* will be replaced by following */
    GB_ByteArray	bytearray ;			/* when GB_NodeTag_Intl_Malloc2 */
%%]]
%%[[97
    float			flt ;				/* when GB_NodeTag_Intl_Float */
    double			dbl ;				/* when GB_NodeTag_Intl_Double */
#if USE_GMP
    // mpz_t			mpz ;				/* when GB_NodeTag_Intl_GMP_mpz */
#endif
%%]]
%%[[98
    GB_Chan			chan ;				/* when GB_NodeTag_Intl_Chan */
%%]]
  } __attribute__ ((__packed__)) content ;
} __attribute__ ((__packed__)) GB_Node, *GB_NodePtr ;

%%]





Per module info for GC.
Currently only used when USE_EHC_MM

%%[8
typedef struct GB_GC_Module {
	GB_BytePtr*		globalEntries ;				// global entry points into code
	Word			nrGlobalEntries ;			// (and the number of them)
	HalfWord*		cafGlobalEntryIndices ;		// which global entry points are cafs
	Word			nrCafGlobalEntryIndices ;
%%[[20
	GB_NodePtr*		expNode	;					// node holding the export table (all globals actually exported via export list of module)
%%]]
} GB_GC_Module ;
%%]



%%[8
#define IF_GB_TR_ON(l,x)		IF_TR_ON(l,if (gb_Opt_TraceSteps) { x })
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Tracing on?

%%[8
extern int gb_Opt_TraceSteps ;
%%]

Info flags

%%[8
extern int gb_Opt_Info ;
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Assume that sizeof(GrWord) == sizeof(GB_Word).
This should be ok and merged later on, but a check in it is currently part of the sanity check.
Size must be minimal 2 words to ensure enough space for an indirection pointer (plus the usual header in front).

The 'Fixed' variants allocate non-collectable.

%%[8
#if USE_BOEHM_GC

#	if TRACE || DUMP_INTERNALS
	extern GB_Ptr gb_HeapAlloc_Bytes_Traced( GB_Word nBytes ) ;
#		define GB_HeapAlloc_Bytes(nBytes)				gb_HeapAlloc_Bytes_Traced(nBytes)
#	else
#		define GB_HeapAlloc_Bytes(nBytes)				Cast(GB_Ptr,GC_MALLOC(nBytes))
#	endif

#	define GB_HeapAlloc_Words(nWords)					GB_HeapAlloc_Bytes((nWords) * sizeof(GB_Word))
#	define GB_HeapAlloc_Bytes_Fixed(nBytes)				Cast(GB_Ptr,GC_MALLOC_UNCOLLECTABLE(nBytes))
#	define GB_HeapAlloc_Words_Fixed(nWords)				GB_HeapAlloc_Bytes_Fixed((nWords) * sizeof(GB_Word))

#	if TRACE
#		define GB_GC_Maintained(x)						( Cast(GB_Ptr,x) >= gb_allocated_lowest_ptr && Cast(GB_Ptr,x) <= gb_allocated_highest_ptr )
#	else
#		define GB_GC_Maintained(x)						False
#	endif

#	define GB_GC_MinAlloc_Field_Words(szw)				(szw)
#	define GB_GC_MinAlloc_Field_Bytes(szb)				(szb)
#	define GB_GC_MinAlloc_Malloc(szb)					(szb)

#	define GB_GC_RegisterRoot(n)						// not necessary

#	define GB_GC_SafeEnter								// not necessary
#	define GB_GC_SafeLeave								// not necessary
#	define GB_GC_Safe1(nm1)								// not necessary
#	define GB_GC_Safe2(nm1,nm2)							// not necessary
#	define GB_GC_Safe3(nm1,nm2,nm3)						// not necessary
#	define GB_GC_Safe4(nm1,nm2,nm3,nm4)					// not necessary
#	define GB_GC_Safe1_Zeroed(nm1)						// not necessary
#	define GB_GC_Safe2_Zeroed(nm1,nm2)					// not necessary
#	define GB_GC_Safe3_Zeroed(nm1,nm2,nm3)				// not necessary
#	define GB_GC_Safe4_Zeroed(nm1,nm2,nm3,nm4)			// not necessary

#	define GB_GC_RegisterModule(m)						// not necessary

	extern void gb_Node_Finalize( void* p, void* cd ) ;
	extern void* gb_Dummy_Finalization_Proc ;
	extern void* gb_Dummy_Finalization_cd ;

	static inline Ptr gb_malloc( size_t sz ) { return malloc( sz ) ; }
	static inline void gb_free( Ptr p ) { return free( p ) ; }

#elif USE_EHC_MM

#	define GB_HeapAlloc_Bytes(nBytes)					Cast(GB_Ptr,mm_itf_alloc(nBytes))
#	define GB_HeapAlloc_Words(nWords)					GB_HeapAlloc_Bytes((nWords) * sizeof(GB_Word))
#	define GB_HeapAlloc_Bytes_Fixed(nBytes)				Cast(GB_Ptr,mm_itf_allocResident(nBytes))
#	define GB_HeapAlloc_Words_Fixed(nWords)				GB_HeapAlloc_Bytes_Fixed((nWords) * sizeof(GB_Word))

#	define GC_MALLOC(nBytes)							GB_HeapAlloc_Bytes(nBytes)
#	define GC_MALLOC_UNCOLLECTABLE(nBytes)				GB_HeapAlloc_Bytes_Fixed(nBytes)

#	if TRACE
#		define GB_GC_Maintained(x)						mm_plan.mutator->isMaintainedByGC( mm_plan.mutator, x )
#	else
#		define GB_GC_Maintained(x)						False
#	endif

#	define GB_GC_MinAlloc_Field_Words(szw)				(maxWord(szw,1))				// minimal for node: at least 1 field payload to allow for indirection/forwarding
#	define GB_GC_MinAlloc_Field_Bytes(szb)				(maxWord(szb,sizeof(Word)))		// minimal for node: at least 1 field payload to allow for indirection/forwarding
#	define GB_GC_MinAlloc_Malloc(szb)					(maxWord(szb,sizeof(Word)))		// minimal for malloc: at least a Word

#	define GB_GC_RegisterRoot(n)						{ GB_Node_ZeroFields(n) ; mm_itf_registerGCRoot( (WPtr)(&n) ) ; }

#	define GB_GC_SafeEnter								MM_LclRoot_EnterGrp
#	define GB_GC_SafeLeave								MM_LclRoot_LeaveGrp
#	define GB_GC_Safe1(nm1)								MM_LclRoot_EnterOne1(nm1)
#	define GB_GC_Safe2(nm1,nm2)							MM_LclRoot_EnterOne2(nm1,nm2)	
#	define GB_GC_Safe3(nm1,nm2,nm3)						MM_LclRoot_EnterOne3(nm1,nm2,nm3)
#	define GB_GC_Safe4(nm1,nm2,nm3,nm4)					MM_LclRoot_EnterOne4(nm1,nm2,nm3,nm4)
#	define GB_GC_Safe1_Zeroed(nm1)						MM_LclRoot_EnterOne1_Zeroed(nm1)
#	define GB_GC_Safe2_Zeroed(nm1,nm2)					MM_LclRoot_EnterOne2_Zeroed(nm1,nm2)	
#	define GB_GC_Safe3_Zeroed(nm1,nm2,nm3)				MM_LclRoot_EnterOne3_Zeroed(nm1,nm2,nm3)
#	define GB_GC_Safe4_Zeroed(nm1,nm2,nm3,nm4)			MM_LclRoot_EnterOne4_Zeroed(nm1,nm2,nm3,nm4)

#	define GB_GC_RegisterModule(m)						mm_itf_registerModule(m)

	static inline Ptr gb_malloc( size_t sz ) { return mm_itf_malloc( sz ) ; }
	static inline void gb_free( Ptr p ) { mm_itf_free( p ) ; }

#else

#	define GB_HeapAlloc_Words(nWords)					Cast(GB_Ptr,heapalloc(nWords))
#	define GB_HeapAlloc_Words_Fixed(nWords)				GB_HeapAlloc_Words(nWords)
#	define GB_HeapAlloc_Bytes(nBytes)					GB_HeapAlloc_Words(EntierUpBy(nBytes,sizeof(GB_Word)))
#	define GB_HeapAlloc_Bytes_Fixed(nBytes)				GB_HeapAlloc_Bytes(nBytes)	

#	if TRACE
#		define GB_GC_Maintained(x)						False
#	else
#		define GB_GC_Maintained(x)						False
#	endif

#	define GB_GC_MinAlloc_Field_Words(szw)				(szw)
#	define GB_GC_MinAlloc_Field_Bytes(szb)				(szb)
#	define GB_GC_MinAlloc_Malloc(szb)					(szb)

#	define GB_GC_RegisterRoot(n)						// not necessary

#	define GB_GC_SafeEnter								// not necessary
#	define GB_GC_SafeLeave								// not necessary
#	define GB_GC_Safe1(nm1)								// not necessary
#	define GB_GC_Safe2(nm1,nm2)							// not necessary
#	define GB_GC_Safe3(nm1,nm2,nm3)						// not necessary
#	define GB_GC_Safe4(nm1,nm2,nm3,nm4)					// not necessary
#	define GB_GC_Safe1_Zeroed(nm1)						// not necessary
#	define GB_GC_Safe2_Zeroed(nm1,nm2)					// not necessary
#	define GB_GC_Safe3_Zeroed(nm1,nm2,nm3)				// not necessary
#	define GB_GC_Safe4_Zeroed(nm1,nm2,nm3,nm4)			// not necessary

#	define GB_GC_RegisterModule(m)						// not necessary

	static inline Ptr gb_malloc( size_t sz ) { return malloc( sz ) ; }
	static inline void gb_free( Ptr p ) { return free( p ) ; }

#endif

#define GB_GC_MinAlloc_Node_Words(szw)			(GB_GC_MinAlloc_Field_Words(szw)+1)
#define GB_GC_MinAlloc_Node_Bytes(szb)			(GB_GC_MinAlloc_Field_Bytes(szb)+sizeof(Word))

#define GB_NodeAlloc_In(nWords,n)				{ (n) = Cast(GB_NodePtr,GB_HeapAlloc_Words(nWords)) ; }
#define GB_NodeAlloc_In_Fixed(nWords,n)			{ (n) = Cast(GB_NodePtr,GB_HeapAlloc_Words_Fixed(nWords)) ; }
#define GB_NodeAlloc_Hdr_In(nWords,h,n)			{ GB_NodeAlloc_In(nWords,n) ; (n)->header = (h) ; }
#define GB_NodeAlloc_Hdr_In_Fixed(nWords,h,n)	{ GB_NodeAlloc_In_Fixed(nWords,n) ; (n)->header = (h) ; }
%%]

For finalizers boehm's gc is assumed!!!!
This breaks when compiled without bgc.

%%[95
#if USE_BOEHM_GC
#define GB_Register_Finalizer(n,cd)			GC_REGISTER_FINALIZER(n, &gb_Node_Finalize, cd, Cast(GC_finalization_proc*,&gb_Dummy_Finalization_Proc), &gb_Dummy_Finalization_cd)
#elif USE_EHC_MM
#define GB_Register_Finalizer(n,cd)			
#else
#define GB_Register_Finalizer(n,cd)			
#endif
%%]

%%[95
#define GB_NodeAlloc_Malloc_In(nBytes,n)	{ GB_NodeAlloc_Hdr_In(GB_NodeMallocSize,GB_MkMallocHeader,n) ; \
											  (n)->content.ptr = gb_malloc(GB_GC_MinAlloc_Malloc(nBytes)) ; \
											  GB_Register_Finalizer(n,&((n)->content.ptr)) ; \
											}
#define GB_NodeAlloc_ByteArray_In(nBytes,n)	{ GB_NodeAlloc_Hdr_In(GB_NodeMallocSize_ByteArray,GB_MkMallocHeader_ByteArray,n) ; \
											  (n)->content.bytearray.size = nBytes ; \
											  (n)->content.bytearray.ptr = gb_malloc(GB_GC_MinAlloc_Malloc(nBytes)) ; \
											  GB_Register_Finalizer(n,&((n)->content.bytearray.ptr)) ; \
											}
%%]

%%[97
#define GB_NodeAlloc_Float_In(n)			{ GB_NodeAlloc_Hdr_In(GB_NodeFloatSize, GB_MkFloatHeader, n) ; }
#define GB_NodeAlloc_Double_In(n)			{ GB_NodeAlloc_Hdr_In(GB_NodeDoubleSize,GB_MkDoubleHeader,n) ; }
%%]


%%[98
#define GB_NodeAlloc_Chan_In_Fixed(n)		{ GB_NodeAlloc_Hdr_In_Fixed(GB_NodeChanSize, GB_MkChanHeader, n) ; }
#define GB_NodeAlloc_Chan_In(n)				{ GB_NodeAlloc_Hdr_In(GB_NodeChanSize, GB_MkChanHeader, n) ; \
											  GB_Register_Finalizer(n,&((n)->content.chan)) ; \
											}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BASE_TYPES_H__ */
%%]


