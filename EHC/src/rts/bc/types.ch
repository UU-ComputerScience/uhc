%%[8
#ifndef __BC_TYPES_H__
#define __BC_TYPES_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types for bc backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
#define GB_Word_SizeOfWordTag 		2
#define GB_Word_TagMask 			Bits_Size2LoMask(GB_Word,GB_Word_SizeOfWordTag)
#define GB_Word_IntMask 			(~ GB_Word_TagMask)
#define GB_Word_TagPtr 				0
#define GB_Word_TagInt 				1
#define GB_Word_TagGC 				2	// GC info

#define GB_Int_ShiftPow2			Bits_Pow2(GB_Int,GB_Word_SizeOfWordTag)

#define GB_Word_Tag(x)				(Cast(Word,x) & GB_Word_TagMask)
#define GB_Word_IsInt(x)			(GB_Word_Tag(x) == GB_Word_TagInt)
#define GB_Word_IsGC(x)				(GB_Word_Tag(x) == GB_Word_TagGC)
#define GB_Word_IsPtr(x)			(! GB_Word_Tag(x))
%%]

%%[8
#define GB_Word_UnTag(x)			((x) >> GB_Word_SizeOfWordTag)
#define GB_Word_MkGC(x)				(((x) << GB_Word_SizeOfWordTag) | GB_Word_TagGC)
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
These must coincide with the values in function src/ehc/GrinCode/GrinByteCode.tag

%%[8
#define GB_NodeTagCat_Fun				0			/* saturated function call closure 		*/
#define GB_NodeTagCat_App				1			/* general purpose application 			*/
#define GB_NodeTagCat_Ind				2			/* indirection 							*/
#define GB_NodeTagCat_CFun				3			/* saturated C function call closure 	*/

#define GB_NodeTagCat_Con				0			/* data, constructor 											*/
#define GB_NodeTagCat_PAp				1			/* partial application, tag is size of missing 					*/
%%[[94
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
#define GB_NodeTag_Intl_GMP_mpz			4			// Internal node: GMP mpz_t 
#define GB_NodeTag_Intl_GMP_intl		5			// Internal node: GMP internal allocated 
// #endif

#define GB_NodeTag_Intl_LTM_mpz			6			// Internal node: LTM int
%%]

%%[98
#define GB_NodeTag_Intl_Chan			7			// Internal node: GB_Chan
%%]

%%[94
#define GB_NodeTag_Intl_WeakPtr			8			// Internal node: GB_WeakPtr
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
%%% Interaction with GC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if USE_EHC_MM
// has node traceable content?
static inline Bool gb_NH_HasTraceableFields( GB_NodeHeader h ) {
	Bool doTrace = True ;
%%[[94
	if ( GB_NH_Fld_NdEv(h) == GB_NodeNdEv_No && GB_NH_Fld_TagCat(h) == GB_NodeTagCat_Intl ) {
		switch( GB_NH_Fld_Tag(h) ) {
			case GB_NodeTag_Intl_WeakPtr :
%%[[95
			case GB_NodeTag_Intl_Malloc :
			case GB_NodeTag_Intl_Malloc2 :
%%]]
%%[[97
			case GB_NodeTag_Intl_Float :
			case GB_NodeTag_Intl_Double :
#		if USE_GMP
			case GB_NodeTag_Intl_GMP_intl :
			case GB_NodeTag_Intl_GMP_mpz :
#		endif
#		if USE_LTM
			case GB_NodeTag_Intl_LTM_mpz :
#		endif
%%]]
%%[[98
			// case GB_NodeTag_Intl_Chan :
%%]]
				doTrace = False ;
				break ;
			default :
				break ;
		}
	}
%%]]
	return doTrace ;
}
#endif
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

#if ! USE_EHC_MM
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

#define GB_MkConNodeN(n,sz,tg)					{GB_NodeAlloc_Fields_In_MinAlloc((sz),n); GB_FillConNodeN(n,sz,tg); }
#define GB_MkConNodeN_Rooted(n,sz,tg)			{GB_MkConNodeN(n,sz,tg) ; GB_GC_RegisterRoot(n); }
#define GB_MkConNodeN_Fixed(n,sz,tg)			{GB_NodeAlloc_Fields_In_Fixed_MinAlloc((sz),n); GB_FillConNodeN(n,sz,tg); }
#define GB_MkConNode0(n,tg)						{GB_NodeAlloc_Fields_In_MinAlloc((0),n); GB_FillConNode0(n,tg); }
#define GB_MkConNode1(n,tg,x1)					{GB_NodeAlloc_In(2,n); GB_FillConNode1(n,tg,x1); }
#define GB_MkConNode2(n,tg,x1,x2)				{GB_NodeAlloc_In(3,n); GB_FillConNode2(n,tg,x1,x2); }
#define GB_MkConNode2_Ensured(n,tg,x1,x2)		{GB_NodeAlloc_In_Ensured(3,n); GB_FillConNode2(n,tg,x1,x2); }
#define GB_MkConNode3(n,tg,x1,x2,x3)			{GB_NodeAlloc_In(4,n); GB_FillConNode3(n,tg,x1,x2,x3); }
#define GB_MkConNode4(n,tg,x1,x2,x3,x4)			{GB_NodeAlloc_In(5,n); GB_FillConNode4(n,tg,x1,x2,x3,x4); }
#define GB_MkConNode5(n,tg,x1,x2,x3,x4,x5)		{GB_NodeAlloc_In(6,n); GB_FillConNode5(n,tg,x1,x2,x3,x4,x5); }

#define GB_MkTupNode1_In(n,x1)				GB_MkConNode1(n,0,x1)
#define GB_MkTupNode2_In(n,x1,x2)			GB_MkConNode2(n,0,x1,x2)
#define GB_MkTupNode2_In_Ensured(n,x1,x2)	GB_MkConNode2_Ensured(n,0,x1,x2)
#define GB_MkTupNode3_In(n,x1,x2,x3)		GB_MkConNode3(n,0,x1,x2,x3)

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
#	define GB_MkExpNodeIn(n,sz)				GB_MkConNodeN_Fixed( n, GB_GC_MinAlloc_Field_Words(sz), 0 )
#elif USE_EHC_MM
#	define GB_MkExpNodeIn(n,sz)				GB_MkConNodeN_Rooted( n, sz, 0 )
#else
#	define GB_MkExpNodeIn(n,sz)				GB_MkConNodeN( n, GB_GC_MinAlloc_Field_Words(sz), 0 )
#endif

%%]
extern GB_NodePtr gb_MkCAF( GB_BytePtr pc ) ;

%%[94
#define GB_NodeWeakPtrSize					(EntierUpDivBy(sizeof(GB_WeakPtr),sizeof(GB_Word)) + 1)
#define GB_MkWeakPtrHeader					GB_MkHeader(GB_NodeWeakPtrSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_WeakPtr)
%%]

%%[95
#define GB_NodeMallocSize					(EntierUpDivBy(sizeof(void*),sizeof(GB_Word)) + 1)
#define GB_NodeMallocSize_ByteArray			(EntierUpDivBy(sizeof(GB_ByteArray),sizeof(GB_Word)) + 1)

#define GB_MkMallocHeader					GB_MkHeader(GB_NodeMallocSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Malloc)
#define GB_MkMallocHeader_ByteArray			GB_MkHeader(GB_NodeMallocSize_ByteArray, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Malloc2)
%%]

%%[97
%%]
#define GB_NodeFloatSize					(EntierUpDivBy(sizeof(float),sizeof(GB_Word)) + 1)
#define GB_MkFloatHeader					GB_MkHeader(GB_NodeFloatSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Float)

#define GB_NodeDoubleSize					(EntierUpDivBy(sizeof(double),sizeof(GB_Word)) + 1)
#define GB_MkDoubleHeader					GB_MkHeader(GB_NodeDoubleSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Double)


%%[98
#define GB_NodeChanSize						(EntierUpDivBy(sizeof(GB_Chan),sizeof(GB_Word)) + 1)
#define GB_MkChanHeader						GB_MkHeader(GB_NodeChanSize, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_Chan)
%%]

%%[8
#define GB_Node_ZeroFieldsFrom(fr,n)		{ memset( &(((GB_NodePtr)n)->content.fields[fr]), 0, ((GB_NH_Fld_Size(((GB_NodePtr)n)->header) - fr) << Word_SizeInBytes_Log) - sizeof(GB_NodeHeader) ) ; }
#define GB_Node_ZeroFields(n)				GB_Node_ZeroFieldsFrom(0,n)
%%]
#define GB_Node_ZeroFields(n)				{ memset( ((GB_NodePtr)n)->content.fields, 0, (GB_NH_Fld_Size(((GB_NodePtr)n)->header) << Word_SizeInBytes_Log) - sizeof(GB_NodeHeader) ) ; }

%%[99
#define GB_MkNode_Handle_OldHandle(n,chan)	GB_MkConNode1(n,2,chan)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LTM allocation, must be partially visible to outside
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_LTM
#define GB_NodeLTMMpzSize(nDigits)			EntierUpDivBy(sizeof(GB_NodeHeader) + 2*sizeof(HalfWord) + (nDigits)*sizeof(GB_LTM_1Digit), sizeof(Word))
#define GB_MkLTMMpzHeader(nWords)			GB_MkHeader(nWords, GB_NodeNdEv_No, GB_NodeTagCat_Intl, GB_NodeTag_Intl_LTM_mpz)

#define GB_NodeAlloc_LTMMpzDigs_In_Alloca(nDigits,n) \
													{ Word _sz = GB_NodeLTMMpzSize(nDigits) ; \
													  GB_NodeAlloc_Hdr_In_Alloca(_sz,GB_MkLTMMpzHeader(_sz),n) ; \
													  GB_Node_ZeroFields(n) ; \
													}
#endif
%%]
													  printf("GB_NodeAlloc_LTMMpzDigs_In_Alloca dig=%x sz=%x p=%p\n",nDigits,_sz,n) ; \


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
typedef MM_WeakPtr_Object	GB_WeakPtr ;
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
  Word				isText ;
} __attribute__ ((__packed__)) GB_Chan ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LTM: Multiple precision Integer, embedding in node for use with LTM library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_LTM
typedef HalfWord	GB_LTM_1Digit ;
typedef     Word	GB_LTM_2Digit ;

// Space before digits in GB_LTM_Int must occupy 1 Word.
typedef struct GB_LTM_Int {
  HalfWord			used ;		// nr of digits in use, total is derived from size in header
  HalfWord			sign ;
  GB_LTM_1Digit		digits[1] ;	// allocation determines real size
} __attribute__ ((__packed__)) GB_LTM_Int ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GB node structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
#if USE_LTM
    GB_LTM_Int		mpz ;				/* when GB_NodeTag_Intl_LTM_mpz */
#endif
%%]]
%%[[94
    GB_WeakPtr		weakptr ;			/* when GB_NodeTag_Intl_WeakPtr */
%%]]
%%[[98
    GB_Chan			chan ;				/* when GB_NodeTag_Intl_Chan */
%%]]
  } __attribute__ ((__packed__)) content ;
} __attribute__ ((__packed__)) GB_Node, *GB_NodePtr ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LTM: embedding macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_LTM
#define GB_LTM_Int_Used(n)		(((n))->content.mpz.used)
#define GB_LTM_Int_Sign(n)		(((n))->content.mpz.sign)
#define GB_LTM_Int_Digits(n)	(((n))->content.mpz.digits)
#define GB_LTM_Int_Alloc(n)		Cast(HalfWord,((GB_NH_NrFlds(((n))->header) - 1) << 1))
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LTM: utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_LTM
#define GB_LTM_Int_NrDigitsOfBits(szBits)		EntierUpDivBy(szBits,DIGIT_BIT)
#define GB_LTM_Int_NrDigitsOfBytes(szBytes)		GB_LTM_Int_NrDigitsOfBits((szBytes)<<Byte_SizeInBits_Log)
#define GB_LTM_Int_NrDigitsOfDoubleExp(exp)		(GB_LTM_Int_NrDigitsOfBits(MAX(0,exp)+(sizeof(double)<<Byte_SizeInBits_Log))+1)
#define GB_LTM_Int_NrDigitsOfAndLike(n1,n2)		MAX(GB_LTM_Int_Used(n1),GB_LTM_Int_Used(n2))
#define GB_LTM_Int_NrDigitsOfAddLike(n1,n2)		(GB_LTM_Int_NrDigitsOfAndLike(n1,n2)+1)
#define GB_LTM_Int_NrDigitsOfMulLike(n1,n2)		(GB_LTM_Int_Used(n1)+GB_LTM_Int_Used(n2)+1)
#define GB_LTM_Int_NrDigitsOfDivLike(n1,n2)		((MAX(GB_LTM_Int_Used(n1),GB_LTM_Int_Used(n2))<<1)+2)
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GB module info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
#	define GB_HeapAlloc_Bytes_GCInfo(nBytes,gcInfo)		GB_HeapAlloc_Bytes(nBytes)

#	define GB_HeapAlloc_Words(nWords)					GB_HeapAlloc_Bytes((nWords) << Word_SizeInBytes_Log)
#	define GB_HeapAlloc_Words_Ensured(nWords)			GB_HeapAlloc_Words(nWords)
#	define GB_HeapAlloc_Bytes_Fixed(nBytes)				Cast(GB_Ptr,GC_MALLOC_UNCOLLECTABLE(nBytes))
#	define GB_HeapAlloc_Words_Fixed(nWords)				GB_HeapAlloc_Bytes_Fixed((nWords) << Word_SizeInBytes_Log)
#	define GB_HeapFree_Fixed(p)							

#	if TRACE
#		define GB_GC_Managed(x)							( Cast(GB_Ptr,x) >= gb_allocated_lowest_ptr && Cast(GB_Ptr,x) <= gb_allocated_highest_ptr )
#	else
#		define GB_GC_Managed(x)							False
#	endif

#	define GB_GC_MinAlloc_Field_Words(szw)				(szw)
#	define GB_GC_MinAlloc_Node_Words(szw)				(szw)
#	define GB_GC_MinAlloc_Malloc(szb)					(szb)

#	define GB_GC_RegisterRoot(n)						// not necessary

#	define GB_GCSafe_Enter								// not necessary
#	define GB_GCSafe_Leave								// not necessary
#	define GB_GCSafe_1(nm1)								// not necessary
#	define GB_GCSafe_2(nm1,nm2)							// not necessary
#	define GB_GCSafe_3(nm1,nm2,nm3)						// not necessary
#	define GB_GCSafe_4(nm1,nm2,nm3,nm4)					// not necessary
#	define GB_GCSafe_1_Zeroed(nm1)						// not necessary
#	define GB_GCSafe_2_Zeroed(nm1,nm2)					// not necessary
#	define GB_GCSafe_3_Zeroed(nm1,nm2,nm3)				// not necessary
#	define GB_GCSafe_4_Zeroed(nm1,nm2,nm3,nm4)			// not necessary

#	define GB_GC_RegisterModule(m)						// not necessary

	extern void gb_Node_Finalize( void* p, void* cd ) ;
	extern void* gb_Dummy_Finalization_Proc ;
	extern void* gb_Dummy_Finalization_cd ;

	static inline Ptr gb_malloc( size_t sz ) { return malloc( sz ) ; }
	static inline void gb_free( Ptr p ) { return free( p ) ; }

#elif USE_EHC_MM

#	if TRACE
#		define GB_HeapAlloc_Bytes(nBytes)				( mm_itf_maxAllocCheck(nBytes) ? Cast(GB_Ptr,mm_itf_alloc(nBytes,0)) : (GB_Ptr)NULL )
#	else
#		define GB_HeapAlloc_Bytes(nBytes)				Cast(GB_Ptr,mm_itf_alloc(nBytes,0))
#	endif
#	define GB_HeapAlloc_Bytes_Ensured(nBytes)			Cast(GB_Ptr,mm_itf_allocEnsured(nBytes))
#	define GB_HeapAlloc_Bytes_GCInfo(nBytes,gcInfo)		Cast(GB_Ptr,mm_itf_alloc(nBytes,gcInfo))
#	define GB_HeapAlloc_Words(nWords)					GB_HeapAlloc_Bytes((nWords) << Word_SizeInBytes_Log)
#	define GB_HeapAlloc_Words_Ensured(nWords)			GB_HeapAlloc_Bytes_Ensured((nWords) << Word_SizeInBytes_Log)
#	define GB_HeapAlloc_Bytes_Fixed(nBytes)				Cast(GB_Ptr,mm_itf_allocResident(nBytes))
#	define GB_HeapAlloc_Words_Fixed(nWords)				GB_HeapAlloc_Bytes_Fixed((nWords) << Word_SizeInBytes_Log)
#	define GB_HeapFree_Fixed(p)							mm_itf_deallocResident(p)

#	define GC_MALLOC(nBytes)							GB_HeapAlloc_Bytes(nBytes)
#	define GC_MALLOC_UNCOLLECTABLE(nBytes)				GB_HeapAlloc_Bytes_Fixed(nBytes)

#	define GB_GC_Managed(x)								mm_plan.mutator->isMaintainedByGC( mm_plan.mutator, (Word)x )

#	define GB_GC_MinAlloc_Field_Words(szw)				(MAX(szw,1))				// minimal for node: at least 1 field payload to allow for indirection/forwarding
#	define GB_GC_MinAlloc_Node_Words(szw)				(MAX(szw,2))				// minimal for node: at least 1 field payload to allow for indirection/forwarding
#	define GB_GC_MinAlloc_Malloc(szb)					(MAX(szb,sizeof(Word)))		// minimal for malloc: at least a Word

#	define GB_GC_RegisterRoot(n)						{ GB_Node_ZeroFields(n) ; mm_itf_registerGCRoot( (WPtr)(&n) ) ; }

#	define GB_GCSafe_Enter								MM_LclRoot_EnterGrp
#	define GB_GCSafe_Leave								MM_LclRoot_LeaveGrp
#	define GB_GCSafe_1(nm1)								MM_LclRoot_EnterOne1(nm1)
#	define GB_GCSafe_2(nm1,nm2)							MM_LclRoot_EnterOne2(nm1,nm2)	
#	define GB_GCSafe_3(nm1,nm2,nm3)						MM_LclRoot_EnterOne3(nm1,nm2,nm3)
#	define GB_GCSafe_4(nm1,nm2,nm3,nm4)					MM_LclRoot_EnterOne4(nm1,nm2,nm3,nm4)
#	define GB_GCSafe_1_Zeroed(nm1)						MM_LclRoot_EnterOne1_Zeroed(nm1)
#	define GB_GCSafe_2_Zeroed(nm1,nm2)					MM_LclRoot_EnterOne2_Zeroed(nm1,nm2)	
#	define GB_GCSafe_3_Zeroed(nm1,nm2,nm3)				MM_LclRoot_EnterOne3_Zeroed(nm1,nm2,nm3)
#	define GB_GCSafe_4_Zeroed(nm1,nm2,nm3,nm4)			MM_LclRoot_EnterOne4_Zeroed(nm1,nm2,nm3,nm4)

#	define GB_GC_RegisterModule(m)						mm_itf_registerModule(m)

	extern void gb_Node_Finalize( Word p ) ;
#	if USE_GMP
		extern void gb_Node_FinalizeInteger( GB_NodePtr n ) ;
#	endif
	
	static inline Ptr gb_malloc( size_t sz ) { return mm_itf_malloc( sz ) ; }
	static inline void gb_free( Ptr p ) { mm_itf_free( p ) ; }

#else

#	define GB_HeapAlloc_Words(nWords)					Cast(GB_Ptr,heapalloc(nWords))
#	define GB_HeapAlloc_Words_Ensured(nWords)			GB_HeapAlloc_Words(nWords)
#	define GB_HeapAlloc_Words_Fixed(nWords)				GB_HeapAlloc_Words(nWords)
#	define GB_HeapAlloc_Bytes(nBytes)					GB_HeapAlloc_Words(EntierUpBy(nBytes,sizeof(GB_Word)))
#	define GB_HeapAlloc_Bytes_GCInfo(nBytes,gcInfo)		GB_HeapAlloc_Bytes(nBytes)
#	define GB_HeapAlloc_Bytes_Fixed(nBytes)				GB_HeapAlloc_Bytes(nBytes)	
#	define GB_HeapFree_Fixed(p)							

#	if TRACE
#		define GB_GC_Managed(x)						False
#	else
#		define GB_GC_Managed(x)						False
#	endif

#	define GB_GC_MinAlloc_Field_Words(szw)				(szw)
#	define GB_GC_MinAlloc_Node_Words(szw)				(szw)
#	define GB_GC_MinAlloc_Malloc(szb)					(szb)

#	define GB_GC_RegisterRoot(n)						// not necessary

#	define GB_GCSafe_Enter								// not necessary
#	define GB_GCSafe_Leave								// not necessary
#	define GB_GCSafe_1(nm1)								// not necessary
#	define GB_GCSafe_2(nm1,nm2)							// not necessary
#	define GB_GCSafe_3(nm1,nm2,nm3)						// not necessary
#	define GB_GCSafe_4(nm1,nm2,nm3,nm4)					// not necessary
#	define GB_GCSafe_1_Zeroed(nm1)						// not necessary
#	define GB_GCSafe_2_Zeroed(nm1,nm2)					// not necessary
#	define GB_GCSafe_3_Zeroed(nm1,nm2,nm3)				// not necessary
#	define GB_GCSafe_4_Zeroed(nm1,nm2,nm3,nm4)			// not necessary

#	define GB_GC_RegisterModule(m)						// not necessary

	static inline Ptr gb_malloc( size_t sz ) { return malloc( sz ) ; }
	static inline void gb_free( Ptr p ) { return free( p ) ; }

#endif

#define GB_GC_MinAlloc_FieldAnd1_Words(szw)		(GB_GC_MinAlloc_Field_Words(szw)+1)

#define GB_NodeAlloc_In(nWords,n)				{ (n) = Cast(GB_NodePtr,GB_HeapAlloc_Words(nWords)) ; }
#define GB_NodeAlloc_In_Fixed(nWords,n)			{ (n) = Cast(GB_NodePtr,GB_HeapAlloc_Words_Fixed(nWords)) ; }
#define GB_NodeAlloc_In_Alloca(nWords,n)		{ (n) = Cast(GB_NodePtr,alloca((nWords) << Word_SizeInBytes_Log)) ; }

#if USE_EHC_MM
#	define GB_NodeAlloc_In_Ensured(nWords,n)	{ (n) = Cast(GB_NodePtr,GB_HeapAlloc_Words_Ensured(nWords)) ; }
#else
#	define GB_NodeAlloc_In_Ensured(nWords,n)	GB_NodeAlloc_In(nWords,n)
#endif

#define GB_NodeAlloc_Hdr_In(nWords,h,n)			{ GB_NodeAlloc_In(nWords,n) ; (n)->header = (h) ; }
#define GB_NodeAlloc_Hdr_In_Ensured(nWords,h,n)	{ GB_NodeAlloc_In_Ensured(nWords,n) ; (n)->header = (h) ; }
#define GB_NodeAlloc_Hdr_In_Fixed(nWords,h,n)	{ GB_NodeAlloc_In_Fixed(nWords,n) ; (n)->header = (h) ; }
#define GB_NodeAlloc_Hdr_In_Alloca(nWords,h,n)	{ GB_NodeAlloc_In_Alloca(nWords,n) ; (n)->header = (h) ;}

#if USE_EHC_MM
#	define GB_NodeAlloc_Fields_In_MinAlloc(nFlds,n)				{ GB_NodeAlloc_In(GB_GC_MinAlloc_FieldAnd1_Words(nFlds),n) ; (n)->content.fields[0]=0 ; }
#	define GB_NodeAlloc_Fields_In_Fixed_MinAlloc(nFlds,n)		{ GB_NodeAlloc_In_Fixed(GB_GC_MinAlloc_FieldAnd1_Words(nFlds),n) ; (n)->content.fields[0]=0 ; }
#else
#	define GB_NodeAlloc_Fields_In_MinAlloc(nFlds,n)				GB_NodeAlloc_In(GB_GC_MinAlloc_FieldAnd1_Words(nFlds),n)
#	define GB_NodeAlloc_Fields_In_Fixed_MinAlloc(nFlds,n)		GB_NodeAlloc_In_Fixed(GB_GC_MinAlloc_FieldAnd1_Words(nFlds),n)
#endif

%%]

For finalizers boehm's gc is assumed!!!!
This breaks when compiled without bgc.
20091001: EHC MM is under construction.

%%[95
#if USE_BOEHM_GC
#	define GB_Register_Finalizer(n,cd)			GC_REGISTER_FINALIZER(n, &gb_Node_Finalize, cd, Cast(GC_finalization_proc*,&gb_Dummy_Finalization_Proc), &gb_Dummy_Finalization_cd)
#	define GB_Register_FinalizerEnsured(n,cd)	GB_Register_Finalizer(n,cd)
#	define GB_AllocEnsure_Bytes(sz)					
#elif USE_EHC_MM
#	define GB_Register_Finalizer(n,cd)			{ GB_GCSafe_Enter ; GB_GCSafe_1(n) ; GB_Register_FinalizerEnsured(n,cd) ; GB_GCSafe_Leave ; }
#	define GB_Register_FinalizerEnsured(n,cd)	{ mm_itf_registerFinalization( (Word)(n), &gb_Node_Finalize ) ; }
#	if TRACE
#		define GB_AllocEnsure_Bytes(sz)			{ mm_itf_maxAllocCheck(sz) ; mm_itf_allocEnsure( (Word_SizeInBytes << 2) + (sz), 0 ) ; }
#	else
#		define GB_AllocEnsure_Bytes(sz)			{ mm_itf_allocEnsure( (Word_SizeInBytes << 2) + (sz), 0 ) ; }
#	endif
#else
#	define GB_Register_Finalizer(n,cd)	
#	define GB_Register_FinalizerEnsured(n,cd)	GB_Register_Finalizer(n,cd)
#	define GB_AllocEnsure_Bytes(sz)					
#endif

#define GB_AllocEnsure_Words(sz)					GB_AllocEnsure_Bytes((sz)<<Word_SizeInBytes_Log)
#define GB_AllocEnsure_Bytes_Finalizer(sz,nrFin)	GB_AllocEnsure_Bytes( (sz) + (nrFin) * (GB_NodeWeakPtrSize << Word_SizeInBytes_Log) )
#define GB_AllocEnsure_Words_Finalizer(sz,nrFin)	GB_AllocEnsure_Bytes_Finalizer((sz)<<Word_SizeInBytes_Log,nrFin)
%%]

%%[95
#define GB_NodeAlloc_ByteArray_In(nBytes,n)	{ GB_NodeAlloc_Hdr_In(GB_NodeMallocSize_ByteArray,GB_MkMallocHeader_ByteArray,n) ; \
											  (n)->content.bytearray.size = nBytes ; \
											  (n)->content.bytearray.ptr = gb_malloc(GB_GC_MinAlloc_Malloc(nBytes)) ; \
											  GB_Register_Finalizer(n,&((n)->content.bytearray.ptr)) ; \
											}
%%]

%%[98
#define GB_NodeAlloc_Chan_In(n)				{ GB_NodeAlloc_Hdr_In(GB_NodeChanSize, GB_MkChanHeader, n) ; \
											  (n)->content.chan.file = NULL ; \
											  (n)->content.chan.name = NULL ; \
											  (n)->content.chan.isText = False ; \
											  GB_Register_Finalizer(n,&((n)->content.chan)) ; \
											}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GC specific information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Descriptor of what can and cannot be traced during GC, only known to the interpreter or other execution machine

%%[8
typedef struct GB_GCStackInfo {
  Word16	sz ;	// size of stack fragment described by this info, in words
  Word8		nrDescrs ; 
  Word8*	descrs ; 
} __attribute__ ((__packed__)) GB_GCStackInfo ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function specific information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Descriptor of info about functions

%%[8
typedef struct GB_FunctionInfo {
  Word16	szStack ;	// size of stack required by function, in bytes
  Word8		flags ;
  Word8*	nm ; 		// name of function
} __attribute__ ((__packed__)) GB_FunctionInfo ;

typedef SHalfWord		GB_FunctionInfo_Inx ;

#define GB_FunctionInfo_Inx_None						(-1)
%%]

%%[8
#define GB_FunctionInfoFlag_None						0x0
#define GB_FunctionInfoFlag_1stArgIsStackTrace			0x1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Link Chain kinds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Defines + encoding must correspond with the datatype LinkChainKind in src/ehc/GrinByteCode

%%[8
#define GB_LinkChainEncoding_Ind			0
#define GB_LinkChainEncoding_16_10			1	// 16(32) bits info, 10(26) bits offset to next entry
%%]

%%[8
#define GB_LinkChainKind_None				0
#define GB_LinkChainKind_GCInfo				1
#define GB_LinkChainKind_Const				2
#define GB_LinkChainKind_Code				3
#define GB_LinkChainKind_Offset				4
#define GB_LinkChainKind_Offsets			5
#define GB_LinkChainKind_CallInfo			6
#define GB_LinkChainKind_StringConst		7
#define GB_LinkChainKind_FunctionInfo		8
%%[[20
#define GB_LinkChainKind_ImpEntry			9
%%]]
%%]

%%[8
#if USE_64_BITS
#define GB_LinkChainKind_Info_Shift			32
#else
#define GB_LinkChainKind_Info_Shift			16
#endif
#define GB_LinkChainKind_Off_Shift			6
#define GB_LinkChainKind_Kind_Shift			2
#define GB_LinkChainKind_Enc_Shift			0

#define GB_LinkChainKind_Ind_Inx_Shift		GB_LinkChainKind_Kind_Shift

#define GB_LinkChainKind_Fld_Info(x)		Bits_ExtrFromSh(Word,x,GB_LinkChainKind_Info_Shift)
#define GB_LinkChainKind_Fld_Off(x)			Bits_ExtrFromToSh(Word,x,GB_LinkChainKind_Off_Shift,GB_LinkChainKind_Info_Shift-1)
#define GB_LinkChainKind_Fld_Kind(x)		Bits_ExtrFromToSh(Word,x,GB_LinkChainKind_Kind_Shift,GB_LinkChainKind_Off_Shift-1)
#define GB_LinkChainKind_Fld_Enc(x)			Bits_ExtrFromToSh(Word,x,GB_LinkChainKind_Enc_Shift,GB_LinkChainKind_Kind_Shift-1)

#define GB_LinkChainKind_Ind_Fld_Inx(x)		Bits_ExtrFromSh(Word,x,GB_LinkChainKind_Ind_Inx_Shift)
%%]

This encoding of a LinkChain entry is used when too large, the entry inlined in the code refers to an external entry as described by
the following:

%%[8
typedef struct GB_LinkChainResolvedInfo {
	Word32		info ;
	Word16		off ;
	Word16		kind ;
} __attribute__ ((__packed__)) GB_LinkChainResolvedInfo ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evaluation assertions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// check (and panic) if not evaluated
static inline Bool gb_assert_IsEvaluated( Word n, char* msg ) {
#	if TRACE && __UHC_TARGET_BC__
		if ( GB_GC_Managed( n ) && GB_NH_Fld_NdEv( ((GB_NodePtr)n)->header ) != GB_NodeNdEv_No ) {
			rts_panic2_1( "unevaluated", msg, n ) ;
		}
#	endif
	return True ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GC Memory correctness
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if USE_EHC_MM

#if TRACE
static inline void gb_assert_IsNotDangling( Word x, char* msg ) {
	// if ( GB_Word_IsPtr(x) ) {
		mm_assert_IsNotDangling( x, msg ) ;
	// }
}
#else
#define gb_assert_IsNotDangling(n,m)
#endif

#if TRACE
static inline void gb_assert_IsNotDangling_Node( GB_NodePtr n, char* msg ) {
	int i ;
	if ( gb_NH_HasTraceableFields( n->header ) ) {
		for ( i = 0 ; i < GB_NH_NrFlds(n->header) ; i++ ) {
			gb_assert_IsNotDangling( n->content.fields[i], msg ) ;
		}
	}
}
#else
#define gb_assert_IsNotDangling_Node(n,m)
#endif

#else
#define gb_assert_IsNotDangling(w,m)
#define gb_assert_IsNotDangling_Node(n,m)
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Indirection following & assertions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// is header of node an indirection?
static inline Bool gb_NodeHeader_IsIndirection( GB_NodeHeader h ) {
	return GB_NH_Fld_NdEv(h) == GB_NodeNdEv_Yes && GB_NH_Fld_TagCat(h) == GB_NodeTagCat_Ind ;
}

// is node an indirection?
static inline Bool gb_IsIndirection( Word n ) {
	return GB_Word_IsPtr(n) && gb_NodeHeader_IsIndirection( ((GB_NodePtr)n)->header ) ;
}
%%]

%%[8
#if __UHC_TARGET_BC__
extern void gb_prWord( GB_Word x ) ;
#endif

// check (and panic) if indirection
static inline Bool gb_assert_IsNotIndirection( Word n, char* msg ) {
#	if TRACE && __UHC_TARGET_BC__
		if ( GB_GC_Managed( n ) && gb_IsIndirection(n) ) {
			gb_prWord( n ) ; printf("\n") ;
			rts_panic2_1( "indirection", msg, n ) ;
		}
#	endif
	return True ;
}
%%]

%%[8
// following indirection when already knowing it is an indirection
static inline Word gb_Indirection_FollowIndirection( Word n ) {
	do {
		n = ((GB_NodePtr)n)->content.fields[0] ;
	} while ( gb_IsIndirection(n) ) ;
	return n ;
}

// following indirection when not yet knowing it is an indirection
static inline Word gb_Indirection_FollowObject( Word n ) {
	if ( GB_Word_IsPtr(n) && gb_NodeHeader_IsIndirection( ((GB_NodePtr)n)->header ) ) {
		n = gb_Indirection_FollowIndirection( n ) ;
	}
	return n ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BC_TYPES_H__ */
%%]
