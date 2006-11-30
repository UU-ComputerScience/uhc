
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word, and Pointer to Word.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Requirement: sizeof(GB_Ptr) == sizeof(GB_Word)

%%[8
#if USE_64_BITS
typedef uint64_t GB_Word ;
typedef  int64_t GB_SWord ;
#else
typedef uint32_t GB_Word ;
typedef  int32_t GB_SWord ;
#endif

typedef GB_Word* GB_Ptr ;
typedef GB_Ptr*  GB_PtrPtr ;
typedef uint8_t* GB_BytePtr ;
typedef GB_SWord GB_Int ;
typedef uint16_t GB_NodeTag ;
typedef uint16_t GB_NodeSize ;
typedef uint8_t  GB_Byte ;
%%]

%%[8
#define GB_Deref(x)						(*Cast(GB_Ptr,x))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if USE_64_BITS
#define GB_NodeHeader_Size_BitSz		32
#define GB_NodeHeader_NdEv_BitSz		2
#define GB_NodeHeader_TagCat_BitSz		2
#define GB_NodeHeader_GC				2
#define GB_NodeHeader_Tag_BitSz			26
#else
#define GB_NodeHeader_Size_BitSz		16
#define GB_NodeHeader_NdEv_BitSz		2
#define GB_NodeHeader_TagCat_BitSz		2
#define GB_NodeHeader_GC				2
#define GB_NodeHeader_Tag_BitSz			10
#endif

#define GB_NodeNdEv_BlH					2			/* black hole */
#define GB_NodeNdEv_Yes					1
#define GB_NodeNdEv_No					0

#define GB_NodeTagCat_Fun				0			/* saturated function call closure 		*/
#define GB_NodeTagCat_App				1			/* general purpose application 			*/
#define GB_NodeTagCat_Ind				2			/* indirection 							*/
#define GB_NodeTagCat_CFun				3			/* saturated C function call closure 	*/

#define GB_NodeTagCat_Con				0			/* data, constructor 								*/
#define GB_NodeTagCat_PAp				1			/* partial application, tag is size of missing 		*/

typedef struct GB_NodeHeader {
  unsigned 	size 		: GB_NodeHeader_Size_BitSz 		;			/* size, incl header, in words 					*/
  unsigned 	needsEval 	: GB_NodeHeader_NdEv_BitSz 		;			/* possibly needs eval? 						*/
  unsigned 	tagCateg 	: GB_NodeHeader_TagCat_BitSz 	;			/* kind of tag, dpd on needsEval 				*/
  unsigned 	gc 			: GB_NodeHeader_GC		 		;			/* garbage collection info (unused currently)	*/
  unsigned 	tag 		: GB_NodeHeader_Tag_BitSz 		;			/* tag, or additional size dpd on tagCateg 		*/
} GB_NodeHeader ;

%%]

%%[8
typedef struct GB_Node {
  GB_NodeHeader	header ;
  GB_Word 		fields[] ;
} GB_Node, *GB_NodePtr ;

#define GB_NodeHeaderNrFields(h)			((h).size-1)
#define GB_NodeNrFields(n)					GB_NodeHeaderNrFields((n)->header)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_MkHeader(sz,ev,cat,tg)			{sz, ev, cat, 0, tg}
#define GB_MkFunHeader(nArg)				GB_MkHeader((nArg)+2, GB_NodeNdEv_Yes, GB_NodeTagCat_Fun, 0)
#define GB_MkCFunHeader(nArg)				GB_MkHeader((nArg)+2, GB_NodeNdEv_Yes, GB_NodeTagCat_CFun, 0)
#define GB_MkCAFHeader						GB_MkFunHeader(0)
#define GB_MkConHeader(sz,tg)				GB_MkHeader((sz)+1, GB_NodeNdEv_No, GB_NodeTagCat_Con, tg)

#define GB_MkConEnumNode(tg)				{ GB_MkConHeader(0,tg) }

#define GB_FillNodeFlds1(n,x1)				{(n)->fields[0] = Cast(GB_Word,x1);}
#define GB_FillNodeFlds2(n,x1,x2)			{GB_FillNodeFlds1(n,x1   );(n)->fields[1] = Cast(GB_Word,x2);}
#define GB_FillNodeFlds3(n,x1,x2,x3)		{GB_FillNodeFlds2(n,x1,x2);(n)->fields[2] = Cast(GB_Word,x3);}

#define GB_FillNodeHdr(h,n)					{(n)->header = h;}
#define GB_FillConNode0(n,tg)				{GB_NodeHeader _h = GB_MkConHeader(0,tg); GB_FillNodeHdr(_h,n);}
#define GB_FillConNode1(n,tg,x1)			{GB_NodeHeader _h = GB_MkConHeader(1,tg); GB_FillNodeHdr(_h,n); GB_FillNodeFlds1(n,x1);}
#define GB_FillConNode2(n,tg,x1,x2)			{GB_NodeHeader _h = GB_MkConHeader(2,tg); GB_FillNodeHdr(_h,n); GB_FillNodeFlds2(n,x1,x2);}

#define GB_MkConNode0(n,tg)					{n = Cast(GB_NodePtr,GB_HeapAlloc_Words(1)); GB_FillConNode0(n,tg); }
#define GB_MkConNode1(n,tg,x1)				{n = Cast(GB_NodePtr,GB_HeapAlloc_Words(2)); GB_FillConNode1(n,tg,x1); }
#define GB_MkConNode2(n,tg,x1,x2)			{n = Cast(GB_NodePtr,GB_HeapAlloc_Words(3)); GB_FillConNode2(n,tg,x1,x2); }

#define GB_FillCFunNode0(n,f)				{GB_NodeHeader _h = GB_MkCFunHeader(0); GB_FillNodeHdr(_h,n);GB_FillNodeFlds1(n,f);}
#define GB_FillCFunNode1(n,f,x1)			{GB_NodeHeader _h = GB_MkCFunHeader(1); GB_FillNodeHdr(_h,n);GB_FillNodeFlds2(n,f,x1);}
#define GB_FillCFunNode2(n,f,x1,x2)			{GB_NodeHeader _h = GB_MkCFunHeader(2); GB_FillNodeHdr(_h,n);GB_FillNodeFlds3(n,f,x1,x2);}

#define GB_MkCFunNode0(n,f)					{n = Cast(GB_NodePtr,GB_HeapAlloc_Words(2)); GB_FillCFunNode0(n,f); }
#define GB_MkCFunNode1(n,f,x1)				{n = Cast(GB_NodePtr,GB_HeapAlloc_Words(3)); GB_FillCFunNode1(n,f,x1); }
#define GB_MkCFunNode2(n,f,x1,x2)			{n = Cast(GB_NodePtr,GB_HeapAlloc_Words(4)); GB_FillCFunNode2(n,f,x1,x2); }

extern GB_Node* gb_MkCAF( GB_BytePtr pc ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Tag_List_Nil						1
#define GB_Tag_List_Cons					0

#define GB_MkListNil(n)						{n = &gb_Nil;}
#define GB_MkListCons(n,x1,x2)				GB_MkConNode2(n,GB_Tag_List_Cons,x1,x2)

#define GB_List_IsNull(n)					((n)->header.tag == GB_Tag_List_Nil)
#define GB_List_Head(n)						((n)->fields[0])
#define GB_List_Tail(n)						Cast( GB_NodePtr,(n)->fields[1] )

#define GB_List_Iterate(n,sz,body)			while ( sz-- && ! GB_List_IsNull( n ) ) { \
												body ; \
												n = GB_List_Tail(n) ; \
											}

%%]

%%[8
extern GB_NodePtr gb_listTail( GB_NodePtr n ) ;
extern GB_Word gb_listHead( GB_NodePtr n ) ;
extern Bool gb_listNull( GB_NodePtr n ) ;
extern void gb_listForceEval( GB_NodePtr n, int sz ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PackedString
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Tag_PackedString					0
#define GB_Tag_PackedStringOffset			1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Linking, fixing addresses, module info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_LinkTbl_EntryKind_Const			0			/* constant */
#define GB_LinkTbl_EntryKind_ConstPtr		1			/* ptr to constant */
#define GB_LinkTbl_EntryKind_CodeEntry		2			/* code entry */
%%]
#define GB_LinkTbl_EntryKind_FixOffset		2			/* fix offsets */

Link commands for global references

%%[8
typedef struct GB_LinkEntry {
  uint16_t		tblKind ;
  uint32_t		inx     ;
%%[[12
  uint16_t		inxMod  ;
%%]]
  GB_Ptr		infoLoc ;
} GB_LinkEntry ;
%%]

Fixing offsets, replacing offsets with actual address

%%[8
#define GB_Offset 	GB_Word 

typedef struct GB_FixOffset {
    GB_Ptr		codeLoc ;
    uint16_t    nrOfLocs ;
} GB_FixOffset ;
%%]

Module info

%%[12
typedef struct GB_ModEntry {
  char*			name ;
  GB_NodePtr	expNode ;
} GB_ModEntry ;

extern GB_ModEntry* gb_lookupModEntry( char* modNm, GB_ModEntry* modTbl ) ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Assume that sizeof(GrWord) == sizeof(GB_Word).
This should be ok and merged later on, but a check in it is currently part of the sanity check.
Size must be minimal 2 words to ensure enough space for an indirection pointer (plus the usual header in front).

%%[8
#if USE_BOEHM_GC
#define GB_HeapAlloc_Words(nWords)	GB_HeapAlloc_Bytes(nWords * sizeof(GB_Word))
#define GB_HeapAlloc_Bytes(nBytes)	Cast(GB_Ptr,GC_MALLOC(nBytes))
#else
#define GB_HeapAlloc_Words(nWords)	Cast(GB_Ptr,heapalloc(nWords))
#define GB_HeapAlloc_Bytes(nBytes)	GB_HeapAlloc_Words(nBytes / sizeof(GB_Word))
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GB_Ptr and GB_Int are tagged values, stored in a GB_Word, GB_Ints are shifted left
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Word_TagSize 			1
#define GB_Word_TagMask 			1
#define GB_Word_TagInt 				1
#define GB_Word_TagPtr 				0

#define GB_Int_ShiftPow2			Bits_Pow2(GB_Int,GB_Word_TagSize)

#define GB_Word_IsInt(x)			((x) & GB_Word_TagMask)
#define GB_Word_IsPtr(x)			(! GB_Word_IsInt(x))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GB_Int arithmetic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_GBInt2CastedInt(ty,x)	((ty)((x) / GB_Int_ShiftPow2))
#define GB_GBInt2Int(x)				GB_GBInt2CastedInt(int,x)
#define GB_Int2GBInt(x)				((Cast(GB_Int,x)) << GB_Word_TagSize | GB_Word_TagInt)

#define GB_Int0						GB_Int2GBInt(0)
#define GB_Int1						GB_Int2GBInt(1)
#define GB_Int2						GB_Int2GBInt(2)

#define GB_Int_Add(x,y)				((x) + (y) - GB_Word_TagInt)
#define GB_Int_Sub(x,y)				((x) - (y) + GB_Word_TagInt)
#define GB_Int_Mul(x,y)				(((x)-GB_Word_TagInt) * ((y)/GB_Int_ShiftPow2) + GB_Word_TagInt)
#define GB_Int_Div(x,y)				((((x)-GB_Word_TagInt) / (((y)-GB_Word_TagInt))) * GB_Int_ShiftPow2 + GB_Word_TagInt)
#define GB_Int_Neg(x)				GB_Int_Sub(GB_Int0,x)

%%]
#define GB_Int_Div(x,y)				(((x)-GB_Word_TagInt) / ((y)/GB_Int_ShiftPow2) + GB_Word_TagInt)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instruction opcode inline operands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
/* Location variant: load source, store destination, extensive variant */
#define GB_InsOp_LocE_TOS			0x0
#define GB_InsOp_LocE_Reg			0x1
#define GB_InsOp_LocE_Imm			0x2
#define GB_InsOp_LocE_PC			0x3

/* Location variant: load destination, store source, brief variant */
#define GB_InsOp_LocB_TOS			0x0
#define GB_InsOp_LocB_Reg			0x1

/* Location variant: operator destination */
#define GB_InsOp_LocO_TOS			0x0
#define GB_InsOp_LocO_Reg			0x1
#define GB_InsOp_LocO_SP			0x2
#define GB_InsOp_LocO_PC			0x3

/* Operator kind/type */
#define GB_InsOp_TyOp_Add			0x0
#define GB_InsOp_TyOp_Sub			0x1
#define GB_InsOp_TyOp_Mul			0x2
#define GB_InsOp_TyOp_Div			0x3

/* Operator data kind/type */
#define GB_InsOp_DataOp_I1			0x0
#define GB_InsOp_DataOp_I2			0x1
#define GB_InsOp_DataOp_F1			0x2
#define GB_InsOp_DataOp_F2			0x3
#define GB_InsOp_DataOp_IW			0x4
#define GB_InsOp_DataOp_II			0x5

/* Immediate constant size */
#define GB_InsOp_ImmSz_08			0x0
#define GB_InsOp_ImmSz_16			0x1
#define GB_InsOp_ImmSz_32			0x2
#define GB_InsOp_ImmSz_64			0x3

/* Indirection level, deref times */
#define GB_InsOp_Deref0				0x0
#define GB_InsOp_Deref1				0x1
#define GB_InsOp_Deref2				0x2
#define GB_InsOp_DerefInt			0x3

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Groups/categories/prefixes of/for instruction opcodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Ins_Prefix(pre,sh)					(Cast(GB_Byte,pre) << (sh))

#define GB_Ins_PreLd							GB_Ins_Prefix(0x0,7)
#define GB_Ins_PreSt							GB_Ins_Prefix(0x4,5)
#define GB_Ins_PreArith							GB_Ins_Prefix(0x5,5)
#define GB_Ins_PreCall							GB_Ins_Prefix(0x18,3)
#define GB_Ins_PreHeap							GB_Ins_Prefix(0x1D,3)
#define GB_Ins_PreEvAp							GB_Ins_Prefix(0x1C,3)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instruction opcodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Ins_Ld(indLev,locB,locE,immSz)		(GB_Ins_PreLd | ((indLev) << 5) | ((locB) << 4) | ((locE) << 2) | ((immSz) << 0))
#define GB_Ins_Call(locB)						(GB_Ins_PreCall | ((0x0) << 1) | ((locB) << 0))
#define GB_Ins_TailCall(locB)					(GB_Ins_PreCall | ((0x1) << 1) | ((locB) << 0))
#define GB_Ins_RetCall							(GB_Ins_PreCall | ((0x2) << 1))
#define GB_Ins_RetCase							(GB_Ins_PreCall | ((0x2) << 1) | 0x1)
#define GB_Ins_CaseCall 						(GB_Ins_PreCall | ((0x3) << 1))
#define GB_Ins_CallC	 						(GB_Ins_PreCall | ((0x3) << 1) | 0x1)
#define GB_Ins_AllocStore(locB)					(GB_Ins_PreHeap | ((0x2) << 1) | ((locB) << 0))
#define GB_Ins_Fetch(locB)						(GB_Ins_PreHeap | ((0x3) << 1) | ((locB) << 0))
#define GB_Ins_Eval(locB)						(GB_Ins_PreEvAp | ((0x0) << 1) | ((locB) << 0))
#define GB_Ins_Apply(locB)						(GB_Ins_PreEvAp | ((0x1) << 1) | ((locB) << 0))
#define GB_Ins_TailEval(locB)					(GB_Ins_PreEvAp | ((0x2) << 1) | ((locB) << 0))
#define GB_Ins_Op(opTy,locO)					(GB_Ins_PreArith | ((0x1) << 4) | ((opTy) << 2) | ((locO) << 0))
#define GB_Ins_OpExt(dtTy,indLev,locE,immSz)	(((dtTy) << 5) | ((indLev) << 4) | ((locE) << 2) | ((immSz) << 0))
#define GB_Ins_FetchUpdate						0xF9
#define GB_Ins_EvalApplyCont					0xFA
#define GB_Ins_PApplyCont						0xFB
#define GB_Ins_Ldg								0xFC
#define GB_Ins_EvalUpdCont						0xFD
#define GB_Ins_Ext								0xFE
#define GB_Ins_NOP								0xFF
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extended instruction opcodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_InsExt_Halt							0xFF
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface with interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern GB_Byte gb_code_Eval[] ;

extern void gb_push( GB_Word x ) ;
extern GB_Word gb_eval( GB_Word x ) ;

extern unsigned int gb_StepCounter ;

extern void gb_interpretLoop( ) ;
extern void gb_interpretLoopWith( GB_BytePtr initPC ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void gb_Initialize() ;

extern void gb_InitTables
	( int byteCodesSz
	, GB_BytePtr byteCodes
	, int linkEntriesSz
	, GB_LinkEntry* linkEntries
	, GB_BytePtr* globalEntries
	, int cafEntriesSz
	, GB_BytePtr** cafEntries
	, int fixOffsetsSz
	, GB_FixOffset* fixOffsets
	, GB_Word* consts
%%[[12
	, GB_NodePtr impNode
	, GB_NodePtr expNode
	, GB_ModEntry* modTbl
%%]]
	) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern int gb_Opt_TraceSteps ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Sanity check on assumptions made by interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void gb_checkInterpreterAssumptions() ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dumping, tracing. printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if DUMP_INTERNALS
extern void gb_prState( char* msg, int maxStkSz ) ;
#endif
%%]

%%[8
#define IF_GB_TR_ON(l,x)		IF_TR_ON(l,if (gb_Opt_TraceSteps) { x })
%%]



