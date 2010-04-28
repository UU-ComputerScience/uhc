%%[8
#ifndef __BC_INTERPRETER_H__
#define __BC_INTERPRETER_H__
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_TOS					(*sp)
#define GB_SetTOS(x)			{*sp = Cast(GB_Word,x);}
#define GB_Push(v)				{*(--sp) = (Cast(GB_Word,v)) ; }

#define GB_Popn(n)				(sp+=(n))
#define GB_Pop					(sp++)
#define GB_PopCastIn(ty,v)		{(v) = Cast(ty,*GB_Pop) ;}
#define GB_PopIn(v)				GB_PopCastIn(GB_Word,v)
%%]

%%[8
#define GB_Stack_SzAvailable			((Word)sp - (Word)StackAreaLow - STACKSIZE_SPARE_UNUSED)
#define GB_Stack_SzIsAvailable(sz)		((Word)(sz) <= GB_Stack_SzAvailable)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Base pointer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_BP_Set				{ bp = sp ; }
#define GB_BP_Link				{ GB_Push(bp) ; GB_BP_Set ; }
#define GB_BP_Unlink			{ bp = Cast(GB_Ptr,GB_Deref(bp)) ; }
#define GB_BP_UnlinkSP			{ sp = bp ; GB_BP_Unlink ; sp++ ; }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% General pointer/register access
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_RegRelCast(ty,r,o)		(Cast(ty*,r)+(o))
#define GB_RegRel(r,o)				((r)+(o))
#define GB_RegByteRelCastx(ty,r,o)	GB_DerefCast(ty,GB_RegRelCast(GB_Byte,r,o))
#define GB_RegRelx(r,o)				GB_Deref(GB_RegRel(r,o))

#define GB_RegByteRel(ty,r,o)		Cast(ty*,((Cast(GB_BytePtr,r))+(o)))
#define GB_RegByteRelx(r,o)			GB_Deref(GB_RegByteRel(GB_Word,r,o))
%%]

%%[8
#define GB_SPRel(o)					GB_RegRel(sp,o)
#define GB_SPRelx(o)				GB_Deref(GB_SPRel(o))
%%]

%%[8
#define GB_PCRel(o)					GB_RegRel(pc,o)

#define GB_SetRegRel(r,o,v)			{ *GB_RegRel(r,o) = v ; }
#define GB_SetRegByteRel(ty,r,o,v)	{ *GB_RegByteRel(ty,r,o) = v ; }
#define GB_SetCallCResult(tys,tyv,r,o,v)	{ *GB_RegByteRel(tyv,r,o) = *Cast(tyv*,Cast(void*,&v)) ; }
%%]




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alloc routines for GMP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
#if USE_GMP
extern void* gb_Alloc_GMP( size_t nBytes ) ;
extern void* gb_ReAlloc_GMP( void *n, size_t nBytesOld, size_t nBytes ) ;
extern void gb_Free_GMP( void *n, size_t nBytesOld ) ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Comparing Node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Equality on nodes which are an instance of Enum, hence are encoded as GB_Int.

%%[98
#define GB_EnumIsEqual(x,y)					((x) == (y))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PackedString
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Tag_PackedString					0

#define GB_MkPackedString(n,x)		 		GB_MkConNode1(n,GB_Tag_PackedString,x)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Tag_List_Nil						1
#define GB_Tag_List_Cons					0

#define GB_MkListNil(n)						{n = gb_Nil;}
#define GB_MkListCons(n,x1,x2)				GB_MkConNode2(n,GB_Tag_List_Cons,x1,x2)

#define GB_List_IsNull(n)					(GB_Word_IsPtr(n) && GB_NH_Fld_Tag((n)->header) == GB_Tag_List_Nil)
#define GB_List_Head(n)						((n)->content.fields[0])
#define GB_List_Tail(n)						Cast( GB_NodePtr,(n)->content.fields[1] )
#define GB_List_TailPtr(n)					Cast( GB_NodePtr*,&(n)->content.fields[1] )

#define GB_List_Iterate(n,evaln,sz,body)	while ( sz-- && gb_assert_IsNotIndirection(Cast(Word,n = evaln),"GB_List_Iterate") && ! GB_List_IsNull( n ) ) { \
												body ; \
												n = GB_List_Tail(n) ; \
											}

%%]

%%[8
extern GB_NodePtr gb_listForceEval( GB_NodePtr* pn, int* psz ) ;
extern GB_NodePtr gb_listForceEval2( GB_NodePtr n, int* psz ) ;
%%]

%%[98
extern GB_NodePtr gb_copyCStringFromEvalString( char* cString, GB_NodePtr hsString, int sz ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Maybe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98
#define GB_Tag_Maybe_Nothing				1
#define GB_Tag_Maybe_Just					0

#define GB_MkMaybeNothing(n)				{n = gb_Nothing;}
#define GB_MkMaybeJust(n,x1)				GB_MkConNode1(n,GB_Tag_Maybe_Just,x1)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exceptions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
extern GB_NodePtr gb_ThrownException ;
extern int gb_ThrownException_NrOfEvalWrappers ;

#define GB_PassExcWith(action,gcsafe,extratest,whenexc) \
											{ \
												action	; \
												if ( gb_ThrownException != NULL && extratest ) {\
													IF_GB_TR_ON(3,{printf("GB_PassExcWith exc=%p\n",gb_ThrownException) ;}) ; \
													gcsafe ; \
													whenexc ; \
												} \
											}

#define GB_PassExc(action)					GB_PassExcWith(action,,True,return gb_ThrownException)
#define GB_PassExc_GCSafe(action)			GB_PassExcWith(action,GB_GCSafe_Leave,True,return gb_ThrownException)
#define GB_PassExc_Dflt(df,action)			GB_PassExcWith(action,,True,return df)
#define GB_PassExc_Dflt_GCSafe(df,action)	GB_PassExcWith(action,GB_GCSafe_Leave,True,return df)
#define GB_PassExc_Cast(tp,action)			GB_PassExcWith(action,,True,return Cast(tp,gb_ThrownException))
#define GB_PassExc_Cast_GCSafe(tp,action)	GB_PassExcWith(action,GB_GCSafe_Leave,True,return Cast(tp,gb_ThrownException))
#define GB_PassExc_CastAsWord(action)		GB_PassExc_Cast(GB_Word,action)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IOError
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Definition must match the one in Prelude.hs

%%[96
#define GB_Tag_IOError_IOError							0

#define GB_MkIOErrorIOError(n,x1,x2,x3,x4,x5)			GB_MkConNode5(n,GB_Tag_IOError_IOError,x1,x2,x3,x4,x5)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Definitions must match the one in Prelude.hs

Async exceptions

%%[96
#define GB_Tag_AsyncException_HeapOverflow   				0
#define GB_Tag_AsyncException_StackOverflow   				1
#define GB_Tag_AsyncException_ThreadKilled	  				2

#define GB_MkAsyncException_HeapOverflow(n)					GB_MkConNode0(n,GB_Tag_AsyncException_HeapOverflow   )
#define GB_MkAsyncException_StackOverflow(n,s)				GB_MkConNode1(n,GB_Tag_AsyncException_StackOverflow,s)
#define GB_MkAsyncException_ThreadKilled(n)					GB_MkConNode0(n,GB_Tag_AsyncException_ThreadKilled   )
%%]

Plain exceptions

%%[96
#define GB_Tag_Exception_ArithException   					0
#define GB_Tag_Exception_ArrayException   					1
#define GB_Tag_Exception_AssertionFailed  					2
#define GB_Tag_Exception_AsyncException   					3
#define GB_Tag_Exception_BlockedOnDeadMVar					4
#define GB_Tag_Exception_Deadlock         					5
#define GB_Tag_Exception_ErrorCall        					6
#define GB_Tag_Exception_ExitException    					7
#define GB_Tag_Exception_IOException       					8
#define GB_Tag_Exception_NoMethodError    					9
#define GB_Tag_Exception_NonTermination   					10
#define GB_Tag_Exception_PatternMatchFail 					11
#define GB_Tag_Exception_RecConError      					12
#define GB_Tag_Exception_RecSelError      					13
#define GB_Tag_Exception_RecUpdError      					14

#define GB_MkExceptionArithException(n,x1)					GB_MkConNode1(n,GB_Tag_Exception_ArithException   ,x1)
#define GB_MkExceptionArrayException(n,x1)					GB_MkConNode1(n,GB_Tag_Exception_ArrayException   ,x1)
#define GB_MkExceptionAssertionFailed(n,x1)					GB_MkConNode1(n,GB_Tag_Exception_AssertionFailed  ,x1)
#define GB_MkExceptionAsyncException(n,x1)					GB_MkConNode1(n,GB_Tag_Exception_AsyncException   ,x1)
#define GB_MkExceptionBlockedOnDeadMVar(n)					GB_MkConNode0(n,GB_Tag_Exception_BlockedOnDeadMVar   )
#define GB_MkExceptionDeadlock(n)							GB_MkConNode0(n,GB_Tag_Exception_Deadlock            )
#define GB_MkExceptionErrorCall(n,x1)						GB_MkConNode1(n,GB_Tag_Exception_ErrorCall        ,x1)
#define GB_MkExceptionExitException(n,x1)					GB_MkConNode1(n,GB_Tag_Exception_ExitException    ,x1)
#define GB_MkExceptionIOError(n,x1)							GB_MkConNode1(n,GB_Tag_Exception_IOException      ,x1)
#define GB_MkExceptionNoMethodError(n,x1)					GB_MkConNode1(n,GB_Tag_Exception_NoMethodError    ,x1)
#define GB_MkExceptionNonTermination(n)						GB_MkConNode0(n,GB_Tag_Exception_NonTermination      )
#define GB_MkExceptionPatternMatchFail(n,x1)				GB_MkConNode1(n,GB_Tag_Exception_PatternMatchFail ,x1)
#define GB_MkExceptionRecConError(n,x1)						GB_MkConNode1(n,GB_Tag_Exception_RecConError      ,x1)
#define GB_MkExceptionRecSelError(n,x1)						GB_MkConNode1(n,GB_Tag_Exception_RecSelError      ,x1)
#define GB_MkExceptionRecUpdError(n,x1)						GB_MkConNode1(n,GB_Tag_Exception_RecUpdError      ,x1)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Debug/symbolic/trace info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef struct GB_ByteCodeInstrEntry {
  GB_BytePtr				bcLoc ;
  GB_Byte					bcSize ;
  char*						bc ;
} GB_ByteCodeInstrEntry ;

typedef struct GB_ByteCodeEntryPoint {
  Word8*					nm ;
  GB_ByteCodeInstrEntry*	bcInstr ;
  GB_Word					bcInstrSize ;
} GB_ByteCodeEntryPoint ;

typedef struct GB_ByteCodeModule {
  char*						bcModNm ;
  GB_ByteCodeEntryPoint*	bcEntry ;
  GB_Word					bcEntrySize ;
  GB_BytePtr				bcLoc ;
  GB_Word				 	bcSize ;
  // GB_GCInfo*				bcGCInfo ;
  // Word32				 	bcGCInfoSize ;
} GB_ByteCodeModule ;
%%]

%%[8
// extern void gb_prWordAsNode( GB_NodePtr n ) ;
extern void gb_prWord( GB_Word x ) ;

#if TRACE || DUMP_INTERNALS
extern void gb_prByteCodeInstrEntry( GB_ByteCodeInstrEntry* e ) ;
extern void gb_prByteCodeModule( GB_ByteCodeModule* e ) ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PackedString
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Tag_PackedString					0
#define GB_Tag_PackedStringOffset			1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Linking, fixing addresses, module info, EntryKind should correspond to %{GRIN}GrinByteCode.LinkTbl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
#define GB_LinkTbl_EntryKind_Const					0			/* obsolete (now via PatchCode_Deref1): constant */
#define GB_LinkTbl_EntryKind_CodeEntry				1			/* obsolete (now via Patch...): code entry */
#define GB_LinkTbl_EntryKind_PatchCode_Deref0		2			/* patch code with value */
#define GB_LinkTbl_EntryKind_PatchCode_Deref1		3			/* patch code with *value */
#define GB_LinkTbl_EntryKind_PatchCode_Deref2		4			/* patch code with **value */
#define GB_LinkTbl_EntryKind_PatchOffsets			5			/* patch code containing offsets with abolute address */
#define GB_LinkTbl_EntryKind_ImpEntry				6			/* import entry */

Link commands for global references

%%[8
%%]
typedef struct GB_LinkEntry {
  HalfWord		tblKind ;
  GB_Ptr		linkLoc ;
  GB_Word		linkVal ;
} GB_LinkEntry ;

Module info

%%[8
typedef struct GB_ModEntry {
  char*						name ;
%%[[20
  GB_NodePtr*				expNode ;
%%]]
  GB_ByteCodeModule*		bcModule ;
  GB_FunctionInfo*			functionInfos ;
} GB_ModEntry ;

extern int gb_lookupModEntry( char* modNm, GB_ModEntry* modTbl ) ;

%%]

Imported module info: binding of module name to position in global module table (a GB_ModEntry[])

%%[20
typedef struct GB_ImpModEntry {
  char*						name ;			// name of module
  Word						globModInx ;	// index global table of GB_ModEntry, filled in at link time
} GB_ImpModEntry ;
%%]

%%[8
extern int gb_lookupInfoForPC
	( GB_BytePtr pc
	, GB_ByteCodeModule** m
	, GB_ByteCodeEntryPoint** e
	, GB_ByteCodeInstrEntry** i ) ;
%%]

%%[8
#if TRACE || DUMP_INTERNALS
extern void gb_prModEntries( GB_ModEntry* modTbl ) ;
#endif
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Before each bytecode function start 'function information' is stored, a GB_FunctionInfo*.

%%[8
#define GB_FunctionInfo_Inline				Word		// A GB_FunctionInfo*
%%]

Retrieval of function info given a pc

%%[8
#define GB_FromPCToFunctionInfo(p)			Cast(GB_FunctionInfo*,GB_Deref((Word)(p) - sizeof(GB_FunctionInfo_Inline)))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Call information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

After each call 'call information' is stored.
Invariant is that a return address points to the address immediately after this information.
The type and size used should agree with the code generation part.

%%[8
typedef struct GB_CallInfo_CCall {
  char*		type ;
} GB_CallInfo_CCall ;

typedef struct GB_CallInfo {
	Word8	 				kind ;
	Word8*   				name ;					// name of called function (to become obsolete when functionInfo works)
	// GB_FunctionInfo*		functionInfo ;			// info about the called function (20100301 AD: under implementation)
	GB_FunctionInfo_Inx		functionInfoModOff ;	// offset in imported module table, replaced at linking time with index into global module table
	GB_FunctionInfo_Inx		functionInfoOff ;		// offset in per module FunctionInfo table
	GB_GCStackInfo*			gcStackInfo ;
#if TRACE
	GB_CallInfo_CCall		ccall ;
#endif
} __attribute__ ((__packed__)) GB_CallInfo ;

typedef GB_CallInfo* GB_CallInfoPtr ;

#define GB_CallInfo_Inline				GB_Word		// A GB_CallInfoPtr, inlined after instruction, to be skipped by interpreter, used by exception handling & debugging

#if TRACE
#define GB_MkCallInfoWith(k,n,mo,fo,gc,w)		{k,(BPtr)n,mo,fo,gc,w}		// make CallInfo
#else
#define GB_MkCallInfoWith(k,n,mo,fo,gc,w)		{k,(BPtr)n,mo,fo,gc}		// make CallInfo
#endif
#define GB_MkCallInfo(k,n)				GB_MkCallInfoWith(k,n,-1,-1,NULL,NULL)

#define GB_CallInfo_Fld_Kind(i)    		i

// the order must correspond to alternatives of CallInfoKind in ehc/GrinByteCode, extra ones may be at the end
#define GB_CallInfo_Kind_Call    		0			// normal call
#define GB_CallInfo_Kind_Tail    		1			// tail call
#define GB_CallInfo_Kind_Eval    		2			// eval call
#define GB_CallInfo_Kind_EvalWrap  		3			// eval call wrapper
#define GB_CallInfo_Kind_TailEv  		4			// tail eval call
#define GB_CallInfo_Kind_Apply   		5			// apply call
#define GB_CallInfo_Kind_CCall   		6			// C call
#define GB_CallInfo_Kind_EvCont  		7			// eval update continuation
#define GB_CallInfo_Kind_ApCont  		8			// apply continuation
#define GB_CallInfo_Kind_PApCont  		9			// partial apply continuation
#define GB_CallInfo_Kind_Hdlr    		10			// exception handler installment
#define GB_CallInfo_Kind_TailEval		11			// tail eval
// following may be defined freely
#define GB_CallInfo_Kind_EvAppFunCont  	12			// apply fun eval update continuation
#define GB_CallInfo_Kind_EvAppFunEvCont 13			// apply fun eval update eval continuation
#define GB_CallInfo_Kind_EvalTopWrap  	14			// top level eval call wrapper
#define GB_CallInfo_Kind_TailEvalCont  	15			// return/cleanup after taileval
%%[[96
#define GB_CallInfo_Kind_IntlCCall		16			// internal C call which must look like foreign function call (for exception handling)
%%]]
%%]

Flags

%%[8
%%]
#define GB_CallInfo_Flag_None				0			// nothing, nada
#define GB_CallInfo_Flag_ExplStackTrace		1			// this function takes as its first arg

Retrieval of call info given a bp

%%[8
#define GB_FromBPToCallInfo(p)			Cast(GB_CallInfo*,GB_Deref(GB_RegRelx(p,1) - sizeof(GB_CallInfo_Inline)))
%%]

%%[8
extern Bool gb_CallInfo_Kind_IsVisible( Word kind ) ;
extern GB_FunctionInfo* gb_CallInfo_GetFunctionInfo( GB_ModEntry* allMod, GB_CallInfo* ci ) ;
extern Word8* gb_CallInfo_GetName( GB_ModEntry* allMod, GB_CallInfo* ci ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Calling convention
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_CallRetNrWords				2			// return address + bp link
#define GB_CallRetNrBytes				(GB_CallRetNrWords * sizeof(GB_Word))
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GB_Int arithmetic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_GBInt2CastedInt(ty,x)	(Cast(ty,(x)&GB_Word_IntMask) / GB_Int_ShiftPow2)
#define GB_GBInt2Int(x)				GB_GBInt2CastedInt(SWord,x)
#define GB_TagWord2Word(x)			(((x)<<GB_Word_SizeOfWordTag) | GB_Word_TagInt)
#define GB_UntagWord2Word(x)		((x)>>GB_Word_SizeOfWordTag)
#define GB_Int2GBInt(x)				((Cast(GB_Int,x)) << GB_Word_SizeOfWordTag | GB_Word_TagInt)

#define GB_Int0						GB_Int2GBInt(0)
#define GB_Int1						GB_Int2GBInt(1)
#define GB_Int2						GB_Int2GBInt(2)

#define GB_Int_Zero					GB_Int0
#define GB_Int_One					GB_Int1
#define GB_Int_Two					GB_Int2

#define GB_Int_Add(x,y)				((x) + (y) - GB_Word_TagInt)
#define GB_Int_Sub(x,y)				((x) - (y) + GB_Word_TagInt)
#define GB_Int_Mul(x,y)				(((x)-GB_Word_TagInt) * ((y)/GB_Int_ShiftPow2) + GB_Word_TagInt)
#define GB_Int_Quot(x,y)			GB_Int2GBInt( GB_GBInt2Int(x) / GB_GBInt2Int(y) )
#define GB_Int_Rem(x,y)				GB_Int2GBInt( GB_GBInt2Int(x) % GB_GBInt2Int(y) )
#define GB_Int_Neg(x)				GB_Int_Sub(GB_Int_Zero,x)

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instruction opcode inline operands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
/* Location variant: load source, store destination, extensive variant */
#define GB_InsOp_LocE_SP			0x0
#define GB_InsOp_LocE_Reg			0x1
#define GB_InsOp_LocE_Imm			0x2
#define GB_InsOp_LocE_PC			0x3

/* Location variant: load destination, store source, brief variant */
#define GB_InsOp_LocB_TOS			0x0
#define GB_InsOp_LocB_Reg			0x1

/* Location variant: operator destination */
#define GB_InsOp_LocODst_TOS		0x0
#define GB_InsOp_LocODst_Reg		0x1

/* Location variant: operator source */
#define GB_InsOp_LocOSrc_SP			0x0
#define GB_InsOp_LocOSrc_Reg		0x1
#define GB_InsOp_LocOSrc_Imm		0x2
#define GB_InsOp_LocOSrc_TOS		0x3

/* Operator kind/type */
#define GB_InsOp_TyOp_Add			0x0
#define GB_InsOp_TyOp_Sub			0x1
#define GB_InsOp_TyOp_Mul			0x2
#define GB_InsOp_TyOp_Quot			0x3

/* Operator data kind/type */
#define GB_InsOp_DataOp_IW			0x0
#define GB_InsOp_DataOp_II			0x1
#define GB_InsOp_DataOp_FW			0x2

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

/* Indirection level, deref times, brief variant */
#define GB_InsOp_DerefB1			0x0
#define GB_InsOp_DerefB2			0x1

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Groups/categories/prefixes of/for instruction opcodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Ins_Prefix(pre,sh)					(Cast(GB_Byte,pre) << (sh))

#define GB_Ins_PreLd							GB_Ins_Prefix(0x0,7)
#define GB_Ins_PreSt							GB_Ins_Prefix(0x2,6)
#define GB_Ins_PreArith							GB_Ins_Prefix(0x6,5)
#define GB_Ins_PreEvAp							GB_Ins_Prefix(0x1C,3)
#define GB_Ins_PreHeap							GB_Ins_Prefix(0x1D,3)
#define GB_Ins_PreCall							GB_Ins_Prefix(0x1E,3)
#define GB_Ins_PreOther							GB_Ins_Prefix(0x1F,3)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instruction opcodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Ins_Ld(indLev,locB,locE,immSz)		(GB_Ins_PreLd | ((indLev) << 5) | ((locB) << 4) | ((locE) << 2) | ((immSz) << 0))
#define GB_Ins_St(indLev,locE,locB,immSz)		(GB_Ins_PreSt | ((indLev) << 5) | ((locE) << 3) | ((locB) << 2) | ((immSz) << 0))
#define GB_Ins_Call(locB)						(GB_Ins_PreCall | ((0x0) << 1) | ((locB) << 0))
// moved to extension
// #define GB_Ins_TailCall(locB)					(GB_Ins_PreCall | ((0x1) << 1) | ((locB) << 0))
#define GB_Ins_RetCall							(GB_Ins_PreCall | ((0x2) << 1) | 0x0)
#define GB_Ins_RetCase							(GB_Ins_PreCall | ((0x2) << 1) | 0x1)
#define GB_Ins_CaseCall 						(GB_Ins_PreCall | ((0x3) << 1) | 0x0)
#define GB_Ins_CallC	 						(GB_Ins_PreCall | ((0x3) << 1) | 0x1)
#define GB_Ins_Split(locB)						(GB_Ins_PreHeap | ((0x0) << 1) | ((locB) << 0))
#define GB_Ins_Adapt(locB)						(GB_Ins_PreHeap | ((0x1) << 1) | ((locB) << 0))
#define GB_Ins_AllocStore(locB)					(GB_Ins_PreHeap | ((0x2) << 1) | ((locB) << 0))
#define GB_Ins_Fetch(locB)						(GB_Ins_PreHeap | ((0x3) << 1) | ((locB) << 0))
#define GB_Ins_Eval(locB)						(GB_Ins_PreEvAp | ((0x0) << 1) | ((locB) << 0))
#define GB_Ins_Apply(locB)						(GB_Ins_PreEvAp | ((0x1) << 1) | ((locB) << 0))
// moved to extension
// #define GB_Ins_TailEval(locB)					(GB_Ins_PreEvAp | ((0x2) << 1) | ((locB) << 0))
#define GB_Ins_Ldg(locB)						(GB_Ins_PreEvAp | ((0x3) << 1) | ((locB) << 0))
#define GB_Ins_Op(opTy,dtTy,locO)				(GB_Ins_PreArith | ((opTy) << 3) | ((dtTy) << 1) | ((locO) << 0))
#define GB_Ins_OpExt(indLev,locE,immSz)			(((indLev) << 4) | ((locE) << 2) | ((immSz) << 0))
#define GB_Ins_FetchUpdate						(GB_Ins_PreOther | 0x1)
#define GB_Ins_EvalApplyCont					(GB_Ins_PreOther | 0x2)
#define GB_Ins_PApplyCont						(GB_Ins_PreOther | 0x3)
#define GB_Ins_LdNodeTag						(GB_Ins_PreOther | 0x4)
#define GB_Ins_EvalUpdCont						(GB_Ins_PreOther | 0x5)
#define GB_Ins_Ext								(GB_Ins_PreOther | 0x6)
#define GB_Ins_TailEvalCont						(GB_Ins_PreOther | 0x7)
// #define GB_Ins_NOP								(GB_Ins_PreOther | 0x7)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extended instruction opcodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Split into 2 groups:
- hi bit == 0: tail variants, ...
- hi bit == 1: various others

%%[8
#define GB_InsTail_Call(locB)					(((0x0) << 1) | ((locB) << 0))
#define GB_InsTail_Eval(locB)					(((0x1) << 1) | ((locB) << 0))
#define GB_InsTail_Apply(locB)					(((0x2) << 1) | ((locB) << 0))
%%]

%%[8
#define GB_InsExt_Halt							0xFF
#define GB_InsExt_TagInt2Word					0xFC
#define GB_InsExt_UntagWord2Int					0xFD
#define GB_InsExt_TagWord2Word					0xFA
#define GB_InsExt_UntagWord2Word				0xFB
%%]

%%[96
#define GB_InsExt_ResetThrownException			0xFE
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface with interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern GB_Byte gb_code_Eval[] ;

extern void gb_push( GB_Word x ) ;
extern GB_Word gb_eval( GB_Word x ) ;

#if GB_COUNT_STEPS
extern unsigned long gb_StepCounter ;
#endif

extern void gb_interpretLoop( ) ;
extern void gb_interpretLoopWith( GB_BytePtr initPC ) ;
%%]

%%[96
extern void gb_unlinkSP() ;
extern void gb_setPC( GB_BytePtr c ) ;
%%]

%%[99
%%]
extern GB_Byte gb_code_Startup[] ;

%%[99
extern void gb_runIO( GB_Word io ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
extern GB_Word gb_intl_primCatchException( GB_Word e, GB_Word handler ) ;
extern GB_NodePtr gb_intl_throwException( GB_Word exc ) ;
extern GB_NodePtr gb_intl_throwExceptionFromPrim( GB_NodePtr exc ) ;
extern GB_NodePtr gb_intl_throwIOErrorFromPrim( GB_NodePtr ioe_handle, GB_Word ioe_type, GB_NodePtr ioe_filename, char* strErr ) ;
extern GB_NodePtr gb_intl_throwStackOverflow( GB_FunctionInfo* functionInfo ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO Channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extern GB_NodePtr gb_chan_stdin ;
extern GB_NodePtr gb_chan_stdout ;
extern GB_NodePtr gb_chan_stderr ;

%%[98
extern void gb_chan_initstd() ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void gb_Initialize() ;

extern void gb_InitTables
	( GB_BytePtr byteCodes, int byteCodesSz
	, HalfWord* cafGlEntryIndices, int cafGlEntryIndicesSz
	, GB_BytePtr* globalEntries, int globalEntriesSz
	, GB_Word* consts
	// , GB_GCInfo* gcInfos
	, GB_GCStackInfo* gcStackInfos
	, GB_LinkChainResolvedInfo* linkChainInds
	, GB_CallInfo* callinfos, int callinfosSz
	, GB_FunctionInfo* functionInfos, int functionInfosSz
	, BPtr bytePool
	, Word linkChainOffset
%%[[20
	, GB_ImpModEntry* impModules, int impModulesSz
	, GB_NodePtr* expNode, int expNodeSz, int* expNodeOffs
	, GB_ModEntry* modTbl, Word modTblInx
%%]]
	) ;
%%]

%%[8
extern void gb_SetModTable( GB_ModEntry* modTbl, GB_Word modTblSz ) ;
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void gb_exit( int i ) ;
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
#if TRACE || DUMP_INTERNALS
extern void gb_prWord( GB_Word x ) ;
extern void gb_prTOSAsInt( ) ;
#endif
%%]

%%[8
#if DUMP_INTERNALS
extern void gb_prState( char* msg, int maxStkSz ) ;
#endif
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BC_INTERPRETER_H__ */
%%]
