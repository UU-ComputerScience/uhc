%%[8
#ifndef __BC_INTERPRETER_H__
#define __BC_INTERPRETER_H__
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// top of stack
#define GB_TOS					(*sp)
#define GB_SetTOS(x)			{*sp = Cast(GB_Word,x);}

// pushing
#define GB_ShftPush(v)			{*(--sp) = (Cast(GB_Word,v)) ; }
#define GB_TyPushShft(ty,v)		{ty* sp_ = (ty*)sp; sp_--; *sp_ = Cast(ty,v); sp = (GB_Word*)sp_;}		/* avoid predecrement */
#define GB_PushShft(v)			GB_TyPushShft(GB_Word,v)
#define GB_Push(v)				GB_ShftPush(v)

// pushing multiple values
#define GB_PushFromTo(plo,phi)	/* push memory area on stack */ 				\
								MemCopyBackward(phi,plo,sp) ;

#define GB_PushNodeArgs(nd,hdr,phi,plo)	/* push args of `fun + args' node fields */ 				\
								phi = Cast(GB_Ptr,&((nd)->content.fields[GB_NH_NrFlds(hdr)])) ;	\
								plo = Cast(GB_Ptr,&((nd)->content.fields[1])) ;								\
								IF_GB_TR_ON(3,printf("GB_PushNodeArgs:MemCopyBackward plo %p phi %p sp %p\n",plo,phi,sp);) ;			\
								GB_PushFromTo(plo,phi) ;

// popping
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
// Reg access

// reg relative using the type to influence the offset multiplication
#define GB_RegRelCast(ty,r,o)		(Cast(ty*,r)+(o))
// reg relative using the type of r to (implicitly) influence the offset multiplication
#define GB_RegRel(r,o)				((r)+(o))
// byte relative, content size determined by ty
#define GB_RegByteRelCastx(ty,r,o)	GB_DerefCast(ty,GB_RegRelCast(GB_Byte,r,o))
// same, but arithmetic determined by r
#define GB_RegRelx(r,o)				GB_Deref(GB_RegRel(r,o))

#define GB_RegByteRel(ty,r,o)		Cast(ty*,((Cast(GB_BytePtr,r))+(o)))
#define GB_RegByteRelx(r,o)			GB_Deref(GB_RegByteRel(GB_Word,r,o))
%%]

%%[8
// Reg setting
#define GB_SetReg(r,x)				{r = Cast(GB_Word,x);}
#define GB_SetRegRel(r,o,v)			{ *GB_RegRel(r,o) = v ; }
#define GB_SetRegByteRel(ty,r,o,v)	{ *GB_RegByteRel(ty,r,o) = v ; }
%%]

%%[8
// SP relative
#define GB_SPRel(o)					GB_RegRel(sp,o)
#define GB_SPRelx(o)				GB_Deref(GB_SPRel(o))

#define GB_SPByteRel(ty,o)			GB_RegByteRel(ty,sp,o)
#define GB_SPByteRelx(o)			GB_Deref(GB_SPByteRel(GB_Word,o))

#define GB_SetSPRel(o,v)			GB_SetRegRel(sp,o,v)
#define GB_SetSPByteRel(o,v)		GB_SetRegRel(sp,o,v)

// TOS relative
#define GB_SetTOSRel(o,x)			{ *GB_SPRel(o) = (x); }
#define GB_SetTOSByteRel(o,x)		{ *Cast(GB_Ptr,GB_SPByteRel(GB_Word,o)) = (x); }
#define GB_TOSCastedIn(ty,v)		{(v) = Cast(ty,GB_TOS) ;}

#define GB_PopnUpdTOS(n,x)			{sp += (n) ; GB_SetTOS(x) ; }
%%]

%%[8
// PC
#define GB_PCRel(o)					GB_RegRel(pc,o)

%%]

%%[8
// Call result
#define GB_SetCallCResult(tys,tyv,r,o,v)	{ *GB_RegByteRel(tys,r,o) = *Cast(tyv*,Cast(void*,&v)) ; }
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
  FunctionInfo*			functionInfos ;
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

Before each bytecode function start 'function information' is stored, a FunctionInfo*.

%%[8
#define FunctionInfo_Inline				Word		// A FunctionInfo*
%%]

Retrieval of function info given a pc

%%[8
#define GB_FromPCToFunctionInfo(p)			Cast(FunctionInfo*,GB_Deref((Word)(p) - sizeof(FunctionInfo_Inline)))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Call information retrieval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Retrieval of call info given a bp

%%[8
#define GB_FromBPToCallInfo(p)			Cast(CallInfo*,GB_Deref(GB_RegRelx(p,1) - sizeof(CallInfo_Inline)))
%%]

%%[8
extern Bool gb_CallInfo_Kind_IsVisible( Word kind ) ;
extern FunctionInfo* gb_CallInfo_GetFunctionInfo( GB_ModEntry* allMod, CallInfo* ci ) ;
extern Word8* gb_CallInfo_GetName( GB_ModEntry* allMod, CallInfo* ci ) ;
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
extern void gb_evalTos() ;
extern void gb_applyTos() ;
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
extern GB_NodePtr gb_intl_throwStackOverflow( FunctionInfo* functionInfo ) ;
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
%%% The actual Virtual Machine instruction implementations,
%%% for use directly by compiler and by the interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Naming convention:
- VM: publicly available
- VMI: if available then for interpreter only, the VM variant then uses the VMI variant via wrapper function.

Moving data.

Note: for now (20111114) the 64 bits variants are not always correctly taking a proper sized value when run on a 32 bit platform,
so these are assumed not to be used, instead the user of these macros should take care of properly pushing multiple 32 bit word values in another way.

%%[8
// push various bitsized values, most efficient variant
#define GB_VM_Push8(x)				GB_Push(x)
#define GB_VM_Push16(x)				GB_Push(x)
#define GB_VM_Push32(x)				GB_Push(x)
#if USE_64_BITS
#define GB_VM_Push64(x)				GB_Push(x)
#else
#define GB_VM_Push64(x)				GB_TyPushShft(int64_t,x)
#endif

// push various bitsized ints, most efficient variant
#define GB_VM_PushInt8(x)			GB_VM_Push8(GB_Int2GBInt(x))
#define GB_VM_PushInt16(x)			GB_VM_Push16(GB_Int2GBInt(x))
#define GB_VM_PushInt32(x)			GB_VM_Push32(GB_Int2GBInt(x))
#define GB_VM_PushInt64(x)			GB_VM_Push64(GB_Int2GBInt(x))

// push various bitsized values relative from TOS
#define GB_VM_PushTos8(x)			GB_PushShft(GB_SPByteRelx(x))
#define GB_VM_PushTos16(x)			GB_PushShft(GB_SPByteRelx(x))
#define GB_VM_PushTos32(x)			GB_PushShft(GB_SPByteRelx(x))
#if USE_64_BITS
#define GB_VM_PushTos64(x)			GB_PushShft(GB_SPByteRelx(x))
#else
#define GB_VM_PushTos64(x)			GB_TyPushShft(int64_t,GB_SPByteRelx(x))
#endif

// push various bitsized values relative from TOS, dereferenced
#define GB_VM_PushTosX8(x)			GB_PushShft(GB_RegByteRelx( GB_TOS, x ))
#define GB_VM_PushTosX16(x)			GB_PushShft(GB_RegByteRelx( GB_TOS, x ))
#define GB_VM_PushTosX32(x)			GB_PushShft(GB_RegByteRelx( GB_TOS, x ))
#if USE_64_BITS
#define GB_VM_PushTosX64(x)			GB_PushShft(GB_RegByteRelx( GB_TOS, x ))
#else
#define GB_VM_PushTosX64(x)			GB_TyPushShft(int64_t,GB_RegByteRelx( GB_TOS, x ))
#endif

// push various bitsized values relative from RR, dereferenced
#define GB_VM_PushRrX8(x)			GB_PushShft(GB_RegByteRelx( rr, x ))
#define GB_VM_PushRrX16(x)			GB_PushShft(GB_RegByteRelx( rr, x ))
#define GB_VM_PushRrX32(x)			GB_PushShft(GB_RegByteRelx( rr, x ))
#if USE_64_BITS
#define GB_VM_PushRrX64(x)			GB_PushShft(GB_RegByteRelx( rr, x ))
#else
#define GB_VM_PushRrX64(x)			GB_TyPushShft(int64_t,GB_RegByteRelx( rr, x ))
#endif

// load various bitsized values into RR, relative from TOS
#define GB_VM_LdRrTos8(x)			GB_SetReg( rr, GB_SPByteRelx(x))
#define GB_VM_LdRrTos16(x)			GB_SetReg( rr, GB_SPByteRelx(x))
#define GB_VM_LdRrTos32(x)			GB_SetReg( rr, GB_SPByteRelx(x))
#if USE_64_BITS
#define GB_VM_LdRrTos64(x)			GB_SetReg( rr, GB_SPByteRelx(x))
#else
#define GB_VM_LdRrTos64(x)			GB_SetReg( rr, GB_SPByteRelx(x))
#endif

// load global, TOS & RR variant
#define GB_VM_PushGlbl(x)			{x = *Cast(GB_Word*,x) ; GB_Push( x ) ;}
#define GB_VM_LdRrGlbl(x)			{rr = *Cast(GB_Word*,x) ;}

// load node tag
#define GB_VM_PushNdTg()			GB_PushShft(GB_Int2GBInt(GB_NH_Fld_Tag(((GB_NodePtr)GB_TOS)->header)))
%%]

Control.

%%[8
// call bytecode implementation of function, starting at bytecode address x
extern void gb_VM_CallFunBc(GB_Word x);
#define GB_VM_CallFunBc(x)			gb_VM_CallFunBc(x)

// exception: reset internal admin

// case
#define GB_VM_CaseBegin()			{GB_Word tg_;GB_PopIn(tg_);switch(tg_){
#define GB_VM_CaseEnd()				}}
#define GB_VM_CaseArm(i)			case i:
%%]

Allocation, storage, heap

%%[8
// allocate node, fill with values from the stack, leave ptr to node on TOS
extern void gb_VM_AllocStoreTos(Word nBytes, Word gcInfo);
#define GB_VM_AllocStoreTos(i)		{GB_Word s_;GB_PopIn(s_);gb_VM_AllocStoreTos(s_,i);}

// fetch content of node, leaving it on the stack
extern void gb_VM_FetchTos(GB_Word n);
#define GB_VM_FetchTos()			gb_VM_FetchTos(GB_TOS)

extern void gb_VM_FetchUpdateTos(Word nOld, Word nNew) ;
#define GB_VM_FetchUpdateTos()		{GB_Word o_, n_; GB_PopIn(n_);GB_PopIn(o_);gb_VM_FetchUpdateTos(o_,n_);}
%%]

Evaluation, apply, ...

%%[8
// eval TOS to WHNF
#define GB_VM_EvalTos()				gb_evalTos()

// apply TOS to args further on the stack
#define GB_VM_ApplyTos()			gb_applyTos()
%%]

Conversion

%%[8
// conversion between GB and non-GB representations of int and word
#define GB_VM_TagInt2WordTos()		GB_SetTOS( GB_Int2GBInt( GB_TOS ) )
#define GB_VM_UntagWord2IntTos()	GB_SetTOS( GB_GBInt2Int( GB_TOS ) )
#define GB_VM_TagWord2WordTos()		GB_SetTOS( GB_TagWord2Word( GB_TOS ) )
#define GB_VM_UntagWord2WordTos()	GB_SetTOS( GB_UntagWord2Word( GB_TOS ) )
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
	, GCStackInfo* gcStackInfos
	, GB_LinkChainResolvedInfo* linkChainInds
	, CallInfo* callinfos, int callinfosSz
	, FunctionInfo* functionInfos, int functionInfosSz
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
