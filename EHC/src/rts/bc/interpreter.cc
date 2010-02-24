%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
#include "interpreter.h"
// #include "ccall.h"
%%]

%%[8
#if USE_REGS_FOR_BP
#else
GB_Ptr      bp ;
#endif

#if USE_REGS_FOR_SP
#else
GB_Ptr      sp ;
#endif

#if USE_REGS_FOR_PC
#else
GB_BytePtr  pc ;
#endif

#if defined(RR_REG) && USE_REGS_FOR_SP && USE_REGS_FOR_PC && USE_REGS_FOR_BP
#else
GB_Word     rr ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% C Function types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef GB_Word GB_CFun();
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic types for C function types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Push2(v)				{*(sp-1) = (Cast(GB_Word,v)) ; sp-- ;}		/* avoid predecrement */
#define GB_SetReg(r,x)			{r = Cast(GB_Word,x);}
#define GB_SetTOSRel(o,x)		{ *GB_SPRel(o) = (x); }
#define GB_SetTOSByteRel(o,x)	{ *Cast(GB_Ptr,GB_SPByteRel(GB_Word,o)) = (x); }
#define GB_TOSCastedIn(ty,v)	{(v) = Cast(ty,GB_TOS) ;}

#define GB_PopnUpdTOS(n,x)		{sp += (n) ; GB_SetTOS(x) ; }

#define GB_SetSPRel(o,v)		GB_SetRegRel(sp,o,v)
#define GB_SetSPByteRel(o,v)	GB_SetRegRel(sp,o,v)

#define GB_SPByteRel(ty,o)		GB_RegByteRel(ty,sp,o)
#define GB_SPByteRelx(o)		GB_Deref(GB_SPByteRel(GB_Word,o))

#define GB_PushFromTo(plo,phi)	/* push memory area on stack */ 				\
								MemCopyBackward(phi,plo,sp) ;

#define GB_PushNodeArgs(nd,hdr,phi,plo)	/* push args of `fun + args' node fields */ 				\
								phi = Cast(GB_Ptr,&((nd)->content.fields[GB_NH_NrFlds(hdr)])) ;	\
								plo = Cast(GB_Ptr,&((nd)->content.fields[1])) ;								\
								IF_GB_TR_ON(3,printf("GB_PushNodeArgs:MemCopyBackward plo %p phi %p sp %p\n",plo,phi,sp);) ;			\
								GB_PushFromTo(plo,phi) ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exceptions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The current thrown exception.
This value marks the state of 'handling and exception' throughout primitive functions.
Handling the interpreterpart is done by unwinding the interpreter stack, but C functions need to do their unwinding explicitly.
Furthermore we need to count the number of switches (via eval wrappers) between interpreter and C functions when unwinding,
in order to know in which interpreterloop to continue.

%%[96
GB_NodePtr gb_ThrownException = NULL ;
int gb_ThrownException_NrOfEvalWrappers ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Immediate inlined constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_PCImmIn(ty,x)		{ x = *(Cast(ty*,pc)); pc += sizeof(ty); }
#define GB_PCExtIn(x)			GB_PCImmIn(GB_Byte,x)

#define GB_PCImmIn2(sz,x)		{ switch (sz) { \
								    case GB_InsOp_ImmSz_08 : \
								      GB_PCImmIn(int8_t,x) ; break ; \
								    case GB_InsOp_ImmSz_16 : \
								      GB_PCImmIn(int16_t,x) ; break ; \
								    case GB_InsOp_ImmSz_32 : \
								      GB_PCImmIn(int32_t,x) ; break ; \
								    case GB_InsOp_ImmSz_64 : \
								      GB_PCImmIn(int64_t,x) ; break ; \
								  } \
								}
%%]
#define GB_PCImm(ty)			(*(((ty*)pc)++))
#define GB_PCExt				GB_PCImm(GB_Byte)

%%[8
#define GB_Skip_CallInfoPtr		pc += sizeof(GB_CallInfo_Inline) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Allocation + tracing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if TRACE || DUMP_INTERNALS
GB_Ptr gb_allocated_lowest_ptr = NULL-1 , gb_allocated_highest_ptr = NULL ;

GB_Ptr gb_HeapAlloc_Bytes_Traced( GB_Word nBytes )
{
	GB_Ptr p =
#		if USE_BOEHM_GC
			Cast(GB_Ptr,GC_MALLOC(nBytes)) ;
#		elif USE_EHC_MM
			Cast(GB_Ptr,mm_itf_alloc(nBytes,0)) ;
#		endif
	if ( p < gb_allocated_lowest_ptr )
		gb_allocated_lowest_ptr = p ;
	if ( p > gb_allocated_highest_ptr )
		gb_allocated_highest_ptr = p ;
	return p ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal chunks of bytecode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static GB_CallInfo gb_callinfo_EvalWrap 		= GB_MkCallInfo(GB_CallInfo_Kind_EvalWrap		, "evalwrap"		) ;
static GB_CallInfo gb_callinfo_EvalTopWrap 		= GB_MkCallInfo(GB_CallInfo_Kind_EvalTopWrap	, "evaltopwrap"		) ;
static GB_CallInfo gb_callinfo_EvCont   		= GB_MkCallInfo(GB_CallInfo_Kind_EvCont  		, "evalcont"		) ;
static GB_CallInfo gb_callinfo_TailEvalCont   	= GB_MkCallInfo(GB_CallInfo_Kind_TailEvalCont  	, "tailevalcont"	) ;
static GB_CallInfo gb_callinfo_EvAppFunCont   	= GB_MkCallInfo(GB_CallInfo_Kind_EvAppFunCont  	, "evalappfuncont"	) ;
static GB_CallInfo gb_callinfo_EvAppFunEvCont   = GB_MkCallInfo(GB_CallInfo_Kind_EvAppFunEvCont , "evalappfunevcont") ;
static GB_CallInfo gb_callinfo_PEvCont  		= GB_MkCallInfo(GB_CallInfo_Kind_PApCont 		, "pappcont"		) ;
%%]

%%[8
static GB_Byte gb_code_AfterEvalCall[] =
  { 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_EvalUpdCont
  } ;

static GB_Byte gb_code_AfterTailEvalCall[] =
  { 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_TailEvalCont
  } ;

static GB_Byte gb_code_AfterEvalApplyFunCall[] =
  { 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_EvalApplyCont
  , 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_EvalUpdCont
  } ;

static GB_Byte gb_code_AfterCallInApplyWithTooManyArgs[] =
  { 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_PApplyCont
  } ;

GB_Byte gb_code_Eval[] =
  { GB_Ins_Eval(GB_InsOp_LocB_TOS)
  , 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_Ext, GB_InsExt_Halt
  } ;

#define GB_InitPatch_gb_code_Eval				*Cast(GB_Ptr,&gb_code_Eval[1]         ) = Cast(GB_Word,&gb_callinfo_EvalTopWrap)
#define GB_InitPatch_gb_code_AfterEvalCall		*Cast(GB_Ptr,&gb_code_AfterEvalCall[0]) = Cast(GB_Word,&gb_callinfo_EvCont  )
#define GB_InitPatch_gb_code_AfterTailEvalCall	*Cast(GB_Ptr,&gb_code_AfterTailEvalCall[0]) = Cast(GB_Word,&gb_callinfo_TailEvalCont  )
#define GB_InitPatch_gb_code_AfterEvalApplyFunCall 				\
									{ \
												*Cast(GB_Ptr,&gb_code_AfterEvalApplyFunCall[0]								) = Cast(GB_Word,&gb_callinfo_EvAppFunCont  ) ; \
												*Cast(GB_Ptr,&gb_code_AfterEvalApplyFunCall[1+sizeof(GB_CallInfo_Inline)]	) = Cast(GB_Word,&gb_callinfo_EvAppFunEvCont  ) ; \
									}
#define GB_InitPatch_gb_code_AfterCallInApplyWithTooManyArgs	\
												*Cast(GB_Ptr,&gb_code_AfterCallInApplyWithTooManyArgs[0]) = Cast(GB_Word,&gb_callinfo_PEvCont  )

%%]

%%[96
static GB_CallInfo gb_callinfo_ExcHdl_EvalValue  					= GB_MkCallInfo(GB_CallInfo_Kind_Call 		, "exception handler value eval"	) ;
static GB_CallInfo gb_callinfo_ExcHdl_NormalReturn_MarkedAsHandler  = GB_MkCallInfo(GB_CallInfo_Kind_Hdlr 		, "exception handler"				) ;
static GB_CallInfo gb_callinfo_ExcHdl_ThrowReturn  					= GB_MkCallInfo(GB_CallInfo_Kind_Call 		, "exception handler throw eval"	) ;
static GB_CallInfo gb_callinfo_Apply 								= GB_MkCallInfo(GB_CallInfo_Kind_Apply		, "apply"							) ;
static GB_CallInfo gb_callinfo_IntlCCall							= GB_MkCallInfo(GB_CallInfo_Kind_IntlCCall	, "internal C call"							) ;
%%]

%%[96
static GB_Byte gb_code_ExcHdl_EvalValue[] =
  { 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  //, GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_08), 2*sizeof(GB_Word)
  , GB_Ins_Eval(GB_InsOp_LocB_TOS)
  , 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_RetCall, GB_InsOp_ImmSz_08<<2 | GB_InsOp_ImmSz_08
  , sizeof(GB_Word)		// nArgMine
  , 2*sizeof(GB_Word)	// nArgSurr
  } ;

static GB_Byte gb_code_ExcHdl_NormalReturn_MarkedAsHandler[] =
  { 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_RetCall, GB_InsOp_ImmSz_08<<2 | GB_InsOp_ImmSz_08
  , sizeof(GB_Word)		// nArgMine
  , 3*sizeof(GB_Word)	// nArgSurr
  } ;

static GB_Byte gb_code_ExcHdl_ThrowReturn[] =
  { 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_Ext, GB_InsExt_ResetThrownException
  , GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_08), 1
  , GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_08), (4+GB_CallRetNrWords)*sizeof(GB_Word)	// after: 1, exc, bp, ret, 2, expr
  , GB_Ins_Eval(GB_InsOp_LocB_TOS)
  , 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_Apply(GB_InsOp_LocB_TOS)
  , 0, 0, 0, 0
#if USE_64_BITS
  , 0, 0, 0, 0
#endif
  , GB_Ins_RetCall, GB_InsOp_ImmSz_08<<2 | GB_InsOp_ImmSz_08
  , sizeof(GB_Word)		// nArgMine
  , 3*sizeof(GB_Word)	// nArgSurr
  } ;

#define GB_InitPatch_gb_code_ExcHdl_EvalValue \
									{ *Cast(GB_Ptr,&gb_code_ExcHdl_EvalValue[0]         						) = Cast(GB_Word,&gb_callinfo_ExcHdl_EvalValue) ; \
									  *Cast(GB_Ptr,&gb_code_ExcHdl_EvalValue[1+sizeof(GB_CallInfo_Inline)]  	) = Cast(GB_Word,&gb_callinfo_EvalWrap) ; \
									}
#define GB_InitPatch_gb_code_ExcHdl_NormalReturn_MarkedAsHandler \
									*Cast(GB_Ptr,&gb_code_ExcHdl_NormalReturn_MarkedAsHandler[0]         		) = Cast(GB_Word,&gb_callinfo_ExcHdl_NormalReturn_MarkedAsHandler)
#define GB_InitPatch_gb_code_ExcHdl_ThrowReturn \
									{ *Cast(GB_Ptr,&gb_code_ExcHdl_ThrowReturn[0]         						) = Cast(GB_Word,&gb_callinfo_ExcHdl_ThrowReturn) ; \
									  *Cast(GB_Ptr,&gb_code_ExcHdl_ThrowReturn[7+sizeof(GB_CallInfo_Inline)]    ) = Cast(GB_Word,&gb_callinfo_EvalWrap) ; \
									  *Cast(GB_Ptr,&gb_code_ExcHdl_ThrowReturn[8+2*sizeof(GB_CallInfo_Inline)]  ) = Cast(GB_Word,&gb_callinfo_Apply) ; \
									}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Finalization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Todo: USE_EHC_MM finalization

%%[94
#if USE_BOEHM_GC || USE_EHC_MM
#if USE_EHC_MM
void gb_Node_Finalize( Word p )
#else
void gb_Node_Finalize( void* p, void* cd )
#endif
{
	GB_NodePtr n = Cast(GB_NodePtr,p) ;
	GB_NodeHeader h = n->header ;
	if ( GB_NH_Fld_NdEv(h) == GB_NodeNdEv_No && GB_NH_Fld_TagCat(h) == GB_NodeTagCat_Intl )
	{
		switch( GB_NH_Fld_Tag(h) )
		{
%%[[95
			case GB_NodeTag_Intl_Malloc :
				gb_free( n->content.ptr ) ;
				break ;
			case GB_NodeTag_Intl_Malloc2 :
				gb_free( n->content.bytearray.ptr ) ;
				break ;
%%]]
%%[[97
#			if USE_EHC_MM && USE_GMP
				case GB_NodeTag_Intl_GMP_mpz :
					gb_Node_FinalizeInteger( n ) ;
					break ;
#			endif
%%]]
%%[[98
			case GB_NodeTag_Intl_Chan :
				fclose( n->content.chan.file ) ;
				break ;
%%]]
		}
	}
}

#if USE_BOEHM_GC
void* gb_Dummy_Finalization_Proc ;
void* gb_Dummy_Finalization_cd ;
#endif
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node, List specific interpretation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Force evaluation of a list upto *psz list nodes, without evaluating the elements.
On return:
  *psz holds actual nr of evaluated nodes
  *pn  holds the first list node.
  returns the first list node after the last evaluated list node.

Of the 2 following variants gb_listForceEval gives problems, I guess
this is because of the interaction with by-reference access to a node,
which also is GC'ed. I am unsure, but the 2nd variant eliminates some
crashes.

%%[8
GB_NodePtr gb_listForceEval( GB_NodePtr* pn, int* psz )
{
	int sz = 0 ;
	while ( (sz < *psz || *psz == 0) )
	{
		GB_GCSafe_Enter ;
		GB_GCSafe_1(*pn) ;
%%[[8
		*pn = Cast(GB_NodePtr,gb_eval( Cast(GB_Word,*pn) )) ;
%%][96
		GB_PassExc_GCSafe( *pn = Cast(GB_NodePtr,gb_eval( Cast(GB_Word,*pn) )) ) ;
%%]]
		if ( GB_List_IsNull( *pn ) )
			break ;
  		IF_GB_TR_ON(3,printf("gb_listForceEval1 *psz %d, sz %d, n %p: ", *psz, sz, *pn ););
  		IF_GB_TR_ON(3,gb_prWord(Cast(GB_Word,*pn)););
  		IF_GB_TR_ON(3,printf("\n" ););
		GB_GCSafe_Leave ;
		pn = GB_List_TailPtr(*pn) ;
  		IF_GB_TR_ON(3,printf("gb_listForceEval2 n %p\n", *pn ););
  		sz++ ;
	}
	*psz = sz ;
	return *pn ;
}
%%]

%%[8
GB_NodePtr gb_listForceEval2( GB_NodePtr n, int* psz )
{
	int sz = 0 ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(n) ;
	while ( (sz < *psz || *psz == 0) )
	{
%%[[8
		n = Cast(GB_NodePtr,gb_eval( Cast(GB_Word,n) )) ;
%%][96
		GB_PassExc_GCSafe( n = Cast(GB_NodePtr,gb_eval( Cast(GB_Word,n) )) ) ;
%%]]
		if ( GB_List_IsNull( n ) )
			break ;
  		IF_GB_TR_ON(3,printf("gb_listForceEval1 *psz %d, sz %d, n %p: ", *psz, sz, n ););
  		IF_GB_TR_ON(3,gb_prWord(Cast(GB_Word,n)););
  		IF_GB_TR_ON(3,printf("\n" ););
		n = GB_List_Tail(n) ;
  		IF_GB_TR_ON(3,printf("gb_listForceEval2 n %p\n", n ););
  		sz++ ;
	}
	GB_GCSafe_Leave ;
	*psz = sz ;
	return n ;
}
%%]

%%[98
/*
  In the following function gb_eval(GB_List_Head(n)) must be put into a local var,
  inlining produces a faulty program.
  Reason unknown :-(.
*/

GB_NodePtr gb_copyCStringFromEvalString( char* cString, GB_NodePtr hsString, int sz )
{
	GB_GCSafe_Enter ;
	GB_GCSafe_1(hsString) ;
	int bufInx = 0 ;
	GB_Word xx ;
	gb_assert_IsNotIndirection(Cast(Word,hsString),"gb_copyCStringFromEvalString") ;
	GB_List_Iterate(hsString,Cast(GB_NodePtr,gb_Indirection_FollowObject(Cast(Word,hsString))),sz,{GB_PassExc_GCSafe(xx = gb_eval(GB_List_Head(hsString))); cString[bufInx++] = GB_GBInt2Int(xx);}) ;
	GB_GCSafe_Leave ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO Channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98
void gb_chan_initstd()
{}

%%]
GB_NodePtr gb_chan_stdin ;
GB_NodePtr gb_chan_stdout ;
GB_NodePtr gb_chan_stderr ;

void gb_chan_initstd()
{
	GB_NodeAlloc_Chan_In_Rooted(gb_chan_stdin) ;
	gb_chan_stdin->content.chan.file  = stdin ;

	GB_NodeAlloc_Chan_In_Rooted(gb_chan_stdout) ;
	gb_chan_stdout->content.chan.file = stdout ;

	GB_NodeAlloc_Chan_In_Rooted(gb_chan_stderr) ;
	gb_chan_stderr->content.chan.file = stderr ;
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Global info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static GB_ModEntry* 	gb_AllMod ;
static GB_Word 			gb_AllModSize ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% C interface to internals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void gb_push( GB_Word x )
{
	GB_Push( x ) ;
}

GB_Word gb_eval( GB_Word x )
{
	GB_Push( x ) ;
	gb_assert_IsNotDangling( x, "gb_eval" ) ;
%%[[8
	gb_interpretLoopWith( gb_code_Eval ) ;
%%][96
	GB_PassExcWith \
		( {gb_ThrownException_NrOfEvalWrappers = 0 ; gb_interpretLoopWith( gb_code_Eval );} \
		, \
		, True \
		, {gb_ThrownException_NrOfEvalWrappers-- ; return Cast(GB_Word,gb_ThrownException);} \
		) ;
%%]]
	GB_PopIn( x ) ;
	gb_assert_IsNotDangling( x, "gb_eval" ) ;
	gb_assert_IsEvaluated( x, "gb_eval" ) ;
	return x ;
}

GB_Word gb_getTOS()
{
	return GB_TOS ;	
}

void gb_setTOS(GB_Word x)
{
	GB_SetTOS(x);	
}
%%]

%%[96
void gb_unlinkSP()
{
	GB_BP_UnlinkSP ;
}

void gb_setPC( GB_BytePtr c )
{
	pc = c ;
	GB_BP_UnlinkSP ;
}

%%]

%%[99
void gb_runIO( GB_Word io )
{
	GB_NodePtr runIO ;
	
	GB_GCSafe_Enter ;
	GB_GCSafe_1( io ) ;
	GB_GCSafe_1_Zeroed( runIO ) ;
	
	GB_MkAppNode1In( runIO, io, gb_Unit ) ;

	GB_GCSafe_Leave ;
	gb_eval( (Word)runIO ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Tracing on?

%%[8
int gb_Opt_TraceSteps = True ;
%%]

Info flags

%%[8
int gb_Opt_Info = 0 ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if TRACE || DUMP_INTERNALS
void gb_prByteCodeInstrEntry( GB_ByteCodeInstrEntry* e )
{
	int i ;
	printf( "%p: ", e->bcLoc ) ;
	for ( i = 0 ; i < e->bcSize ; i++ )
	{
		printf("%0.2x ", e->bcLoc[i]) ;
	}
	printf( " : %s\n", e->bc ) ;
}

void gb_prByteCodeModule( GB_ByteCodeModule* m )
{
	printf("*** module %s\n",m->bcModNm);
	int e, i;
	for ( e = 0 ; e < m->bcEntrySize ; e++ )
	{
		GB_ByteCodeEntryPoint* ep = &m->bcEntry[e] ;
		printf("  *** entry %s\n", ( ep->nm ? (char*)ep->nm : "--" ) );
		if ( ep->bcInstr != NULL )
		{
			for ( i = 0 ; i < ep->bcInstrSize ; i++ )
			{
				printf("    ");
				gb_prByteCodeInstrEntry(&ep->bcInstr[i]) ;
			}
		}
	}
}

#endif
%%]

%%[8
#if TRACE || DUMP_INTERNALS
void gb_prModEntries( GB_ModEntry* modTbl )
{
	int i ;
	for ( i = 0 ; modTbl[i].bcModule != NULL ; i++ )
	{
		gb_prByteCodeModule(modTbl[i].bcModule) ;
	}
}
#endif
%%]

%%[8
void gb_prCallInfoName( GB_CallInfo* ci )
{
	if ( ci->name )
	{
		printf( "%s", ci->name ) ;
	}
}

void gb_prCallInfo( GB_CallInfo* ci )
{
	printf( "(ci=%p", ci ) ;
	if ( ci != NULL ) {
		printf( ", kind=%d) ", ci->kind ) ;
		switch ( ci->kind ) {
			case GB_CallInfo_Kind_Eval :
				printf( "eval " ) ;
				gb_prCallInfoName( ci ) ;
				break ;
			case GB_CallInfo_Kind_Apply :
				printf( "apply " ) ;
				gb_prCallInfoName( ci ) ;
				break ;
			default :
				gb_prCallInfoName( ci ) ;
				break ;
		}
	} else {
		printf( ")" ) ;
	}
}

Bool gb_CallInfo_Kind_IsVisible( Word kind )
{
	switch( kind )
	{
		case GB_CallInfo_Kind_Call :
		case GB_CallInfo_Kind_Eval :
		case GB_CallInfo_Kind_Apply :
		case GB_CallInfo_Kind_CCall :
/*
*/
#	if TRACE
		case GB_CallInfo_Kind_Tail :
		case GB_CallInfo_Kind_EvalWrap :
		case GB_CallInfo_Kind_Hdlr :
		case GB_CallInfo_Kind_EvCont :
		case GB_CallInfo_Kind_TailEvalCont :
		case GB_CallInfo_Kind_TailEv :
		case GB_CallInfo_Kind_ApCont :
		case GB_CallInfo_Kind_PApCont :
		case GB_CallInfo_Kind_TailEval :
#	endif
			return True ;
			break ;
		default :
			return False ;
			break ;
	}
	return False ;
}

Bool gb_prCallInfoIsVisible( GB_CallInfo* ci )
{
	if ( ci != NULL )
	{
		return gb_CallInfo_Kind_IsVisible( ci->kind ) ;
	}
	return False ;
}
%%]

%%[8
#if GB_COUNT_STEPS
unsigned long gb_StepCounter ;
#endif

void gb_prWordAsNode( GB_NodePtr n )
{
	printf( "sz %d, ev %d, cat %d, tg %d:"
		  , GB_NH_Fld_Size(n->header), GB_NH_Fld_NdEv(n->header), GB_NH_Fld_TagCat(n->header), GB_NH_Fld_Tag(n->header) ) ;
	int i ;
	for ( i = 0 ; i < 5 && i < GB_Node_NrFlds(n) ; i++ )
	{
#			if USE_64_BITS
			printf( " 0x%0.16lx"
#			else
			printf( " 0x%0.8x"
#			endif
				  , n->content.fields[i] ) ;
	}
}

void gb_prWord( GB_Word x )
{
	GB_Node* n = Cast(GB_Node*,x) ;
#	if USE_64_BITS
		printf( "Wd 0x%0.16lx: "
#	else
		printf( "Wd 0x%0.8x: "
#	endif
	      , x ) ;
	if ( GB_GC_Managed( x ) ) {
		gb_prWordAsNode( Cast(GB_NodePtr,x) ) ;
	} else {
#		if USE_64_BITS
			printf( "gb int %ld"
#		else
			printf( "gb int %d"
#		endif
				  , GB_GBInt2Int(x) ) ;
	}
	/* printf( "\n" ) ; */
}

#if TRACE || DUMP_INTERNALS
extern char* gb_lookupMnem( GB_Byte c ) ;

void gb_prTOSAsInt( )
{
#		if USE_64_BITS
			printf( "%ld"
#		else
			printf( "%d"
#		endif
				  , GB_GBInt2Int(GB_TOS) ) ;
}

void gb_prStack( int maxStkSz )
{
    int i ;
    
	for ( i = 0 ; i < maxStkSz && sp+i < Cast(GB_Ptr,StackAreaHigh) ; i++ )
	{
#		if USE_64_BITS
			printf( "  %lx: "
#		else
			printf( "  %x: "
#		endif
				  , sp+i) ;
		gb_prWord( sp[i] ) ;
		printf( "\n" ) ;
	}
}

void gb_prState( char* msg, int maxStkSz )
{
	int i ;
	// printf( "--------------------------------- %s ---------------------------------\n", msg ) ;
	printf( "---------------------\n", msg ) ;
#	if USE_64_BITS
		printf( "[%ld]PC 0x%lx: 0x%0.2x '%s'"
#	else
		printf( "[%ld]PC 0x%x: 0x%0.2x '%s'"
#	endif
#	if GB_COUNT_STEPS
	      , gb_StepCounter
#	else
		  , 0
#	endif
	      , pc, *pc, gb_lookupMnem(*pc)) ;
	for ( i = 0 ; i < 8 ; i++ )
		printf(" %0.2x", pc[1+i]) ;
#	if USE_64_BITS
		printf( ", SP 0x%lx: 0x%0.16lx, BP 0x%lx, RR 0x%lx"
#	else
		printf( ", SP 0x%x: 0x%0.8x, BP 0x%x, RR 0x%x"
#	endif
	      , sp, *sp, bp, rr ) ;
%%[[96
	if ( bp != NULL )
	{
		printf( ", CI.kind %d", GB_FromBPToCallInfo(bp)->kind ) ;
	}
%%]]
	printf( "\n" ) ;
	GB_ByteCodeModule* bcm ;
	GB_ByteCodeEntryPoint* bce ;
	GB_ByteCodeInstrEntry* bci ;
	if ( gb_lookupInfoForPC( pc, &bcm, &bce, &bci ) )
	{
		// printf("bcm=%x bce=%x bci=%x\n",bcm, bce, bci) ;
		printf
%%[[8
			( "%s.%s + %p/%p: %s\n"
			, bcm->bcModNm
%%][20
			( "%s + %p/%p: %s\n"
%%]]
			, ( bce->nm ? (char*)bce->nm : "--" ) 
			, bci - bce->bcInstr, pc - bce->bcInstr->bcLoc
			, bci->bc
			) ;
	}
	gb_prStack( maxStkSz ) ;
}

#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shared #defines for interpreter loop and primitives closely related (exception handling)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_RetTailCall_Code(getDst,jumpDst,getPrevCallAdmin,prepCallAdmin,restorePrevCallAdmin)		/* share between tailcall & retcall */							\
				spSave = sp ;																												\
				GB_PCExtIn(x) ;																												\
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,2,3),x3) ; 			/* nArgMine  , in bytes				*/							\
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x4) ; 			/* nArgSurr  , in bytes				*/							\
				IF_GB_TR_ON(3,printf( "nArgMine %d nArgSurr %d\n", x3, x4 );) \
				getPrevCallAdmin ; 											/* ret address of current function 	*/							\
				getDst ; 													/* destination of call 				*/							\
				IF_GB_TR_ON(3,printf( "retSave %x dst %x\n", retSave, dst );) \
				p2 = GB_RegByteRel(GB_Word,bp,x4+GB_CallRetNrBytes) ;		/* after last old arg is dst startpoint of backwards copy */	\
				p  = GB_SPByteRel(GB_Word,x3) ;								/* after last new arg is src startpoint of backwards copy */	\
				MemCopyBackward(p,sp,p2) ;									/* copy new args over old 			*/							\
				prepCallAdmin ;												/* sp points to call admin (if any),*/							\
				restorePrevCallAdmin ;										/* which is assigned here			*/							\
				jumpDst ;													/* jump to dst						*/
%%]

Calling C:
The interpreter has two entry points to C code:
- plain/directly
- via closure, only constructed from within C itself; we assume no unboxed values (like float/double) are passed around.
Both have a different stack setup, which is not ideal for understanding, so may well change.
The tricky point is exception handling which makes assumptions about the stack setup.
The code is split up in preamble + call + postamble, where call is only necessary for the closure based call, plus postamble when an exception did occur.

%%[8
#define GB_CallC_Code(f,nargs,args,res) \
				IF_GB_TR_ON(3,printf("GB_CallC_Code1 f %x nargs %d\n", f, nargs );); \
				switch ( nargs )																																								\
				{																																												\
					case 0 : res = Cast(GB_CFun*,f)(  ) ; break ;																																\
					case 1 : res = Cast(GB_CFun*,f)( GB_RegRelx(args,0) ) ; break ;																												\
					case 2 : res = Cast(GB_CFun*,f)( GB_RegRelx(args,0), GB_RegRelx(args,1) ) ; break ;																							\
					case 3 : res = Cast(GB_CFun*,f)( GB_RegRelx(args,0), GB_RegRelx(args,1), GB_RegRelx(args,2) ) ; break ;																		\
					case 4 : res = Cast(GB_CFun*,f)( GB_RegRelx(args,0), GB_RegRelx(args,1), GB_RegRelx(args,2), GB_RegRelx(args,3) ) ; break ;													\
					case 5 : res = Cast(GB_CFun*,f)( GB_RegRelx(args,0), GB_RegRelx(args,1), GB_RegRelx(args,2), GB_RegRelx(args,3), GB_RegRelx(args,4) ) ; break ;								\
					case 6 : res = Cast(GB_CFun*,f)( GB_RegRelx(args,0), GB_RegRelx(args,1), GB_RegRelx(args,2), GB_RegRelx(args,3), GB_RegRelx(args,4), GB_RegRelx(args,5) ) ; break ;			\
					default :																																									\
						gb_panic1_1( "no C call for nr of args", nargs ) ;																														\
						break ;																																									\
				} \
				IF_GB_TR_ON(3,printf("GB_CallC_Code2 f %x res %x\n", f, res ););

#define GB_CallC_CodeWithType(f,nargs,args,res,argTyStr) \
				IF_GB_TR_ON(3,printf("GB_CallC_CodeWithType1 f=%x nargs=%d ty=%s\n", f, nargs, tyStr );); \
				switch ( nargs ) {																																								\
					case 0 :																																									\
						res = Cast(GB_CFun_w0*,f)( ) ;																																			\
						break ;																																									\
					case 1 :																																									\
						switch () {																																								\
							case 'w' :																																									\
								res = Cast(GB_CFun_w1w*,f)( GB_RegByteRelCastx(GB_Word,args,0) ) ;																									\
								break ;																																									\
						}																																										\
						break ;																																									\
					case 2 :																																									\
						res = Cast(GB_CFun_w2ww*,f)( GB_RegByteRelCastx(GB_Word,args,0), GB_RegByteRelCastx(GB_Word,args,sizeof(GB_Word)) ) ;													\
						break ;																																									\
					default :																																									\
						gb_panic1_1( "no C call for nr of args", nargs ) ;																														\
						break ;																																									\
				} \
				IF_GB_TR_ON(3,printf("GB_CallC_CodeWithType12 res=%x\n", res ););

#define GB_CallC_Code_Preamble(nargs) \
				GB_SetTOS(nargs) ;											/* push nr of args			 						*/		\
				GB_Push(pc) ;												/* setup call admin to look the same as normal 		*/		\
				GB_BP_Link ;

#define GB_CallC_Code_Postamble(nargs,res) \
				GB_BP_UnlinkSP ;																										\
				GB_PopCastIn(GB_BytePtr,pc) ;																							\
				GB_PopIn(nargs) ;											/* get nr of args									*/		\
				IF_GB_TR_ON(3,printf("GB_CallC_Code_Postamble nargs %d res %x\n", nargs, res ););										\
				sp = GB_RegRel(sp,nargs) ;									/* pop args											*/		\
				GB_Push(res) ;
%%]

%%[8
#define GB_UpdWithIndirection_Code_Copy(nOld,xNew,nNew,h,tmpp1,tmpp2,tmpp3,setRes) 													\
				{																													\
					tmpp1 = Cast(GB_Ptr,&(nNew->content.fields[GB_Node_NrFlds(nNew)])) ;	/* overwrite content of old with new */	\
					tmpp2 = Cast(GB_Ptr,nOld) ;																						\
					tmpp3 = Cast(GB_Ptr,nNew) ;																						\
					MemCopyForward(tmpp3,tmpp1,tmpp2) ;																				\
					setRes ;											/* return new (avoids indirection)			*/				\
				}

#define GB_UpdWithIndirection_Code_Ind(nOld,xNew,h,setRes) 													\
				{																													\
					h = nOld->header ;											/* turn into indirection node				*/		\
					h = GB_MkHeader(GB_NH_Fld_Size(h),GB_NodeNdEv_Yes,GB_NodeTagCat_Ind,0) ;										\
					nOld->header = h ;																								\
					nOld->content.fields[0] = xNew ;								/* assumption !!: memory is available !! 	*/	\
					setRes ;																										\
				}

#define GB_UpdWithIndirection_Code1(nOld,xNew,nNew,h,tmpp1,tmpp2,tmpp3,setRes) 														\
				nNew = Cast(GB_NodePtr,xNew) ;																						\
				if (  GB_Word_IsPtr(xNew)																							\
				   && (GB_NH_Fld_Size(nNew->header) <= GB_NH_Fld_Size(nOld->header))												\
				   )																												\
					GB_UpdWithIndirection_Code_Copy(nOld,xNew,nNew,h,tmpp1,tmpp2,tmpp3,setRes) 										\
				else																												\
					GB_UpdWithIndirection_Code_Ind(nOld,xNew,h,setRes)

#define GB_UpdWithIndirection_Code2(nOld,xNew,nNew,h,tmpp1,tmpp2,tmpp3,setRes) 														\
				nNew = Cast(GB_NodePtr,xNew) ;																						\
				GB_UpdWithIndirection_Code_Ind(nOld,xNew,h,setRes)

#define GB_UpdWithIndirection_Code_Tmp(nOld,xNew)					 																\
				{ GB_Ptr tmpp1,tmpp2,tmpp3 ;																						\
				  GB_NodePtr nNew ;																									\
				  GB_NodeHeader h ;																									\
				  GB_UpdWithIndirection_Code1(nOld,xNew,nNew,h,tmpp1,tmpp2,tmpp3,;) ;												\
				}

#define GB_Op_TOSDst_ImmInt_Code(ty,x,opres)												\
						GB_PCImmIn(ty,x) ;						/* load immediate opnd 	*/	\
						x = GB_Int2GBInt( x ) ;					/* make int of it	 	*/	\
						GB_SetTOS( opres ) ;					/* update top of stack 	*/	

#define GB_Op_TOSDst_SPRelImm_Code(ty,x,opres)												\
						GB_PCImmIn(ty,x) ;						/* load immediate opnd 	*/	\
						GB_SetTOS( opres ) ;					/* update top of stack 	*/	

#define GB_Op_TOSDst_TOSSrc_Code(ty,x,opres)												\
						GB_PopIn( x ) ;							/* pop opnd 			*/	\
						GB_SetTOS( opres ) ;					/* update top of stack 	*/	

#define GB_Op_TOSDst_Case1_Code(deref,locSrc,how,x,op,src1,src2)				\
					case GB_Ins_OpExt(deref,locSrc,GB_InsOp_ImmSz_08) :			\
						how(int8_t,x,op( src1, src2 )) ;						\
						break ;													\
					case GB_Ins_OpExt(deref,locSrc,GB_InsOp_ImmSz_16) :			\
						how(int16_t,x,op( src1, src2 )) ;						\
						break ;													\
					case GB_Ins_OpExt(deref,locSrc,GB_InsOp_ImmSz_32) :			\
						how(int32_t,x,op( src1, src2 )) ;						\
						break ;													\
					case GB_Ins_OpExt(deref,locSrc,GB_InsOp_ImmSz_64) :			\
						how(int64_t,x,op( src1, src2 )) ;						\
						break ;																			

#define GB_Op_TOSDst_Case_Code(op)																				\
					GB_Op_TOSDst_Case1_Code( GB_InsOp_DerefInt,GB_InsOp_LocOSrc_Imm, GB_Op_TOSDst_ImmInt_Code 	\
					                       , x, op, GB_TOS, x 													\
					                       )																	\
					GB_Op_TOSDst_Case1_Code( GB_InsOp_Deref1,GB_InsOp_LocOSrc_SP, GB_Op_TOSDst_SPRelImm_Code 	\
					                       , x, op, GB_TOS, GB_SPByteRelx( x ) 									\
					                       )																	\
					GB_Op_TOSDst_Case1_Code( GB_InsOp_Deref1,GB_InsOp_LocOSrc_TOS, GB_Op_TOSDst_TOSSrc_Code	 	\
					                       , x, op, GB_TOS, x				 									\
					                       )

#define GB_Check_StackSize(pc)																					\
				{																								\
					GB_FunctionInfo* functionInfo = GB_FromPCToFunctionInfo(pc) ;								\
					if ( ! GB_Stack_SzIsAvailable( functionInfo->szStack ) ) {									\
%%[[9696
						gb_intl_throwStackOverflow( functionInfo ) ;											\
						GB_PassExcWith(,,gb_ThrownException_NrOfEvalWrappers > 0,goto interpretIsDone) ;		\
%%]]
						fprintf( stderr, "Stack sp=%p [%p(+spare=%x)..%p], requested=%x\n", sp, StackAreaLow, STACKSIZE_SPARE_UNUSED, StackAreaHigh, functionInfo->szStack ) ; \
						gb_panic2_1( "stackspace", (char*)functionInfo->nm, functionInfo->szStack ) ;			\
					}																							\
				}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interpreter loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void gb_interpretLoopWith( GB_BytePtr initPC )
{
	pc = initPC ;
	gb_interpretLoop() ;
}

void gb_interpretLoop()
{
	/* scratch registers */
  	register GB_Word x, x3 ;
  	register GB_Ptr  p, p2, p3, spSave ;
  	register GB_NodeHeader h ;

	/* scratch */
  	GB_NodePtr n = NULL ;
  	GB_Word x2 = 0, x4, x5, x6, retSave, bpSave ;
  	GB_Ptr  p4, p5, p6 ;
  	GB_BytePtr  dst ;
  	
  	// GB_GCSafe_Enter ;
  	// GB_GCSafe_2( x2, n ) ;

	while( True )
	{
		IF_GB_TR_ON(1,gb_prState( "interpreter step", 3 /*10*/ ) ;)
		switch( *(pc++) )
		{
			/* load immediate constant on stack */
			/* l0ti08 */
			case GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_08) :
				GB_PCImmIn(int8_t,x) ;
				GB_Push( x ) ;
				break ;

			/* l0ti16 */
			case GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_16) :
				GB_PCImmIn(int16_t,x) ;
				GB_Push( x ) ;
				break ;

			/* l0ti32 */
			case GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_32) :
				GB_PCImmIn(int32_t,x) ;
				GB_Push( x ) ;
				break ;

			/* l0ti64 */
			case GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_64) :
				GB_PCImmIn(int64_t,x) ;
				GB_Push( x ) ;
				break ;

			/* l1ti08 */
			/* l1ti16 */
			/* l1ti32 */
			/* l1ti64 */

			/* l2ti08 */
			/* l2ti16 */
			/* l2ti32 */
			/* l2ti64 */

			/* load immediate signed int constant as tagged int on stack */
			/* liti08 */
			case GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_08) :
				GB_PCImmIn(int8_t,x) ;
				GB_Push( GB_Int2GBInt( x ) ) ;
				break ;

			/* liti16 */
			case GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_16) :
				GB_PCImmIn(int16_t,x) ;
				GB_Push( GB_Int2GBInt( x ) ) ;
				break ;

			/* liti32 */
			case GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_32) :
				GB_PCImmIn(int32_t,x) ;
				GB_Push( GB_Int2GBInt( x ) ) ;
				break ;

			/* liti64 */
			case GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_64) :
				GB_PCImmIn(int64_t,x) ;
				GB_Push( GB_Int2GBInt( x ) ) ;
				break ;

			/* l0ts08 */
			/* l0ts16 */
			/* l0ts32 */
			/* l0ts64 */

			/* load TOS relative content on stack */
			/* l1ts08 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_08) :
				GB_PCImmIn(int8_t,x) ;
				GB_Push2( GB_SPByteRelx( x ) ) ;
				break ;

			/* l1ts16 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_16) :
				GB_PCImmIn(int16_t,x) ;
				GB_Push2( GB_SPByteRelx( x ) ) ;
				break ;

			/* l1ts32 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_32) :
				GB_PCImmIn(int32_t,x) ;
				GB_Push2( GB_SPByteRelx( x ) ) ;
				break ;

			/* l1ts64 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_64) :
				GB_PCImmIn(int64_t,x) ;
				GB_Push2( GB_SPByteRelx( x ) ) ;
				break ;

			/* l2ts08 */
			case GB_Ins_Ld(GB_InsOp_Deref2, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_08) :
				GB_PCImmIn(int8_t,x) ;
				GB_Push2( GB_RegByteRelx( GB_TOS, x ) ) ;
				break ;

			/* l2ts16 */
			case GB_Ins_Ld(GB_InsOp_Deref2, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_16) :
				GB_PCImmIn(int16_t,x) ;
				GB_Push2( GB_RegByteRelx( GB_TOS, x ) ) ;
				break ;

			/* l2ts32 */
			case GB_Ins_Ld(GB_InsOp_Deref2, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_32) :
				GB_PCImmIn(int32_t,x) ;
				GB_Push2( GB_RegByteRelx( GB_TOS, x ) ) ;
				break ;

			/* l2ts64 */
			case GB_Ins_Ld(GB_InsOp_Deref2, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_64) :
				GB_PCImmIn(int64_t,x) ;
				GB_Push2( GB_RegByteRelx( GB_TOS, x ) ) ;
				break ;

			
			/* l0tr08 */
			/* l0tr16 */
			/* l0tr32 */
			/* l0tr64 */

			/* load RR relative content on stack */
			/* l1tr08 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_08) :
				GB_PCImmIn(int8_t,x) ;
				gb_assert_IsNotDangling( (Word)GB_RegByteRel( GB_Word, rr, x ), "GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg ..." ) ;
				GB_Push( GB_RegByteRelx( rr, x ) ) ;
				break ;

			/* l1tr16 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_16) :
				GB_PCImmIn(int16_t,x) ;
				// printf( "rr=%x off=%x res=%x\n", rr, x, GB_RegByteRel(GB_Word,rr,x) ) ;
				gb_assert_IsNotDangling( (Word)GB_RegByteRel( GB_Word, rr, x ), "GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg ..." ) ;
				GB_Push( GB_RegByteRelx( rr, x ) ) ;
				break ;

			/* l1tr32 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_32) :
				GB_PCImmIn(int32_t,x) ;
				gb_assert_IsNotDangling( (Word)GB_RegByteRel( GB_Word, rr, x ), "GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg ..." ) ;
				GB_Push( GB_RegByteRelx( rr, x ) ) ;
				break ;

			/* l1tr64 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_64) :
				GB_PCImmIn(int64_t,x) ;
				gb_assert_IsNotDangling( (Word)GB_RegByteRel( GB_Word, rr, x ), "GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg ..." ) ;
				GB_Push( GB_RegByteRelx( rr, x ) ) ;
				break ;


			/* l2tr08 */
			/* l2tr16 */
			/* l2tr32 */
			/* l2tr64 */

			/* ldgt */
			case GB_Ins_Ldg(GB_InsOp_LocB_TOS) :
				GB_PCImmIn(GB_Word,x) ;
				x = *Cast(GB_Word*,x) ;
				GB_Push( x ) ; /* linked in value */
				break ;
			
			/* ldgr */
			case GB_Ins_Ldg(GB_InsOp_LocB_Reg) :
				GB_PCImmIn(GB_Word,rr) ;
				rr = *Cast(GB_Word*,rr) ;
				break ;
			
			/* l0rs08 */
			/* l0rs16 */
			/* l0rs32 */
			/* l0rs64 */

			/* load TOS relative content in reg RR */
			/* l1rs08 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_Reg, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_08) :
				GB_PCImmIn(int8_t,x) ;
				GB_SetReg( rr, GB_SPByteRelx( x ) ) ;
				break ;

			/* l1rs16 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_Reg, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_16) :
				GB_PCImmIn(int16_t,x) ;
				GB_SetReg( rr, GB_SPByteRelx( x ) ) ;
				break ;

			/* l1rs32 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_Reg, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_32) :
				GB_PCImmIn(int32_t,x) ;
				GB_SetReg( rr, GB_SPByteRelx( x ) ) ;
				break ;

			/* l1rs64 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_Reg, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_64) :
				GB_PCImmIn(int64_t,x) ;
				GB_SetReg( rr, GB_SPByteRelx( x ) ) ;
				break ;

			/* l2rs08 */
			/* l2rs16 */
			/* l2rs32 */
			/* l2rs64 */

			/* calling, returning, case */
			/* callt */
			case GB_Ins_Call(GB_InsOp_LocB_TOS) :
				GB_Skip_CallInfoPtr ;
				x = GB_TOS ;
gb_interpreter_InsCallEntry:
				GB_SetTOS( Cast(GB_Word,pc) ) ;								/* return address on stack			*/
				GB_BP_Link ;												/* link bp */
				pc = Cast(GB_BytePtr,x) ;
				GB_Check_StackSize(pc) ;
				break ;
			
			/* callr */

			/* retcall */
			case GB_Ins_RetCall :
				GB_RetTailCall_Code
					( dst = Cast(GB_BytePtr,GB_RegRelx(bp,1)) // dst = Cast(GB_BytePtr,GB_RegByteRelx(spSave,x5))
					, pc = dst
					, ;
					, sp = GB_RegByteRel(GB_Word,bp,x4-x3+GB_CallRetNrBytes) ; GB_BP_Unlink
					, ;
					) ;
				IF_GB_TR_ON(3,{printf("GB_Ins_RetCall: sp=%p, bp=%p, pc=%p, TOS=%x\n", sp, bp, pc, GB_TOS) ;}) ;
				break ;

			/* casecall */
			case GB_Ins_CaseCall :
				GB_PCImmIn(Word,x2) ;										// nr of alternatives, not used further, but required by linking for following table
				p = Cast(GB_Ptr,pc) ;										// start of table after instr						
				GB_PopIn(x) ;												// tag of scrutinee is on TOS
				dst = Cast(GB_BytePtr,p[GB_GBInt2Int(x)]) ;					// destination from following table, indexed by tag
				pc = dst ;													// jump 											
				break ;

			/* callc */
			case GB_Ins_CallC :
				GB_PCExtIn(x) ;
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x2) ; 			/* nr of args											*/
				// GB_Word callenc ;
				// GB_PCImmIn2(GB_InsOp_ImmSz_32,callenc) ; 					/* call encoding										*/
				GB_PCImmIn(GB_Word,x) ;										/* call encoding wrapper function						*/
				x = *Cast(GB_Word*,x) ;
				IF_GB_TR_ON(3,{printf("GB_Ins_CallC-enc: wrapper=%x TOS(func)=%x\n", x, GB_TOS) ;}) ;
				GB_CallInfoPtr pCI = *Cast(GB_CallInfoPtr*,pc) ;
				GB_Skip_CallInfoPtr ;
				IF_GB_TR_ON(3,{printf("GB_Ins_CallC-ty: pCI.ty=%s\n", pCI->ccall.type) ;}) ;
				// p = GB_SPRel(1) ;										/* args													*/
				// x = GB_TOS ;												/* function												*/
				// GB_CallC_Code_Preamble(x2) ;
				// GB_CallC_Code(x,x2,p,x) ;
				// GB_SetTOS( gb_Indirection_FollowObject(GB_TOS) ) ;
				gb_assert_IsNotIndirection( GB_TOS, "GB_Ins_CallC" ) ;
				// GC sensitive/unsafe, gcsafe'd:
				// gb_callc( x2, callenc ) ;
				((Fun_Void)x)() ;											/* call the wrapper */
%%[[96
				IF_GB_TR_ON(3,{printf("GB_Ins_CallC-A: gb_ThrownException = %p, gb_ThrownException_NrOfEvalWrappers = %d\n", gb_ThrownException, gb_ThrownException_NrOfEvalWrappers) ;}) ;
				GB_PassExcWith(,,gb_ThrownException_NrOfEvalWrappers > 0,goto interpretIsDone) ;
%%]]
				// GB_CallC_Code_Postamble(x2,x) ;
				break ;

			/* retcase */
			case GB_Ins_RetCase :
				GB_PCExtIn(x) ;
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,2,3),x2) ; 			/* nRes		 , in bytes 			*/
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x3) ; 			/* retOffSurr, in bytes 			*/
				GB_PCImmIn(GB_BytePtr,dst) ; 								/* destination 			*/
				p2 = GB_SPByteRel(GB_Word,x3) ;								/* after retOffSurr is dst startpoint of backwards copy */
				p  = GB_SPByteRel(GB_Word,x2) ;								/* after nRes is src startpoint of backwards copy */
				MemCopyBackward(p,sp,p2) ;									/* copy new args over old 			*/
				sp = GB_SPByteRel(GB_Word,x3-x2) ;							/* sp points to res		*/
				pc = dst ;													/* jump */
				break ;
						
			/* eval/apply */

			/* evalt */
			case GB_Ins_Eval(GB_InsOp_LocB_TOS) :
				GB_Skip_CallInfoPtr ;
				x = GB_TOS ;
gb_interpreter_InsEvalEntry:
				if ( GB_Word_IsPtr(x) ) {
					gb_assert_IsNotDangling( x, "GB_Ins_Eval" ) ;
					n = Cast(GB_NodePtr,x) ;
					h = n->header ;
					switch( GB_NH_Fld_NdEv(h) )
					{
						case GB_NodeNdEv_Yes :
							switch( GB_NH_Fld_TagCat(h) ) {
								case GB_NodeTagCat_Fun :
									GB_Push(pc) ;													/* save ret for after eval 			*/
									GB_BP_Link ;													/* and bp							*/
									// n->content.fields[0] = gb_Indirection_FollowObject( n->content.fields[0] ) ;
									gb_assert_IsNotIndirection( n->content.fields[0], "GB_Ins_Eval.GB_NodeTagCat_Fun" ) ;
									p = &(n->content.fields[1]) ;									/* 1st arg 							*/
									p2 = &(n->content.fields[GB_NH_NrFlds(h)]) ;					/* after last arg 					*/
									MemCopyBackward(p2,p,sp) ;										/* push args on stack 				*/
									pc = Cast(GB_BytePtr,n->content.fields[0]) ;					/* jump to function 				*/
									GB_Check_StackSize(pc) ;
									GB_Push(&gb_code_AfterEvalCall[sizeof(GB_CallInfo_Inline)]) ;	/* ret addr is to update 			*/
									GB_BP_Link ;													/* and bp							*/
									n->header = GB_NH_SetFld_NdEv(h,GB_NodeNdEv_BlH) ;				/* may not be eval'd when eval'd	*/
									break ;
								case GB_NodeTagCat_CFun :
									p = &(n->content.fields[1]) ;									/* 1st arg 							*/
									x2 = x ;                                                        /* remember val, for gb_interpreter_InsEvalUpdContEntry */
									retSave = Cast(GB_Word,pc) ;									/* remember pc, for gb_interpreter_InsEvalUpdContEntry */
									gb_assert_IsNotIndirection( n->content.fields[0], "GB_Ins_Eval.GB_NodeTagCat_CFun" ) ;
									n->header = GB_NH_SetFld_NdEv(h,GB_NodeNdEv_BlH) ;				/* may not be eval'd when eval'd	*/
									{
										// GC sensitive/unsafe, gcsafe'd: x2
										GB_GCSafe_Enter ;
										GB_GCSafe_1( x2 ) ;
										// no preamble, C call is done directly with args from node, without nr of args pushed
										GB_CallC_Code(n->content.fields[0],GB_NH_NrFlds(h)-1,p,x) ;
										GB_GCSafe_Leave ;
									}
%%[[8
									goto gb_interpreter_InsEvalUpdContEntry ;						/* update with result				*/
%%][96
									IF_GB_TR_ON(3,{printf("GB_Ins_Eval.GB_NodeTagCat_CFun: gb_ThrownException = %p, gb_ThrownException_NrOfEvalWrappers = %d\n", gb_ThrownException, gb_ThrownException_NrOfEvalWrappers) ;}) ;
									GB_PassExcWith(,,gb_ThrownException_NrOfEvalWrappers > 0,goto interpretIsDone) ;
									if ( gb_ThrownException ) {
										// postamble only because we now know we end up in a handler, which uses the default convention
										GB_CallC_Code_Postamble(x2,x) ;								/* now x2 as scratch 				*/
									} else {
										goto gb_interpreter_InsEvalUpdContEntry ;					/* update with result				*/
									}
%%]]
									break ;
								case GB_NodeTagCat_App :
									GB_Push(pc) ;													/* save ret for after eval 			*/
									GB_BP_Link ;													/* and bp							*/
									GB_Push(x = n->content.fields[0]) ;								/* push function to eval 			*/
									pc = &gb_code_AfterEvalApplyFunCall[sizeof(GB_CallInfo_Inline)] ;
									goto gb_interpreter_InsEvalEntry ;								/* evaluate							*/
									break ;
								case GB_NodeTagCat_Ind :
									x = gb_Indirection_FollowIndirection( x ) ;
									GB_SetTOS( x ) ;												/* just follow the indirection		*/
									goto gb_interpreter_InsEvalEntry ;								/* and re-evaluate					*/
									break ;
							}
							break ;
						case GB_NodeNdEv_BlH :
							gb_prWord( x ) ; printf("\n") ;
							gb_panic1_1( "black hole", x ) ;										/* black hole means cycle			*/
							break ;
					}
				}
				break ;

			/* evalr */
			
			/* evupdcont */
			case GB_Ins_EvalUpdCont :
				GB_PopIn(x) ;													/* evaluation result 						*/
				IF_GB_TR_ON(3,{printf("GB_Ins_EvalUpdCont sp=%p, bp=%p, TOS=%x, pc=%p\n", sp, bp, x, pc) ;}) ;
				gb_assert_IsNotIndirection(x,"GB_Ins_EvalUpdCont") ;
				GB_BP_UnlinkSP ;
				GB_PopIn(retSave) ;												/* saved return address 					*/
				x2 = GB_TOS ;													/* node which was to be evaluated 			*/
gb_interpreter_InsEvalUpdContEntry:
				IF_GB_TR_ON(3,printf("GB_Ins_EvalUpdCont toupd=%p, with=%x isInt %d isPtr %d\n",x2,x,GB_Word_IsInt(x),GB_Word_IsPtr(x));) ;
				gb_assert_IsNotDangling( x, "GB_Ins_EvalUpdCont" ) ;
				GB_NodePtr nOld = Cast(GB_NodePtr,x2) ;
				// GB_UpdWithIndirection_Code1(nOld,x,n,h,p,p2,p3,GB_SetTOS( x )) ;
				GB_UpdWithIndirection_Code_Ind(nOld,x,h,GB_SetTOS( x )) ;
				pc = Cast(GB_BytePtr,retSave) ;									/* jump to saved ret address				*/
				break ;

			/* evappcont */
			case GB_Ins_EvalApplyCont :
				GB_Skip_CallInfoPtr ;
				GB_PopIn(x) ;													/* evaluated function						*/
				gb_assert_IsNotDangling( x, "GB_Ins_EvalApplyCont" ) ;
				gb_assert_IsNotIndirection( x, "GB_Ins_EvalApplyCont" ) ;
				n = Cast(GB_NodePtr,GB_SPRelx(2)) ;								/* the apply node							*/
				h = n->header ;
				GB_PushNodeArgs(n,h,p,p2) ;										/* copy arguments from app					*/
				GB_Push(GB_NH_Fld_Size(h)-2) ;									/* nr of args								*/
				GB_Push(x) ;													/* function									*/
				goto gb_interpreter_InsApplyEntry ;								/* continue with apply						*/
				break ;

			/* papplycont */
			case GB_Ins_PApplyCont :
				GB_PopIn(x) ;													/* result value								*/
				gb_assert_IsNotDangling( x, "GB_Ins_PApplyCont" ) ;
				gb_assert_IsNotIndirection( x, "GB_Ins_PApplyCont" ) ;
				pc = Cast(GB_BytePtr,GB_TOS) ;									/* continuation								*/
				GB_SetTOS( x ) ;												/* result is slided down					*/
				goto gb_interpreter_InsApplyEntry ;								/* goto apply								*/

			// 20091009: under construction
			/* tailevalcont */
			case GB_Ins_TailEvalCont :
				x = GB_TOS ;															/* the evaluated value		*/
				x2 = GB_RegRelx(sp,1) ;													/* size of args to remove from surrounding call */
				pc = Cast(GB_BytePtr,GB_RegRelx(bp,1)) ;								/* continuation address				*/
				IF_GB_TR_ON(3,{printf("GB_Ins_TailEvalCont sp=%p, bp=%p, TOS=%x, szarg=%x, pc=%p\n", sp, bp, x, x2, pc) ;}) ;
				sp = GB_RegByteRel(GB_Word,bp,x2+GB_CallRetNrBytes-sizeof(GB_Word)) ;	/* sp points to eval arg			*/
				GB_SetTOS( x ) ;														/* which is set here				*/
				GB_BP_Unlink ;															/* back to in caller context (is return) 	*/
				break ;

			/* applyt */
			case GB_Ins_Apply(GB_InsOp_LocB_TOS) :
				GB_Skip_CallInfoPtr ;
gb_interpreter_InsApplyEntry:
				n = Cast(GB_NodePtr,GB_TOS) ;										/* function										*/
				gb_assert_IsNotDangling( x, "GB_Ins_Apply" ) ;
				gb_assert_IsNotIndirection((Word)n,"GB_Ins_Apply") ;
				x = GB_RegRelx(sp,1) ;													/* nArgs, in words 								*/
				h = n->header ;
				IF_GB_TR_ON(3,printf("GB_Ins_Apply n=%p h(n)=%x, nArgs=%d\n",n,h,x);) ;
				register int nLeftOver, nMiss ;
				switch ( GB_NH_Fld_TagCat(h) ) {
					case GB_NodeTagCat_PAp :
						nMiss = GB_NH_Fld_Tag(h) ;
						nLeftOver = Cast(int,x) - nMiss ;
						IF_GB_TR_ON(3,printf("nMiss %d nGiven %d nLeftOver %d\n",nMiss,x,nLeftOver);) ;
						if ( nLeftOver > 0 ) {											/* func is partial app, enough args on stack				*/
							p = sp ;													/* prepare continuation with next apply						*/
							p2 = GB_SPRel(2) ;											/* by shifting down nr fun args, with remaining 			*/
							p3 = GB_SPRel(2 + nMiss) ;									/* nr + args behind											*/
							MemCopyForward(p2,p3,p) ;
							GB_SPRelx(nMiss) = Cast(GB_Word,pc) ;						/* prepare for applycont, save pc, push nr remaining args	*/
							GB_SPRelx(nMiss+1) = nLeftOver ;
							GB_PushNodeArgs(n,h,p,p2) ;									/* copy arguments from partial app							*/
							pc = Cast(GB_BytePtr,n->content.fields[0]) ;						/* call function									*/
							GB_Push( Cast(GB_Word,&gb_code_AfterCallInApplyWithTooManyArgs[sizeof(GB_CallInfo_Inline)]) ) ;  /* with continuation set to another apply			*/
							GB_BP_Link ;
						} else if ( nLeftOver == 0 ) {
							sp = Cast(GB_Ptr,GB_SPRel(2)) ;								/* remove node+size from stack 								*/
							GB_PushNodeArgs(n,h,p,p2) ;									/* copy arguments from partial app							*/
							x = n->content.fields[0] ;									/* call function											*/
							GB_Push(x) ;
							goto gb_interpreter_InsCallEntry ;
						} else { /* ( nLeftOver < 0 ) */
							GB_NodeHeader hOld = h ;
							h = GB_MkHeader(GB_NH_Fld_Size(h)+x,GB_NH_Fld_NdEv(h),GB_NH_Fld_TagCat(h),GB_NH_Fld_Tag(h)-x) ;
							IF_GB_TR_ON(3,printf("alloc A sz=%x\n",GB_NH_Fld_Size(h));) ;
							{
								// GC sensitive/unsafe, gcsafe'd: n
								GB_GCSafe_Enter ;
								GB_GCSafe_1( n ) ;
								p = GB_HeapAlloc_Words( GB_NH_Fld_Size(h) ) ;			/* fresh node												*/
								GB_GCSafe_Leave ;
							}
							// IF_GB_TR_ON(3,printf("gb_interpreterLoop.GB_Ins_Apply alloc B sz=%x p=%x\n",GB_NH_Fld_Size(h),p);) ;
							x2 = Cast(GB_Word,p) ;										/* remember, to push later on								*/
							Cast(GB_NodePtr,p)->header = h ;							/* set header												*/
							p++ ;
							p2 = n->content.fields ;									/* copy old fields prep										*/
							p3 = Cast(GB_Ptr,&(p2[GB_NH_NrFlds(hOld)])) ;
							MemCopyForward(p2,p3,p) ;									/* copy old fields								*/
							GB_Popn(2) ;
							p3 = GB_SPRel(x) ;											/* copy new args								*/
							MemCopyForward(sp,p3,p) ;
							GB_Push(x2) ;
						}
						break ;

					default :
						gb_panic1_1( "non partial apply applied", Cast(GB_Word,n) ) ;
						break ;
				}
				break ;

			/* applyr */
			
			/* heap allocation (+storing), heap retrieving (fetching) */
			/* allocstoret */
			case GB_Ins_AllocStore(GB_InsOp_LocB_TOS) :
				GB_PCImmIn(Word,x2) ;
				GB_PopIn(x) ;
				// GC sensitive/unsafe, gcsafe'd:
				p = GB_HeapAlloc_Bytes_GCInfo(x,x2) ;
				p2 = p ;
				p3 = GB_SPByteRel(GB_Word,x) ;
				MemCopyForward(sp,p3,p) ;
				GB_Push(p2) ;
				// IF_GB_TR_ON(3,{printf( "alloc sz=%d gcinfo=%x->%x p=%p:", x, x2, Cast(GB_GCInfo*,x2)->nrOfTOS_No_GCTrace, p2 );gb_prWordAsNode(Cast(GB_NodePtr,p2));printf("\n");}) ;
				IF_GB_TR_ON(3,{printf( "alloc sz=%d gcinfo=%x->%x p=%p:", x, x2, Cast(GB_GCStackInfo*,x2)->sz, p2 );gb_prWordAsNode(Cast(GB_NodePtr,p2));printf("\n");}) ;
#				if USE_EHC_MM
					IF_GB_TR_ON(3,{printf( "alloc space=%p\n", mm_Spaces_GetSpaceForAddress( (Word)p2 ) );}) ;
#				endif
				gb_assert_IsNotDangling_Node( (GB_NodePtr)p2, "GB_Ins_AllocStore" ) ;
				break ;

			/* allocstorer */

			/* fetcht */
			case GB_Ins_Fetch(GB_InsOp_LocB_TOS) :
				GB_PopCastIn(GB_NodePtr,n) ;
				gb_assert_IsNotDangling( (Word)n, "GB_Ins_Fetch" ) ;
				gb_assert_IsNotIndirection( (Word)n, "GB_Ins_Fetch" ) ;
				p = Cast(GB_Ptr,&(n->content.fields[GB_Node_NrFlds(n)])) ;
				p2 = n->content.fields ;
				MemCopyBackward(p,p2,sp) ;
				break ;

			/* fetchr */

			/* fetchupd */
			case GB_Ins_FetchUpdate :
				GB_PopIn(x) ;
				GB_PopCastIn(GB_NodePtr,n) ;
				/*
				n->content.fields[0] = x ;
				h = n->header ;
				h = GB_MkHeader(GB_NH_Fld_Size(h),GB_NodeNdEv_Yes,GB_NodeTagCat_Ind,GB_NH_Fld_Tag(h)) ;
				n->header = h ;
				*/
				gb_assert_IsNotDangling( x, "GB_Ins_FetchUpdate1" ) ;
				x = gb_Indirection_FollowObject(x) ;
				gb_assert_IsNotDangling( x, "GB_Ins_FetchUpdate2" ) ;
				GB_UpdWithIndirection_Code_Ind(n,x,h,;) ;
				break ;

			/* lnt */
			case GB_Ins_LdNodeTag :
				GB_TOSCastedIn(GB_NodePtr,n) ;
				gb_assert_IsNotDangling( (Word)n, "GB_Ins_LdNodeTag" ) ;
				gb_assert_IsNotIndirection( (Word)n, "GB_Ins_LdNodeTag" ) ;
				GB_Push(GB_Int2GBInt(GB_NH_Fld_Tag(n->header))) ;
				break ;

			/* nop */
			/*
			case GB_Ins_NOP :
				break ;
			*/

			/* extended instructions */
			case GB_Ins_Ext :
				switch ( *(pc++) )
				{
					/* tailcallt */
					case GB_InsTail_Call(GB_InsOp_LocB_TOS) :
						GB_RetTailCall_Code
							( GB_PopCastIn(GB_BytePtr,dst)
							, pc = dst
							, bpSave = GB_Deref(bp) ; retSave = GB_Deref(GB_RegRel(bp,1)) // retSave = GB_Deref(GB_RegByteRel(GB_Word,spSave,x5))
							, sp = GB_RegByteRel(GB_Word,bp,x4-x3) ; GB_BP_Set
							, GB_SetTOS( bpSave ) ; GB_SetTOSRel( 1, retSave )
							) ;
						break ;

					/* tailcallr */
			
					/* tailapplyt */
					case GB_InsTail_Apply(GB_InsOp_LocB_TOS) :
						GB_PCExtIn(x) ;																						
						GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,2,3),x3) ; 						/* nArgMine  , in bytes				*/	
						GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x4) ; 						/* nArgSurr  , in bytes				*/
						p = sp ;																/* save sp for copying apply args down 	*/
						p2 = GB_RegByteRel(GB_Word,sp,x3) ;										/* end of copy area */
						GB_BP_UnlinkSP ;														/* back to caller context (is return) 	*/
						GB_PopCastIn(GB_BytePtr,pc) ;											/* return address */
						sp = GB_RegByteRel(GB_Word,sp,x4) ;										/* clean up stack			*/
						GB_PushFromTo(p,p2) ;													/* push apply args on adapted stack location */
						goto gb_interpreter_InsApplyEntry ;										/* jump to apply						*/

					/* tailapplyr */
			
					/* tailevalt */
					case GB_InsTail_Eval(GB_InsOp_LocB_TOS) :
%%[[1010
						GB_PCExtIn(x) ;
						GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x2) ; 						/* nArgSurr  , in bytes 			*/
						GB_Skip_CallInfoPtr ;
						pc = Cast(GB_BytePtr,GB_RegRelx(bp,1)) ;								/* continuation address				*/
						x = GB_TOS ;															/* the value to be evaluated		*/
						sp = GB_RegByteRel(GB_Word,bp,x2+GB_CallRetNrBytes-sizeof(GB_Word)) ;	/* sp points to eval arg			*/
						GB_SetTOS( x ) ;														/* which is set here				*/
						GB_BP_Unlink ;															/* in caller context (is return) 	*/
						goto gb_interpreter_InsEvalEntry ;										/* jump to eval						*/
%%]]
		
					// 20091009: under construction
					// case GB_Ins_TailEval(GB_InsOp_LocB_TOS) :
						GB_PCExtIn(x) ;
						GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x2) ; 						/* nArgSurr  , in bytes 			*/
						GB_Skip_CallInfoPtr ;
						IF_GB_TR_ON(3,printf( "GB_Ins_TailEval nArgSurr %x\n", x2 );) ;
						x = GB_TOS ;															/* the value to be evaluated		*/
%%[[1010
%%]]
						WPtr bpNext ;
%%[[1010
						printf( "GB_Ins_TailEval BEF" ) ;
						if (bp) { 
							printf( " k0=%d", GB_FromBPToCallInfo(bp)->kind ) ;
							bpNext = (WPtr)*bp ;
							if (bpNext) { 
								printf( " k1=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
								bpNext = (WPtr)*bpNext ;
								if (bpNext) { 
									printf( " k2=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
									bpNext = (WPtr)*bpNext ;
									if (bpNext) { 
										printf( " k3=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
										bpNext = (WPtr)*bpNext ;
										if (bpNext) { 
											printf( " k4=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
											bpNext = (WPtr)*bpNext ;
											if (bpNext) { 
												printf( " k5=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
												bpNext = (WPtr)*bpNext ;
											}
										}
									}
								}
							}
						}
						printf( "\n" ) ;
%%]]
						switch ( GB_FromBPToCallInfo(bp)->kind ) {
%%[[1010
							case GB_CallInfo_Kind_EvAppFunEvCont :
								if ( (bpNext = (WPtr)*bp) != NULL && GB_FromBPToCallInfo(bpNext)->kind == GB_CallInfo_Kind_TailEvalCont ) {
									bpNext = (WPtr)*bpNext ;										/* remove the intermediate update admin */
									goto gb_interpreter_TailEval_ElimCont ;
								} else {
									goto gb_interpreter_TailEval_Default ;
								}
								break ;
%%]]
							case GB_CallInfo_Kind_EvCont :
								if ( (bpNext = (WPtr)*bp) != NULL && GB_FromBPToCallInfo(bpNext)->kind == GB_CallInfo_Kind_TailEvalCont ) {
									bpNext = (WPtr)*bpNext ;										/* remove the intermediate update admin */
									goto gb_interpreter_TailEval_ElimCont ;
								} else {
									goto gb_interpreter_TailEval_Default ;
								}
								break ;
							case GB_CallInfo_Kind_TailEvalCont :
gb_interpreter_TailEval_ElimCont:
%%[[1010
								do {
%%]]
									bp = bpNext ;														/* remove the intermediate update admin */
									sp = bp - 2 ;														/* go back the place the previous taileval left us */
									GB_NodePtr nOld = Cast(GB_NodePtr,GB_TOS) ;							/* get the node under evaluation */
									GB_UpdWithIndirection_Code_Ind(nOld,x,h,;) ;						/* and let it indirect to the thing to be tailevaluated */
									GB_SetTOS(x) ;														/* go on evaluating with the new value */
%%[[1010
								} while ( (bpNext = (WPtr)*bp) != NULL && GB_FromBPToCallInfo(bpNext)->kind == GB_CallInfo_Kind_TailEvalCont ) ;
%%]]
								break ;
							default :
gb_interpreter_TailEval_Default:
								sp = bp ;															/* remove all locals of current call, then construct frame for taileval */
								GB_Push(x2) ;														/* remember nArgSurr, as arg to taileval cleanup 			*/
								GB_Push( x ) ;														/* value to eval as arg to eval			*/
								break ;
						}
%%[[1010
						// if we are in a call from an eval, which itself is from a taileval, we can (must) shortcut it by replacing it with a node indirection
						// acting as forward indirection to the new evaluation
						WPtr bpEvUpd ;
						if (  bp != NULL
						   && GB_FromBPToCallInfo(bp)->kind == GB_CallInfo_Kind_EvCont
						   && (bpEvUpd = (WPtr)*bp) != NULL
						   && GB_FromBPToCallInfo(bpEvUpd)->kind == GB_CallInfo_Kind_TailEvalCont
						   )
						{			
							bp = (WPtr)*bpEvUpd ;												/* remove the intermediate update admin */
							sp = bp - 2 ;														/* go back the place the previous taileval left us */
							GB_NodePtr nOld = Cast(GB_NodePtr,GB_TOS) ;							/* get the node under evaluation */
							GB_UpdWithIndirection_Code_Ind(nOld,x,h,;) ;						/* and let it indirect to the thing to be tailevaluated */
							GB_SetTOS(x) ;														/* go on evaluating with the new value */
						} else
%%]]
%%[[1010
						{
							sp = bp ;															/* remove all locals of current call, then construct frame for taileval */
							GB_Push(x2) ;														/* remember nArgSurr, as arg to taileval cleanup 			*/
							GB_Push( x ) ;														/* value to eval as arg to eval			*/
						}
%%]]
						pc = &gb_code_AfterTailEvalCall[sizeof(GB_CallInfo_Inline)] ;			/* ret addr is to taileval cleanup 			*/
%%[[1010
						printf( "GB_Ins_TailEval AFT" ) ;
						if (bp) { 
							printf( " k0=%d", GB_FromBPToCallInfo(bp)->kind ) ;
							bpNext = (WPtr)*bp ;
							if (bpNext) { 
								printf( " k1=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
								bpNext = (WPtr)*bpNext ;
								if (bpNext) { 
									printf( " k2=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
									bpNext = (WPtr)*bpNext ;
									if (bpNext) { 
										printf( " k3=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
										bpNext = (WPtr)*bpNext ;
										if (bpNext) { 
											printf( " k4=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
											bpNext = (WPtr)*bpNext ;
											if (bpNext) { 
												printf( " k5=%d", GB_FromBPToCallInfo(bpNext)->kind ) ;
												bpNext = (WPtr)*bpNext ;
											}
										}
									}
								}
							}
						}
						printf( "\n" ) ;
%%]]
						goto gb_interpreter_InsEvalEntry ;										/* jump to eval						*/
		
					case GB_InsExt_Halt:
						goto interpretIsDone ;
						break ;

					case GB_InsExt_TagInt2Word:
						gb_assert_IsNotIndirection( GB_TOS, "GB_InsExt_TagInt2Word" ) ;
						GB_SetTOS( GB_Int2GBInt( GB_TOS ) ) ;
						break ;

					case GB_InsExt_UntagWord2Int:
						gb_assert_IsNotIndirection( GB_TOS, "GB_InsExt_UntagWord2Int" ) ;
						GB_SetTOS( GB_GBInt2Int( GB_TOS ) ) ;
						break ;

					case GB_InsExt_TagWord2Word:
						gb_assert_IsNotIndirection( GB_TOS, "GB_InsExt_TagWord2Word" ) ;
						GB_SetTOS( GB_TagWord2Word( GB_TOS ) ) ;
						break ;

					case GB_InsExt_UntagWord2Word:
						gb_assert_IsNotIndirection( GB_TOS, "GB_InsExt_UntagWord2Word" ) ;
						GB_SetTOS( GB_UntagWord2Word( GB_TOS ) ) ;
						break ;

%%[[96
					case GB_InsExt_ResetThrownException:
						IF_GB_TR_ON(3,{printf("GB_InsExt_ResetThrownException: gb_ThrownException = %p, gb_ThrownException_NrOfEvalWrappers = %d\n", gb_ThrownException, gb_ThrownException_NrOfEvalWrappers) ;}) ;
						gb_ThrownException = NULL ;
						gb_ThrownException_NrOfEvalWrappers = 0 ;
						break ;
%%]]

					default :
						gb_panic1_1( "extended instruction not implemented", *(pc-1) ) ;
						break ;
				}

				break ;

			/* operators */
			/* int add/sub/mul/quot to TOS from XXX */
			case GB_Ins_Op(GB_InsOp_TyOp_Add,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS) :
				switch( *(pc++) )
				{
					GB_Op_TOSDst_Case_Code( GB_Int_Add )

					default:
						gb_panic1_1( "oaiwt<XXX> instruction not implemented", *(pc-1) ) ;
						break ;
				}
				break ;

			case GB_Ins_Op(GB_InsOp_TyOp_Sub,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS) :
				switch( *(pc++) )
				{
					GB_Op_TOSDst_Case_Code( GB_Int_Sub )

					default:
						gb_panic1_1( "osiwt<XXX> instruction not implemented", *(pc-1) ) ;
						break ;
				}
				break ;

			case GB_Ins_Op(GB_InsOp_TyOp_Mul,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS) :
				switch( *(pc++) )
				{
					GB_Op_TOSDst_Case_Code( GB_Int_Mul )

					default:
						gb_panic1_1( "omiwt<XXX> instruction not implemented", *(pc-1) ) ;
						break ;
				}
				break ;

			case GB_Ins_Op(GB_InsOp_TyOp_Quot,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS) :
				switch( *(pc++) )
				{
					GB_Op_TOSDst_Case_Code( GB_Int_Quot )

					default:
						gb_panic1_1( "odiwt<XXX> instruction not implemented", *(pc-1) ) ;
						break ;
				}
				break ;

			default:
				gb_panic1_1( "instruction not implemented", *(pc-1) ) ;
				break ;

		}
		
#		if GB_COUNT_STEPS
			gb_StepCounter++ ;
#		endif

	}
	
	interpretIsDone: ;
  	// GB_GCSafe_Leave ;

}
%%]			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

See GBM doc for explanation.

%%[96
GB_Word gb_intl_primCatchException( GB_Word e, GB_Word handler )
{
	IF_GB_TR_ON(3,{printf("gb_intl_primCatchException1 bp %p\n", bp) ;}) ;
	GB_Push( e ) ;																							// build stack frame which just returns the value
	GB_Push( 1 ) ;
	GB_Push( Cast(GB_Word,&gb_code_ExcHdl_NormalReturn_MarkedAsHandler[sizeof(GB_CallInfo_Inline)]) ) ;		// return, marked as handler
	GB_BP_Link ;
	
	IF_GB_TR_ON(3,{printf("gb_intl_primCatchException2 bp %p\n", bp) ;}) ;
	IF_GB_TR_ON(3,{GB_CallInfo* ci ;ci = GB_FromBPToCallInfo(bp) ;gb_prCallInfo( ci );printf("\n");}) ;
	
	GB_Push( handler ) ;																					// build adapted copy of primitive stack frame
	GB_Push( e ) ;
	GB_Push( 2 ) ;
	GB_Push( Cast(GB_Word,&gb_code_ExcHdl_EvalValue[sizeof(GB_CallInfo_Inline)]) ) ;						// with return to evaluation code
	GB_BP_Link ;
	
	IF_GB_TR_ON(3,{printf("gb_intl_primCatchException3 bp %p\n", bp) ;}) ;
	IF_GB_TR_ON(3,{GB_CallInfo* ci2 ;ci2 = GB_FromBPToCallInfo(bp) ;gb_prCallInfo( ci2 );printf("\n");}) ;
	
	return e ;
}

GB_NodePtr gb_intl_throwException( GB_Word exc )
{
	GB_Ptr p ;
	GB_CallInfo* ci ;
	GB_NodePtr thrownExc ;
	GB_NodePtr reifiedBackTrace ;
	GB_GCSafe_Enter ;
	GB_GCSafe_1(exc) ;
	GB_GCSafe_2_Zeroed(thrownExc,reifiedBackTrace) ;
	
	gb_ThrownException_NrOfEvalWrappers = 0 ;
	
	GB_MkListNil(reifiedBackTrace) ;
	
	IF_GB_TR_ON(3,{printf("gb_intl_throwException bp %p : ", bp) ; printf("\n");}) ;
	for ( p = bp
	    ; p != NULL && ((ci = GB_FromBPToCallInfo(p))->kind) != GB_CallInfo_Kind_Hdlr
	    ; p = Cast(GB_Ptr,*p)
	    )
	{
		IF_GB_TR_ON(3,{printf("gb_intl_throwException:callInfo1: p %p : ", p) ; gb_prCallInfo( ci ); printf("\n");}) ;
		GB_Ptr p2 ;
		if ( ci->kind == GB_CallInfo_Kind_EvCont && (p2 = Cast(GB_Ptr,*p)) != NULL ) {						// if we are in a continuation of an eval then patch it
			GB_CallInfo* ci2 = GB_FromBPToCallInfo(p2) ;
			if ( ci2->kind == GB_CallInfo_Kind_Eval || ci2->kind == GB_CallInfo_Kind_EvalWrap || ci2->kind == GB_CallInfo_Kind_EvalTopWrap ) {
				if ( thrownExc == NULL ) {
					GB_MkCFunNode1In(thrownExc,&gb_intl_throwException,exc) ;									// if not done already, construct exception throwing thunk
				}
				GB_NodePtr nOld = Cast(GB_NodePtr,GB_RegRelx(p2,2)) ;
				IF_GB_TR_ON(3,{printf("gb_intl_throwException:callInfo2: p %p p2 %p nOld %p: ", p, p2, nOld) ; gb_prCallInfo( ci2 ); printf("\n");}) ;
				GB_UpdWithIndirection_Code_Tmp(nOld,Cast(GB_Word,thrownExc)) ;											// update node under evaluation with exception throwing thunk
			}
		} else if ( ci->kind == GB_CallInfo_Kind_EvalTopWrap ) {
			gb_ThrownException_NrOfEvalWrappers++ ;
		}
		if ( gb_prCallInfoIsVisible(ci) && ci->name ) {
			GB_NodePtr n1, n2, n3 ;
			GB_GCSafe_Enter ;
			GB_GCSafe_3_Zeroed(n1, n2, n3) ;
			GB_MkCFunNode1In(n1,primCStringToString,ci->name) ;
			GB_MkTupNode2_In(n2,GB_Int2GBInt(ci->kind),n1) ;
			n3 = reifiedBackTrace ;
			GB_MkListCons(reifiedBackTrace,n2,n3) ;
			GB_GCSafe_Leave ;
		}
	}
	IF_GB_TR_ON(3,{printf("gb_intl_throwException:callInfo3: ") ; gb_prCallInfo( ci ); printf("\n");}) ;
	
	if ( p != NULL ) {
		sp = bp = p ;																						// stack is unwound to handler frame
		GB_SetSPRel( 1, Cast(GB_Word,&gb_code_ExcHdl_ThrowReturn[sizeof(GB_CallInfo_Inline)]) ) ;			// patch return address so primitive returns to handler calling code
	} else {
		gb_panic( "uncaught exception" ) ;
	}
	IF_GB_TR_ON(3,{printf("gb_intl_throwException:4: sp=%p bp=%p\n", sp, bp) ;}) ;
	
	GB_MkTupNode2_In(thrownExc,reifiedBackTrace,exc) ;																// tuple with backtrace
	GB_GCSafe_Leave ;
	return (gb_ThrownException = thrownExc) ;
}

GB_NodePtr gb_intl_throwExceptionFromPrim( GB_NodePtr exc )
{
	return gb_intl_throwException( Cast(GB_Word,exc) ) ;
}

GB_NodePtr gb_intl_throwIOErrorFromPrim( GB_NodePtr ioe_handle, GB_Word ioe_type, GB_NodePtr ioe_filename, char* strErr )
{
	GB_NodePtr ioe_location ;
	GB_NodePtr ioe_description ;
	GB_NodePtr ioe ;
	GB_NodePtr exc ;
	GB_GCSafe_Enter ;
	GB_GCSafe_3(ioe_handle,ioe_type,ioe_filename) ;
	GB_GCSafe_4_Zeroed(ioe_location,ioe_description,ioe,exc) ;

	GB_MkListNil( ioe_location ) ;
	ioe_description = primCStringToString( strErr ) ;
	GB_MkIOErrorIOError( ioe, ioe_handle, GB_Int2GBInt(ioe_type), ioe_location, ioe_description, ioe_filename ) ;
	GB_MkExceptionIOError( exc, ioe ) ;
	
	GB_GCSafe_Leave ;
	return gb_intl_throwExceptionFromPrim( exc ) ;
}

static char gb_intl_throwStackOverflow_Buffer[100] ;

GB_NodePtr gb_intl_throwStackOverflow( GB_FunctionInfo* functionInfo )
{
	GB_NodePtr exc, stk, str ;
	GB_GCSafe_Enter ;
	GB_GCSafe_3_Zeroed(exc, stk, str) ;

	sprintf( gb_intl_throwStackOverflow_Buffer, "Stack sp=%p [%p(+spare=%x)..%p], requested=%x\n", sp, StackAreaLow, STACKSIZE_SPARE_UNUSED, StackAreaHigh, functionInfo->szStack ) ;
	str = primCStringToString( gb_intl_throwStackOverflow_Buffer ) ;
	GB_MkAsyncException_StackOverflow(stk,str) ;
	GB_MkExceptionAsyncException(exc,stk) ;
	
	GB_GCSafe_Leave ;
	return gb_intl_throwExceptionFromPrim( exc ) ;
}


%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void gb_exit( int i )
{
	GB_Ptr p = bp ;
	while ( p != NULL )
	{
		GB_CallInfo* ci = GB_FromBPToCallInfo(p) ;
		if ( gb_prCallInfoIsVisible( ci ) )
		// if ( True )
		{
			printf( "  bp=%p ", p ) ;
			gb_prCallInfo( ci ) ;
			printf( "\n" ) ;
		}
		p = Cast(GB_Ptr,*p) ;
	}
	exit( i ) ;
}
%%]			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void gb_Initialize()
{
	GB_InitPatch_gb_code_Eval ;
	GB_InitPatch_gb_code_AfterEvalCall ;
	GB_InitPatch_gb_code_AfterTailEvalCall ;
	GB_InitPatch_gb_code_AfterEvalApplyFunCall ;
	GB_InitPatch_gb_code_AfterCallInApplyWithTooManyArgs ;
%%[[96
	GB_InitPatch_gb_code_ExcHdl_EvalValue ;
	GB_InitPatch_gb_code_ExcHdl_NormalReturn_MarkedAsHandler ;
	GB_InitPatch_gb_code_ExcHdl_ThrowReturn ;
	// GB_InitPatch_gb_code_ThrowException ;
%%]]
%%[[99
	// GB_InitPatch_gb_code_Startup ;
%%]]
	sp = Cast(GB_Ptr,StackAreaHigh) ;
	bp = Cast(GB_Ptr,0) ;

#	if USE_EHC_MM
		GB_MkConNodeN_Rooted(gb_Nil,0,GB_Tag_List_Nil) ;
%%[[98
		GB_MkConNodeN_Rooted(gb_Nothing,0,GB_Tag_Maybe_Nothing) ;
%%]]
#	endif

%%[[97
	prim_integer_Initialize() ;
%%]]

}

%%]

%%[8
void gb_SetModTable( GB_ModEntry* modTbl, GB_Word modTblSz )
{
	gb_AllMod 		= modTbl ;
	gb_AllModSize 	= modTblSz ;
}
%%]

%%[8
void gb_InitTables
	( GB_BytePtr byteCodes, int byteCodesSz
	// , GB_LinkEntry* linkEntries, int linkEntriesSz
	, HalfWord* cafGlEntryIndices, int cafGlEntryIndicesSz
	, GB_BytePtr* globalEntries, int globalEntriesSz
	, GB_Word* consts
	// , GB_GCInfo* gcInfos
	, GB_GCStackInfo* gcStackInfos
	, GB_LinkChainResolvedInfo* linkChainInds
	, GB_CallInfo* callinfos
	, GB_FunctionInfo* functionInfos
	, BPtr bytePool
	, Word linkChainOffset
%%[[20
	, GB_ImpModEntry* impModules, int impModulesSz
	, GB_NodePtr* expNode, int expNodeSz, int* expNodeOffs
	, GB_ModEntry* modTbl
%%]]
	)
{
	int i, j ;
	GB_Ptr p ;

#	if USE_EHC_MM
		GB_GC_Module gbMod
			= { globalEntries, globalEntriesSz, cafGlEntryIndices, cafGlEntryIndicesSz
%%[[20
			  , expNode
%%]]
			  } ;
		GB_GC_RegisterModule( &gbMod ) ;
#	endif

%%[[20
	for ( i = 0 ; i < impModulesSz ; i++ ) {
		impModules[i].globModInx = gb_lookupModEntry( impModules[i].name, modTbl ) ;
		IF_GB_TR_ON(3,{printf("imp mod %s globInx %d", impModules[i].name, impModules[i].globModInx) ; printf("\n");}) ;
	}
%%]]

	for ( i = 0 ; i < cafGlEntryIndicesSz ; i++ ) {
		GB_BytePtr* e = &globalEntries[ cafGlEntryIndices[i] ] ;
		GB_NodePtr n ;
		GB_MkCafNodeIn(n,*e) ;
		*e = Cast(GB_BytePtr,n) ;
	}

	for ( ; linkChainOffset > 0 ; ) {
		WPtr loc = (WPtr)(&byteCodes[ linkChainOffset ]) ;
		Word link = *loc ;
		Word info, kind, off ;
		if ( GB_LinkChainKind_Fld_Enc( link ) == GB_LinkChainEncoding_Ind ) {
			GB_LinkChainResolvedInfo* ind = &linkChainInds[ GB_LinkChainKind_Ind_Fld_Inx(link) ] ;
			kind = ind->kind ;
			info = ind->info ;
			off  = ind->off ;
		} else {
			kind = GB_LinkChainKind_Fld_Kind(link) ;
			info = GB_LinkChainKind_Fld_Info(link) ;
			off  = GB_LinkChainKind_Fld_Off(link) ;
		}
		IF_GB_TR_ON(3,{printf("gb_InitTables A linkChainOffset=%x, loc=%p, link=%x, enc=%x, kind=%d, info=%x, off=%x\n", linkChainOffset, loc, link, GB_LinkChainKind_Fld_Enc( link ), kind, info, GB_LinkChainKind_Fld_Off( link )) ; }) ;
		switch ( kind ) {
			case GB_LinkChainKind_GCInfo :
				// IF_GB_TR_ON(3,{printf("gb_InitTables gcInfo=%x %p\n", gcInfos[ info ].nrOfTOS_No_GCTrace, &gcInfos[ info ]) ; }) ;
				IF_GB_TR_ON(3,{printf("gb_InitTables gcStackInfo=%p->%x\n", ( info > 0 ? (&gcStackInfos[ info-1 ]) : NULL ), ( info > 0 ? (gcStackInfos[ info-1 ].sz) : 0 )) ; }) ;
				// *loc = (Word)(&gcInfos[ info ]) ;
				*loc = (Word)( info > 0 ? (&gcStackInfos[ info-1 ]) : NULL ) ;				
				break ;

			case GB_LinkChainKind_Const :
				*loc = (Word)(&consts[ info ]) ;
				break ;

			case GB_LinkChainKind_Code :
				*loc = (Word)(&globalEntries[ info ]) ;
				break ;

			case GB_LinkChainKind_CallInfo :
				*loc = (Word)(&callinfos[ info ]) ;
				break ;

			case GB_LinkChainKind_FunctionInfo :
				*loc = (Word)(&functionInfos[ info ]) ;
				break ;

			case GB_LinkChainKind_StringConst :
				*loc = (Word)(&bytePool[ info ]) ;
				break ;

			case GB_LinkChainKind_Offset :
				*loc = Cast(Word,loc+1) + info ;
				break ;

			case GB_LinkChainKind_Offsets :
				*loc = info ;
/*
*/
				{
					WPtr p ;
					Word j ;
					for ( j = 0, p = loc+1 ; j < info ; j++ ) {
						p[j] = Cast(Word,&p[j+1]) + p[j] ;
					}
				}
				break ;

%%[[20
			case GB_LinkChainKind_ImpEntry :
				*loc = (Word)(modTbl[ impModules[ info ].globModInx ].expNode) ;
				break ;
%%]]

			default :
				break ;
		}
		linkChainOffset = ( off > 0 ? linkChainOffset + off : 0 ) ;
		IF_GB_TR_ON(3,{printf("gb_InitTables B linkChainOffset=%x off=%x\n", linkChainOffset, off) ; }) ;
	}

	/*
	for ( i = 0 ; i < linkEntriesSz ; i++ )
	{
		p = Cast(GB_Ptr,linkEntries[i].linkLoc) ;
		switch ( linkEntries[i].tblKind )
		{
			case GB_LinkTbl_EntryKind_CodeEntry :
				IF_GB_TR_ON(3,{printf("link CodeEntry") ; printf("\n");}) ;
				*p = Cast(GB_Word,globalEntries[ linkEntries[i].linkVal ]) ;
				IF_GB_TR_ON(3,{printf("link CodeEntry p %p v %x", p, globalEntries[ linkEntries[i].linkVal ]) ; printf("\n");}) ;
				break ;

			case GB_LinkTbl_EntryKind_PatchCode_Deref0 :
				IF_GB_TR_ON(3,{printf("link PatchCode_Deref0") ; printf("\n");}) ;
				*p = linkEntries[i].linkVal ;
				IF_GB_TR_ON(3,{printf("link PatchCode_Deref0 p %p v %x", p, linkEntries[i].linkVal) ; printf("\n");}) ;
				break ;

			case GB_LinkTbl_EntryKind_PatchCode_Deref1 :
				IF_GB_TR_ON(3,{printf("link PatchCode_Deref1") ; printf("\n");}) ;
				*p = GB_Deref(Cast(GB_Ptr,linkEntries[i].linkVal)) ;
				IF_GB_TR_ON(3,{printf("link PatchCode_Deref1 p %p v %x", p, linkEntries[i].linkVal) ; printf("\n");}) ;
				break ;

			case GB_LinkTbl_EntryKind_PatchCode_Deref2 :
				IF_GB_TR_ON(3,{printf("link PatchCode_Deref2") ; printf("\n");}) ;
				*p = GB_Deref(Cast(GB_Ptr,GB_Deref(Cast(GB_Ptr,linkEntries[i].linkVal)))) ;
				IF_GB_TR_ON(3,{printf("link PatchCode_Deref2 p %p v %x", p, GB_Deref(Cast(GB_Ptr,linkEntries[i].linkVal))) ; printf("\n");}) ;
				break ;

			case GB_LinkTbl_EntryKind_PatchOffsets :
				for ( j = 0 ; j < linkEntries[i].linkVal ; j++ )
				{
					IF_GB_TR_ON(3,{printf("link PatchOffsets") ; printf("\n");}) ;
					p[j] = Cast(GB_Word,&p[j+1]) + p[j] ;
					IF_GB_TR_ON(3,{printf("link PatchOffsets i %d p %p v %x", j, &p[j], Cast(GB_Word,&p[j+1]) + p[j]) ; printf("\n");}) ;
				}
				break ;

%%[[20
			case GB_LinkTbl_EntryKind_ImpEntry :
				IF_GB_TR_ON(3,{printf("link ImpEntry p=%p linkVal=%x", p, (linkEntries[i].linkVal)) ; printf("\n");}) ;
				IF_GB_TR_ON(3,{printf("link ImpEntry p=%p expNode=%x", p, (modTbl[ impModules[ linkEntries[i].linkVal ].globModInx ].expNode)) ; printf("\n");}) ;
				*p = Cast(GB_Word,(modTbl[ impModules[ linkEntries[i].linkVal ].globModInx ].expNode)) ;
				// IF_GB_TR_ON(3,{printf("link ImpEntry p=%x v=%x", p, (modTbl[ linkEntries[i].linkVal ].expNode)) ; printf("\n");}) ;
				break ;
%%]]

		}
	}
	*/
	
%%[[20
	for ( i = 0 ; i < expNodeSz ; i++ )
	{
		(*expNode)->content.fields[i] = Cast(GB_Word,globalEntries[ expNodeOffs[i] ]) ;
		IF_GB_TR_ON(3,{printf("link nd=%p flds=%p exp i=%d/%x off=%x p=%x glob[off]=%x", (*expNode), (*expNode)->content.fields, i, i, expNodeOffs[i], &(*expNode)->content.fields[i], globalEntries[ expNodeOffs[i] ]) ; printf("\n");}) ;
	}
%%]]
	
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Sanity check on assumptions made by interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void gb_checkInterpreterAssumptions()
{
	char* m = "interpreter sanity check" ;
	GB_Ptr p ;
	GB_Word x1, x2 ;
	GB_Node n ;
	GB_NodePtr np ;
	
	// printf( "size node=%d\n", sizeof(n)) ;
	
	if ( sizeof( GB_Word ) != sizeof( GB_Ptr ) ) {
		gb_panic2_1( m, "size of word and pointer must be equal", 0 ) ;
	}
	
	if ( sizeof(Word) != sizeof(GB_Word) ) {
		gb_panic2_1( m, "size of Word and GB_Word must be equal", 0 ) ;
	}
	
	if ( sizeof( GB_Word ) != sizeof(uint32_t) && sizeof( GB_Word ) != sizeof(uint64_t) ) {
		gb_panic2_1( m, "size of word and pointer must be 32 or 64", 0 ) ;
	}
	
	GB_MkConNodeN(np,1,0) ;
	// x1 = Cast(GB_Word,GB_HeapAlloc_Words( 2 )) ;
	x1 = Cast(GB_Word,np) ;
	if ( ! x1 ) {
		gb_panic2_1( m, "heap allocation yields zero pointer", x1 ) ;
	}
	if ( x1 & GB_Word_TagMask ) {
		gb_panic2_2( m, "heap allocated pointers must have lower bits set to zero", GB_Word_SizeOfWordTag, x1 ) ;
	}
	
	if ( Cast(GB_Word,&n) + sizeof(GB_NodeHeader) != Cast(GB_Word,n.content.fields) ) {
		gb_panic2_1( m, "node header improperly aligned", Cast(GB_Word,n.content.fields) - Cast(GB_Word,&n) ) ;
	}
	
/*
	x1 = Cast(GB_Word,gb_False) ;
	if ( x1 & GB_Word_TagMask ) {
		gb_panic2_2( m, "statically allocated nodes must must be word aligned", GB_Word_SizeOfWordTag, x1 ) ;
	}
*/
	
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
int gb_lookupModEntry( char* modNm, GB_ModEntry* modTbl )
{
	int i ;
	for ( i = 0 ; modTbl[i].name != NULL && strcmp( modTbl[i].name, modNm ) != 0 ; i++ ) ;
	if ( modTbl[i].name == NULL )
		gb_panic2( "module lookup", modNm ) ;
	return i ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Debug info lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static int gb_CmpEntryPoint( const void* x, const void* y )
{
	GB_BytePtr pc = Cast(GB_BytePtr,x) ;
	GB_ByteCodeEntryPoint* e = Cast(GB_ByteCodeEntryPoint*,y) ;
	GB_ByteCodeInstrEntry* i1 = e->bcInstr ;
	GB_ByteCodeInstrEntry* i2 = &e->bcInstr[e->bcInstrSize - 1] ;
	if ( pc < i1->bcLoc )
		return -1 ;
	else if ( pc >= i2->bcLoc + i2->bcSize )
		return 1 ;
	else
		return 0 ;
}

static int gb_CmpInstrEntry( const void* x, const void* y )
{
	GB_BytePtr pc = Cast(GB_BytePtr,x) ;
	GB_ByteCodeInstrEntry* i = Cast(GB_ByteCodeInstrEntry*,y) ;
	if ( pc < i->bcLoc )
		return -1 ;
	if ( pc >= i->bcLoc + i->bcSize )
		return 1 ;
	else
		return 0 ;
}

int gb_lookupInfoForPC
	( GB_BytePtr pc
	, GB_ByteCodeModule** m
	, GB_ByteCodeEntryPoint** e
	, GB_ByteCodeInstrEntry** i )
{
	int mc, ec, ic ;
	for ( mc = 0 ; mc < gb_AllModSize ; mc++ )
	{
		*m = gb_AllMod[mc].bcModule ;
		// printf("*m=%x\n", *m) ;
		if ( pc >= (*m)->bcLoc && pc < (*m)->bcLoc + (*m)->bcSize && (*m)->bcEntry != NULL && (*m)->bcEntry[0].bcInstr != NULL )
		{
			*e = Cast( GB_ByteCodeEntryPoint*, bsearch( pc, (*m)->bcEntry, (*m)->bcEntrySize, sizeof(GB_ByteCodeEntryPoint), gb_CmpEntryPoint ) ) ;
			// printf("*m=%x *e=%x\n", *m, *e) ;
			if ( *e && (*e)->bcInstr )
			{
				*i = Cast( GB_ByteCodeInstrEntry*, bsearch( pc, (*e)->bcInstr, (*e)->bcInstrSize, sizeof(GB_ByteCodeInstrEntry), gb_CmpInstrEntry ) ) ;
				// printf("*m=%x *e=%x *i=%x\n", *m, *e, *i) ;
				if ( *i ) {
					return True ;
				}
			}
		}
	}
	return False ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GMP memory allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Todo: finalizer for USE_EHC_MM

%%[97
#if USE_GMP
void* gb_Alloc_GMP( size_t nBytes )
{
#	if USE_EHC_MM
		void *n ;
		n = GB_HeapAlloc_Bytes_Fixed( nBytes ) ;
		IF_GB_TR_ON(3,{printf("gb_Alloc_GMP n=%p\n",n);}) ;
		return n ;
#	else
		GB_Node* n ;
		GB_NodeAlloc_GMP_In(nBytes,n) ;
		IF_GB_TR_ON(3,{printf("gb_Alloc_GMP B n=%p\n",n);}) ;
		return n->content.fields ;		/* return ptr to usable area */
#	endif
}

void* gb_ReAlloc_GMP( void *n, size_t nBytesOld, size_t nBytes )
{
	if ( nBytes > nBytesOld )
	{
		void *nNew ;
#		if USE_EHC_MM
			nNew = GB_HeapAlloc_Bytes_Fixed( nBytes ) ;
			IF_GB_TR_ON(3,{printf("gb_ReAlloc_GMP nNew=%p\n",nNew);}) ;
#		else
			GB_NodePtr nNewNode ;
			GB_NodeAlloc_GMP_In(nBytes,nNewNode) ;
			nNew = nNewNode->content.fields ;
#		endif
		memcpy( nNew, n, nBytesOld ) ;
#		if USE_EHC_MM
			GB_HeapFree_Fixed( n ) ;
#		endif
		return nNew ;
	}
	return n ;
}

void gb_Free_GMP( void *n, size_t nBytesOld )
{
#	if USE_EHC_MM
		IF_GB_TR_ON(3,{printf("gb_Free_GMP n=%p\n",n);}) ;
		// printf(">gb_Free_GMP n=%p\n",n);fflush(stdout);
		GB_HeapFree_Fixed( n ) ;
		// printf("<gb_Free_GMP n=%p\n",n);fflush(stdout);
#	endif
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing, misc info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Instruction mnemonic table

%%[8
#if TRACE || DUMP_INTERNALS
typedef struct GB_Mnem {
	GB_Byte		code ;
	char*		mnem ;
} GB_Mnem ;

static GB_Mnem gb_mnemTable[] =
{ { GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_08)
  , "l0ti08"
  }
, { GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_16)
  , "l0ti16"
  }
, { GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_32)
  , "l0ti32"
  }
, { GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_64)
  , "l0ti64"
  }
, { GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_08)
  , "liti08"
  }
, { GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_16)
  , "liti16"
  }
, { GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_32)
  , "liti32"
  }
, { GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_64)
  , "liti64"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_08)
  , "l1ts08"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_16)
  , "l1ts16"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_32)
  , "l1ts32"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_64)
  , "l1ts64"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_Reg, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_08)
  , "l1rs08"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_Reg, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_16)
  , "l1rs16"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_Reg, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_32)
  , "l1rs32"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_Reg, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_64)
  , "l1rs64"
  }
, { GB_Ins_Ld(GB_InsOp_Deref2, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_08)
  , "l2ts08"
  }
, { GB_Ins_Ld(GB_InsOp_Deref2, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_16)
  , "l2ts16"
  }
, { GB_Ins_Ld(GB_InsOp_Deref2, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_32)
  , "l2ts32"
  }
, { GB_Ins_Ld(GB_InsOp_Deref2, GB_InsOp_LocB_TOS, GB_InsOp_LocE_SP, GB_InsOp_ImmSz_64)
  , "l2ts64"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_08)
  , "l1tr08"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_16)
  , "l1tr16"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_32)
  , "l1tr32"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_64)
  , "l1tr64"
  }
, { GB_Ins_Op(GB_InsOp_TyOp_Add,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS)
  , "oaiwt XXX"
  }
, { GB_Ins_Op(GB_InsOp_TyOp_Sub,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS)
  , "osiwt XXX"
  }
, { GB_Ins_Op(GB_InsOp_TyOp_Mul,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS)
  , "omiwt XXX"
  }
, { GB_Ins_Op(GB_InsOp_TyOp_Quot,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS)
  , "oqiwt XXX"
  }
, { GB_Ins_Ldg(GB_InsOp_LocB_TOS)				, "ldgt" 			}
, { GB_Ins_Ldg(GB_InsOp_LocB_Reg)				, "ldgr" 			}
, { GB_Ins_Call(GB_InsOp_LocB_TOS)				, "callt" 			}
// , { GB_Ins_TailCall(GB_InsOp_LocB_TOS)			, "tailcallt" 		}
, { GB_Ins_AllocStore(GB_InsOp_LocB_TOS)		, "allocstoret" 	}
, { GB_Ins_Fetch(GB_InsOp_LocB_TOS)				, "fetcht" 			}
, { GB_Ins_RetCall								, "retcall" 		}
, { GB_Ins_RetCase								, "retcase" 		}
, { GB_Ins_CaseCall								, "casecall" 		}
, { GB_Ins_CallC								, "callc"	 		}
, { GB_Ins_Eval(GB_InsOp_LocB_TOS)				, "evalt" 			}
// , { GB_Ins_TailEval(GB_InsOp_LocB_TOS)			, "tailevalt" 		}
, { GB_Ins_EvalUpdCont							, "evupdcont" 		}
, { GB_Ins_Apply(GB_InsOp_LocB_TOS)				, "applyt" 			}
, { GB_Ins_Ext									, "ext" 			}
, { GB_Ins_EvalApplyCont						, "evappcont" 		}
, { GB_Ins_LdNodeTag							, "lnt"		 		}
, { GB_Ins_FetchUpdate							, "fetchupd" 		}
, { GB_Ins_PApplyCont							, "papplycont" 		}
, { GB_Ins_TailEvalCont							, "tailevalcont" 	}
// , { GB_Ins_NOP									, "nop" 			}
, { 0											, "--" 				}	/* this must be the last one */
} ;

char* gb_lookupMnem( GB_Byte c )
{
	GB_Mnem *mn = gb_mnemTable ;
	for ( ; mn->code != 0 && mn->code != c ; mn++ ) ;
	return mn->mnem ;
}

#endif

%%]
