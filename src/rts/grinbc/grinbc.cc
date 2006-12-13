%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define USE_REGS_FOR_PC_SP 		1

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Register usage (adapted from lvm evaluator.c)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if defined(__GNUC__) && !defined(DEBUG)
#ifdef __i386__
# define PC_REG asm("%esi")
# define SP_REG asm("%edi")
# define FP_REG
#endif
#ifdef __x86_64__
#define PC_REG asm("6")
#define SP_REG asm("7")
#define FP_REG
#undef USE_REGS_FOR_PC_SP
#endif
#ifdef __mips__
#define PC_REG asm("$16")
#define SP_REG asm("$17")
#define FP_REG asm("$18")
#endif
#ifdef __sparc__
#define PC_REG asm("%l0")
#define SP_REG asm("%l1")
#define FP_REG asm("%l2")
#endif
#ifdef __alpha__
#ifdef __CRAY__
#define PC_REG asm("r9")
#define SP_REG asm("r10")
#define FP_REG asm("r11")
#define INSTR_BASE_REG asm("r12")
#else
#define PC_REG asm("$9")
#define SP_REG asm("$10")
#define FP_REG asm("$11")
#define INSTR_BASE_REG asm("$12")
#endif
#endif
#if defined(PPC) || defined(_ARCH_PPC) || defined(_POWER) || defined(_IBMR2)
#define RR_REG asm("25")
#define PC_REG asm("26")
#define SP_REG asm("27")
#define FP_REG asm("28")
#endif
#ifdef __hppa__
#define PC_REG asm("%r18")
#define SP_REG asm("%r17")
#define FP_REG asm("%r16")
#endif
#ifdef __mc68000__
#define PC_REG asm("a5")
#define SP_REG asm("a4")
#define FP_REG asm("d7")
#endif
#ifdef __arm__
#define PC_REG asm("r9")
#define SP_REG asm("r8")
#define FP_REG asm("r7")
#endif
#ifdef __ia64__
#define PC_REG asm("36")
#define SP_REG asm("37")
#define FP_REG asm("38")
#define INSTR_BASE_REG asm("39")
#endif
#endif  /* GNUC & DEBUG */

%%]

%%[8
#if USE_REGS_FOR_PC_SP
register GB_BytePtr  pc PC_REG ;
register GB_Ptr      sp SP_REG ;
#else
static   GB_BytePtr  pc ;
static   GB_Ptr      sp ;
#endif

#if defined(RR_REG) && USE_REGS_FOR_PC_SP
register GB_Word     rr RR_REG ;
#else
static   GB_Word     rr ;
#endif
%%]

%%[8
#define GB_RegRel(r,o)			((r)+(o))
#define GB_RegRelx(r,o)			GB_Deref(GB_RegRel(r,o))
#define GB_PCRel(o)				GB_RegRel(pc,o)

#define GB_RegByteRel(ty,r,o)	Cast(ty*,((Cast(GB_BytePtr,r))+(o)))
#define GB_RegByteRelx(r,o)		GB_Deref(GB_RegByteRel(GB_Word,r,o))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef GB_Word GB_CFun();
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Push(v)				{*(--sp) = ((GB_Word)(v)) ; }
#define GB_Push2(v)				{*(sp-1) = ((GB_Word)(v)) ; sp-- ;}		/* avoid predecrement */
#define GB_TOS					(*sp)
#define GB_SetTOS(x)			{*sp = (x);}
#define GB_Popn(n)				(sp+=(n))
#define GB_Pop					(sp++)
#define GB_PopCastedIn(ty,v)	{(v) = Cast(ty,*GB_Pop) ;}
#define GB_PopIn(v)				GB_PopCastedIn(GB_Word,v)

#define GB_PopnUpdTOS(n,x)		{sp += (n) ; GB_SetTOS(x) ; }

#define GB_SPRel(o)				GB_RegRel(sp,o)
#define GB_SPRelx(o)			GB_Deref(GB_SPRel(o))

#define GB_SPByteRel(ty,o)		GB_RegByteRel(ty,sp,o)
#define GB_SPByteRelx(o)		GB_Deref(GB_SPByteRel(GB_Word,o))

#define GB_PushNodeArgs(nd,hdr,pfr,pto)	/* push args of `fun + args' node fields */ 				\
								pfr = Cast(GB_Ptr,&((nd)->fields[GB_NH_NrFlds(hdr)])) ;	\
								pto = Cast(GB_Ptr,&((nd)->fields[1])) ;								\
								IF_GB_TR_ON(3,printf("pfr %x pto %x sp %x\n",pfr,pto,sp);) ;			\
								MemCopyBackward(pfr,pto,sp) ;

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
								      GB_PCImmIn(int16_t,x) ; break ; \
								    case GB_InsOp_ImmSz_64 : \
								      GB_PCImmIn(int16_t,x) ; break ; \
								  } \
								}
%%]
#define GB_PCImm(ty)			(*(((ty*)pc)++))
#define GB_PCExt				GB_PCImm(GB_Byte)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal chunks of bytecode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static GB_Byte gb_code_AfterEvalCall[] =
  { GB_Ins_EvalUpdCont
  } ;

static GB_Byte gb_code_AfterEvalApplyFunCall[] =
  { GB_Ins_EvalApplyCont
  } ;

static GB_Byte gb_code_AfterCallInApplyWithTooManyArgs[] =
  { GB_Ins_PApplyCont
  } ;

GB_Byte gb_code_Eval[] =
  { GB_Ins_Eval(GB_InsOp_LocB_TOS)
  , GB_Ins_Ext, GB_InsExt_Halt
  } ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

See gbprim

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
GB_Node* gb_MkCAF( GB_BytePtr pc )
{
  GB_NodeHeader h = GB_MkCAFHeader ;
  GB_Node* n = Cast(GB_Node*,GB_HeapAlloc_Words(2)) ;
  n->header = h ;
  n->fields[0] = Cast(GB_Word,pc) ;
  return n ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node, List specific interpretation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
GB_NodePtr gb_listTail( GB_NodePtr n )
{
	return GB_List_Tail(n) ;
}

GB_Word gb_listHead( GB_NodePtr n )
{
	return GB_List_Head(n) ;
}

Bool gb_listNull( GB_NodePtr n )
{
	return GB_List_IsNull(n) ;
}

void gb_listForceEval( GB_NodePtr n, int sz )
{
	while ( sz-- && ! GB_List_IsNull( n = Cast(GB_NodePtr,gb_eval( Cast(GB_Word,n) )) ) )
	{
  		IF_GB_TR_ON(3,printf("gb_listForceEval1 sz %d, n %x\n", sz, n ););
		n = GB_List_Tail(n) ;
  		IF_GB_TR_ON(3,printf("gb_listForceEval2 n %x\n", n ););
	}
}
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
	gb_interpretLoopWith( gb_code_Eval ) ;
	GB_PopIn( x ) ;
	return x ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
int gb_Opt_TraceSteps = True ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if GB_COUNT_STEPS
unsigned int gb_StepCounter ;
#endif

#if TRACE || DUMP_INTERNALS
extern char* gb_lookupMnem( GB_Byte c ) ;

void gb_prWord( GB_Word x )
{
	GB_Node* n = Cast(GB_Node*,x) ;
#if USE_64_BITS
	printf( "Wd 0x%0.16lx: "
#else
	printf( "Wd 0x%0.8x: "
#endif
	      , x ) ;
	if ( ( GB_Word_IsInt(x)
#if USE_BOEHM_GC
	       || x < Cast(GB_Word,StackEnd)
#else
	       || x < Cast(GB_Word,Heap)
#endif
         )
         && n != &gb_Nil
	)
	{
#if USE_64_BITS
		printf( "int %ld"
#else
		printf( "int %d"
#endif
		      , GB_GBInt2Int(x) ) ;
	} else {
		printf( "sz %d, ev %d, cat %d, tg %d:"
		      , GB_NH_Fld_Size(n->header), GB_NH_Fld_NdEv(n->header), GB_NH_Fld_TagCat(n->header), GB_NH_Fld_Tag(n->header) ) ;
		int i ;
		for ( i = 0 ; i < 5 && i < GB_Node_NrFlds(n) ; i++ )
		{
#if USE_64_BITS
			printf( " 0x%0.16lx"
#else
			printf( " 0x%0.8x"
#endif
			      , n->fields[i] ) ;
		}
	}
	/* printf( "\n" ) ; */
}

void gb_prStack( int maxStkSz )
{
    int i ;
    
	for ( i = 0 ; i < maxStkSz && sp+i < Cast(GB_Ptr,StackEnd) ; i++ )
	{
#if USE_64_BITS
		printf( "  %lx: "
#else
		printf( "  %x: "
#endif
		      , sp+i) ;
		gb_prWord( sp[i] ) ;
		printf( "\n" ) ;
	}
}

void gb_prState( char* msg, int maxStkSz )
{
	int i ;
	printf( "--------------------------------- %s ---------------------------------\n", msg ) ;
#	if USE_64_BITS
		printf( "[%d]PC 0x%lx: 0x%0.2x '%s'"
#	else
		printf( "[%d]PC 0x%x: 0x%0.2x '%s'"
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
		printf( ", SP 0x%lx: 0x%0.16lx, RR 0x%lx"
#	else
		printf( ", SP 0x%x: 0x%0.8x, RR 0x%x"
#	endif
	      , sp, *sp, rr ) ;
	printf( "\n" ) ;
	gb_prStack( maxStkSz ) ;
}

#endif
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
  	register GB_Word x, x2, x3 ;
  	register GB_Ptr  p, p2, p3, spSave ;
  	register GB_NodePtr n ;
  	register GB_NodeHeader h ;

	/* scratch */
  	GB_Word x4, x5, x6, retSave ;
  	GB_Ptr  p4, p5, p6 ;
  	GB_BytePtr  dst ;

	while( 1 )
	{
		IF_GB_TR_ON(1,gb_prState( "interpreter step", 10 ) ;)
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
				GB_Push( GB_RegByteRelx( rr, x ) ) ;
				break ;

			/* l1tr16 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_16) :
				GB_PCImmIn(int16_t,x) ;
				GB_Push( GB_RegByteRelx( rr, x ) ) ;
				break ;

			/* l1tr32 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_32) :
				GB_PCImmIn(int32_t,x) ;
				GB_Push( GB_RegByteRelx( rr, x ) ) ;
				break ;

			/* l1tr64 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Reg, GB_InsOp_ImmSz_64) :
				GB_PCImmIn(int64_t,x) ;
				GB_Push( GB_RegByteRelx( rr, x ) ) ;
				break ;


			/* l2tr08 */
			/* l2tr16 */
			/* l2tr32 */
			/* l2tr64 */

			/* ldgt */
			case GB_Ins_Ldg(GB_InsOp_LocB_TOS) :
				GB_PCImmIn(GB_Word,x) ;
				GB_Push( x ) ; /* linked in value */
				break ;
			
			/* ldgr */
			case GB_Ins_Ldg(GB_InsOp_LocB_Reg) :
				GB_PCImmIn(GB_Word,rr) ;
				break ;
			
			/* calling, returning, case */
			/* callt */
			case GB_Ins_Call(GB_InsOp_LocB_TOS) :
				x = GB_TOS ;
gb_interpreter_InsCallEntry:
				GB_SetTOS( Cast(GB_Word,pc) ) ;
				pc = Cast(GB_BytePtr,x) ;
				break ;
			
			/* callr */

			/* tailcallt */
#define GB_RetTailCall_Code(getDst,jumpDst,getPrevRet,restorePrevRet)		/* share between tailcall & retcall */							\
				spSave = sp ;																												\
				GB_PCExtIn(x) ;																												\
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,4,5),x3) ; 			/* nArgMine  , in bytes				*/							\
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,2,3),x4) ; 			/* nArgSurr  , in bytes				*/							\
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x5) ; 			/* retOffSurr, in bytes 			*/							\
				IF_GB_TR_ON(3,printf( "nArgMine %d nArgSurr %d retOffSurr %d\n", x3, x4, x5 );) \
				getPrevRet ; 												/* ret address of current function 	*/							\
				getDst ; 													/* destination of call 				*/							\
				IF_GB_TR_ON(3,printf( "retSave %x dst %x\n", retSave, dst );) \
				p2 = GB_RegByteRel(GB_Word,spSave,x5+x4+sizeof(GB_Word)) ;	/* after last old arg is dst startpoint of backwards copy */	\
				p  = GB_SPByteRel(GB_Word,x3) ;								/* after last new arg is src startpoint of backwards copy */	\
				MemCopyBackward(p,sp,p2) ;									/* copy new args over old 			*/							\
				sp = GB_RegByteRel(GB_Word,spSave,x4+x5-x3) ;				/* sp points to return addr,		*/							\
				restorePrevRet ;											/* which is assigned here			*/							\
				jumpDst ;													/* jump to dst						*/

			case GB_Ins_TailCall(GB_InsOp_LocB_TOS) :
				GB_RetTailCall_Code
					( GB_PopCastedIn(GB_BytePtr,dst)
					, pc = dst
					, retSave = GB_Deref(GB_RegByteRel(GB_Word,spSave,x5))
					, GB_SetTOS( retSave )
					) ;
				break ;

			/* tailcallr */
			
			/* retcall */
			case GB_Ins_RetCall :
				GB_RetTailCall_Code
					( dst = Cast(GB_BytePtr,GB_RegByteRelx(spSave,x5))
					, pc = dst
					, ;
					, GB_Pop
					) ;
				break ;

			/* casecall */
			case GB_Ins_CaseCall :
				/*
				GB_PCExtIn(x) ;
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x2) ;
				*/
				p = Cast(GB_Ptr,pc) ;										/* start of table after instr						*/
				n = Cast(GB_NodePtr,GB_TOS) ;								/* scrutinee is node 								*/
				dst = Cast(GB_BytePtr,p[GB_NH_Fld_Tag(n->header)]) ;		/* destination from following table, indexed by tag */
				pc = dst ;													/* jump 											*/
				break ;

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
						rts_panic1_1( "no call C for nr of args", nargs ) ;																														\
						break ;																																									\
				} \
				IF_GB_TR_ON(3,printf("GB_CallC_Code2 f %x res %x\n", f, res ););

			/* callc */
			case GB_Ins_CallC :
				GB_PCExtIn(x) ;
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x2) ; 			/* nr of args										*/
				GB_PopIn(x) ;												/* function											*/
				GB_CallC_Code(x,x2,sp,x) ;
				sp = GB_SPRel(x2) ;
				GB_Push(x) ;
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
			/* tailevalt */
			case GB_Ins_TailEval(GB_InsOp_LocB_TOS) :
				GB_PCExtIn(x) ;
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,2,3),x2) ; 			/* nArgSurr  , in bytes 			*/
				GB_PCImmIn2(Bits_ExtrFromToSh(GB_Byte,x,0,1),x3) ; 			/* retOffSurr, in bytes 			*/
				pc = Cast(GB_BytePtr,GB_RegByteRelx(sp,x3))	;				/* continuation address				*/
				x = GB_TOS ;
				sp = GB_SPByteRel(GB_Word,x3+x2) ;							/* sp points to eval arg			*/
				GB_SetTOS( x ) ;											/* which is set here				*/
																			/* so we can fall through to eval	*/

			/* evalt */
			case GB_Ins_Eval(GB_InsOp_LocB_TOS) :
				x = GB_TOS ;
gb_interpreter_InsEvalEntry:
				if ( GB_Word_IsPtr(x) ) {
					n = Cast(GB_NodePtr,x) ;
					h = n->header ;
					switch( GB_NH_Fld_NdEv(h) )
					{
						case GB_NodeNdEv_Yes :
							switch( GB_NH_Fld_TagCat(h) ) {
								case GB_NodeTagCat_Fun :
									GB_Push(pc) ;													/* save ret for after eval 			*/
									p = &(n->fields[1]) ;											/* 1st arg 							*/
									p2 = &(n->fields[GB_NH_NrFlds(h)]) ;							/* after last arg 					*/
									MemCopyBackward(p2,p,sp) ;										/* push args on stack 				*/
									pc = Cast(GB_BytePtr,n->fields[0]) ;							/* jump to function 				*/
									GB_Push(gb_code_AfterEvalCall) ;								/* ret addr is to update 			*/
									GB_NH_SetFld_NdEv(n->header,GB_NodeNdEv_BlH) ;					/* may not be eval'd when eval'd	*/
									break ;
								case GB_NodeTagCat_CFun :
									p = &(n->fields[1]) ;											/* 1st arg 							*/
									x2 = x ;                                                        /* remember val + pc 				*/
									retSave = Cast(GB_Word,pc) ;
									GB_CallC_Code(n->fields[0],GB_NH_NrFlds(h)-1,p,x) ;
									goto gb_interpreter_InsEvalUpdContEntry ;						/* update with result				*/
									break ;
								case GB_NodeTagCat_App :
									GB_Push(pc) ;													/* save ret for after eval 			*/
									GB_Push(x = n->fields[0]) ;										/* push function to eval 			*/
									pc = gb_code_AfterEvalApplyFunCall ;
									goto gb_interpreter_InsEvalEntry ;								/* evaluate							*/
									break ;
								case GB_NodeTagCat_Ind :
									GB_SetTOS( x = n->fields[0] ) ;									/* just follow the indirection		*/
									goto gb_interpreter_InsEvalEntry ;								/* evaluate							*/
									break ;
							}
							break ;
						case GB_NodeNdEv_BlH :
							rts_panic1_1( "black hole", x ) ;									/* black hole means panic			*/
							break ;
					}
				}
				break ;

			/* evalr */
			
			/* evupdcont */
			case GB_Ins_EvalUpdCont :
				GB_PopIn(x) ;													/* evaluation result 						*/
				GB_PopIn(retSave) ;												/* saved return address 					*/
				x2 = GB_TOS ;													/* node which was to be evaluated 			*/
gb_interpreter_InsEvalUpdContEntry:
				IF_GB_TR_ON(3,printf("%x isInt %d isPtr %d\n",x,GB_Word_IsInt(x),GB_Word_IsPtr(x));) ;
				register GB_NodePtr nOld = Cast(GB_NodePtr,x2) ;
				if (  GB_Word_IsPtr(x)
				   && (GB_NH_Fld_Size((n = Cast(GB_NodePtr,x))->header) <= GB_NH_Fld_Size(nOld->header))
				   )
				{
					p = Cast(GB_Ptr,&(n->fields[GB_Node_NrFlds(n)])) ;			/* overwrite content of old with new 		*/
					p2 = Cast(GB_Ptr,nOld) ;
					p3 = Cast(GB_Ptr,n) ;
					MemCopyForward(p3,p,p2) ;	
					GB_SetTOS( x ) ;											/* return new (avoids indirection)			*/
				} else {
					h = nOld->header ;											/* turn into indirection node				*/
					h = GB_MkHeader(GB_NH_Fld_Size(h),GB_NodeNdEv_Yes,GB_NodeTagCat_Ind,0) ;
					nOld->header = h ;
					nOld->fields[0] = x ;										/* assumption !!: memory is available !! 	*/
					GB_SetTOS( x ) ;
				}
				pc = Cast(GB_BytePtr,retSave) ;									/* jump to saved ret address				*/
				break ;

			/* evappcont */ /* under construction 20061122 */
			case GB_Ins_EvalApplyCont :
				GB_PopIn(x) ;													/* evaluated function						*/
				GB_PopIn(retSave) ;												/* saved return address 					*/
				GB_PopCastedIn(GB_NodePtr,n) ;									/* the apply node							*/
				h = n->header ;
				GB_PushNodeArgs(n,h,p,p2) ;										/* copy arguments from app					*/
				GB_Push(GB_NH_Fld_Size(h)-2) ;									/* nr of args								*/
				GB_Push(x) ;													/* function									*/
				pc = Cast(GB_BytePtr,retSave) ;									/* continue with apply						*/
				goto gb_interpreter_InsApplyEntry ;
				break ;

			/* papplycont */ /* under construction 20061122 */
			case GB_Ins_PApplyCont :
				GB_PopIn(x) ;													/* result value								*/
				pc = Cast(GB_BytePtr,GB_TOS) ;									/* continuation								*/
				GB_SetTOS( x ) ;												/* result is slided down					*/
																				/* fall through								*/

			/* applyt */ /* under construction 20061122 */
			case GB_Ins_Apply(GB_InsOp_LocB_TOS) :
gb_interpreter_InsApplyEntry:
				n = Cast(GB_NodePtr,GB_TOS) ;										/* function										*/
				x = GB_SPRelx(1) ;													/* nArgs, in words 								*/
				h = n->header ;
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
							pc = Cast(GB_BytePtr,n->fields[0]) ;								/* call function									*/
							GB_Push( Cast(GB_Word,gb_code_AfterCallInApplyWithTooManyArgs) ) ;  /* with continuation set to another apply			*/
						} else if ( nLeftOver == 0 ) {
							sp = Cast(GB_Ptr,GB_SPRel(2)) ;								/* remove node+size from stack 								*/
							GB_PushNodeArgs(n,h,p,p2) ;									/* copy arguments from partial app							*/
							x = n->fields[0] ;											/* call function											*/
							GB_Push(x) ;
							goto gb_interpreter_InsCallEntry ;
						} else { /* ( nLeftOver < 0 ) */
							p2 = n->fields ;											/* copy old fields prep										*/
							p3 = Cast(GB_Ptr,&((n)->fields[GB_NH_NrFlds(h)])) ;
							h = GB_MkHeader(GB_NH_Fld_Size(h)+x,GB_NH_Fld_NdEv(h),GB_NH_Fld_TagCat(h),GB_NH_Fld_Tag(h)-x) ;
							p = GB_HeapAlloc_Words( GB_NH_Fld_Size(h) ) ;				/* fresh node												*/
							x2 = Cast(GB_Word,p) ;										/* remember, to push later on								*/
							Cast(GB_NodePtr,p)->header = h ;							/* set header												*/
							p++ ;
							MemCopyForward(p2,p3,p) ;									/* copy old fields								*/
							GB_Popn(2) ;
							p3 = GB_SPRel(x) ;											/* copy new args								*/
							MemCopyForward(sp,p3,p) ;
							GB_Push(x2) ;
						}
						break ;

					default :
						rts_panic1_1( "non partial apply applied", Cast(GB_Word,n) ) ;
						break ;
				}
				break ;

			/* applyr */
			
			/* heap allocation (+storing), heap retrieving (fetching) */
			/* allocstoret */
			case GB_Ins_AllocStore(GB_InsOp_LocB_TOS) :
				GB_PopIn(x) ;
				p = GB_HeapAlloc_Bytes(x) ;
				IF_GB_TR_ON(3,printf( "alloc %x\n", p );) \
				p2 = p ;
				p3 = GB_SPByteRel(GB_Word,x) ;
				MemCopyForward(sp,p3,p) ;
				GB_Push(p2) ;
				break ;

			/* allocstorer */

			/* fetcht */
			case GB_Ins_Fetch(GB_InsOp_LocB_TOS) :
				GB_PopCastedIn(GB_NodePtr,n) ;
				p = Cast(GB_Ptr,&(n->fields[GB_Node_NrFlds(n)])) ;
				p2 = n->fields ;
				MemCopyBackward(p,p2,sp) ;
				break ;

			/* fetchr */

			/* fetchupd */
			case GB_Ins_FetchUpdate :
				GB_PopIn(x) ;
				GB_PopCastedIn(GB_NodePtr,n) ;
				n->fields[0] = x ;
				h = n->header ;
				h = GB_MkHeader(GB_NH_Fld_Size(h),GB_NodeNdEv_Yes,GB_NodeTagCat_Ind,GB_NH_Fld_Tag(h)) ;
				n->header = h ;
				break ;

			/* nop */
			case GB_Ins_NOP :
				break ;

			/* extended instructions */
			case GB_Ins_Ext :
				switch ( *(pc++) )
				{
					case GB_InsExt_Halt:
						goto interpretIsDone ;
						break ;

					default :
						rts_panic1_1( "extended instruction not implemented", *(pc-1) ) ;
						break ;
				}

				break ;

			/* operators */
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

			/* int add/sub/mul/div to TOS from XXX */
			case GB_Ins_Op(GB_InsOp_TyOp_Add,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS) :
				switch( *(pc++) )
				{
					GB_Op_TOSDst_Case_Code( GB_Int_Add )

					default:
						rts_panic1_1( "oaiwt<XXX> instruction not implemented", *(pc-1) ) ;
						break ;
				}
				break ;

			case GB_Ins_Op(GB_InsOp_TyOp_Sub,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS) :
				switch( *(pc++) )
				{
					GB_Op_TOSDst_Case_Code( GB_Int_Sub )

					default:
						rts_panic1_1( "osiwt<XXX> instruction not implemented", *(pc-1) ) ;
						break ;
				}
				break ;

			case GB_Ins_Op(GB_InsOp_TyOp_Mul,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS) :
				switch( *(pc++) )
				{
					GB_Op_TOSDst_Case_Code( GB_Int_Mul )

					default:
						rts_panic1_1( "omiwt<XXX> instruction not implemented", *(pc-1) ) ;
						break ;
				}
				break ;

			case GB_Ins_Op(GB_InsOp_TyOp_Div,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS) :
				switch( *(pc++) )
				{
					GB_Op_TOSDst_Case_Code( GB_Int_Div )

					default:
						rts_panic1_1( "odiwt<XXX> instruction not implemented", *(pc-1) ) ;
						break ;
				}
				break ;

			default:
				rts_panic1_1( "instruction not implemented", *(pc-1) ) ;
				break ;

		}
		
#		if GB_COUNT_STEPS
		gb_StepCounter++ ;
#		endif

	}
	
	interpretIsDone: ;

}

%%]			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void gb_Initialize()
{
	sp = Cast(GB_Ptr,StackEnd) ;
}

%%]

%%[8
void gb_InitTables
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
	)
{
	int i ;
	GB_Ptr p ;

	for ( i = 0 ; i < cafEntriesSz ; i++ )
	{
		*(cafEntries[i]) = Cast(GB_BytePtr,gb_MkCAF( *(cafEntries[i]) )) ;
	}
	
%%[[12
	for ( i = 0 ; i < GB_Node_NrFlds(impNode) ; i++ )
	{
		GB_ModEntry* mod = gb_lookupModEntry( Cast(char*,impNode->fields[i]), modTbl ) ;
		impNode->fields[i] = Cast(GB_Word,mod->expNode) ;
	}
%%]]

	for ( i = 0 ; i < linkEntriesSz ; i++ )
	{
		p = Cast(GB_Ptr,linkEntries[i].infoLoc) ;
		switch ( linkEntries[i].tblKind )
		{
			case GB_LinkTbl_EntryKind_Const :
				*p = consts[ linkEntries[i].inx ] ;
				break ;

			case GB_LinkTbl_EntryKind_ConstPtr :
				*p = GB_Deref(Cast(GB_Ptr,consts[ linkEntries[i].inx ])) ;
				break ;

			case GB_LinkTbl_EntryKind_CodeEntry :
				*p = Cast(GB_Word,globalEntries[ linkEntries[i].inx ]) ;
				break ;

%%[[12
			case GB_LinkTbl_EntryKind_ImpEntry :
				*p = impNode->fields[ linkEntries[i].inxMod ] ;
				break ;
%%]]
		}
	}
	
	for ( i = 0 ; i < fixOffsetsSz ; i++ )
	{
		int j ;
		p = fixOffsets[i].codeLoc ;
		for ( j = 0 ; j < fixOffsets[i].nrOfLocs ; j++ )
		{
			p[j] = Cast(GB_Word,&p[j+1]) + p[j] ;
		}
	}
	
%%[[12
	for ( i = 0 ; i < GB_Node_NrFlds(expNode) ; i++ )
	{
		expNode->fields[i] = Cast(GB_Word,globalEntries[ expNode->fields[i] ]) ;
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
	
	if ( sizeof( GB_Word ) != sizeof( GB_Ptr ) ) {
		rts_panic2_1( m, "size of word and pointer must be equal", 0 ) ;
	}
	
	if ( sizeof(GrWord) != sizeof(GB_Word) ) {
		rts_panic2_1( m, "size of GrWord and GB_Word must be equal", 0 ) ;
	}
	
	if ( sizeof( GB_Word ) != sizeof(uint32_t) && sizeof( GB_Word ) != sizeof(uint64_t) ) {
		rts_panic2_1( m, "size of word and pointer must be 32 or 64", 0 ) ;
	}
	
	x1 = Cast(GB_Word,GB_HeapAlloc_Words( 2 )) ;
	if ( ! x1 ) {
		rts_panic2_1( m, "heap allocation yields zero pointer", x1 ) ;
	}
	if ( x1 & GB_Word_TagMask ) {
		rts_panic2_2( m, "heap allocated pointers must have lower bits set to zero", GB_Word_TagSize, x1 ) ;
	}
	
	x1 = Cast(GB_Word,&gb_False) ;
	if ( x1 & GB_Word_TagMask ) {
		rts_panic2_2( m, "statically allocated nodes must must be word aligned", GB_Word_TagSize, x1 ) ;
	}
	
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
GB_ModEntry* gb_lookupModEntry( char* modNm, GB_ModEntry* modTbl )
{
	for ( ; modTbl->name != NULL && strcmp( modTbl->name, modNm ) != 0 ; modTbl++ ) ;
	if ( modTbl == NULL )
		rts_panic2( "module lookup", modNm ) ;
	return modTbl ;
}

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
, { GB_Ins_Op(GB_InsOp_TyOp_Div,GB_InsOp_DataOp_IW,GB_InsOp_LocODst_TOS)
  , "odiwt XXX"
  }
, { GB_Ins_Ldg(GB_InsOp_LocB_TOS)				, "ldgt" 			}
, { GB_Ins_Ldg(GB_InsOp_LocB_Reg)				, "ldgr" 			}
, { GB_Ins_Call(GB_InsOp_LocB_TOS)				, "callt" 			}
, { GB_Ins_TailCall(GB_InsOp_LocB_TOS)			, "tailcallt" 		}
, { GB_Ins_AllocStore(GB_InsOp_LocB_TOS)		, "allocstoret" 	}
, { GB_Ins_Fetch(GB_InsOp_LocB_TOS)				, "fetcht" 			}
, { GB_Ins_RetCall								, "retcall" 		}
, { GB_Ins_RetCase								, "retcase" 		}
, { GB_Ins_CaseCall								, "casecall" 		}
, { GB_Ins_CallC								, "callc"	 		}
, { GB_Ins_Eval(GB_InsOp_LocB_TOS)				, "evalt" 			}
, { GB_Ins_TailEval(GB_InsOp_LocB_TOS)			, "tailevalt" 		}
, { GB_Ins_EvalUpdCont							, "evupdcont" 		}
, { GB_Ins_Apply(GB_InsOp_LocB_TOS)				, "applyt" 			}
, { GB_Ins_Ext									, "ext" 			}
, { GB_Ins_EvalApplyCont						, "evappcont" 		}
, { GB_Ins_FetchUpdate							, "fetchupd" 		}
, { GB_Ins_PApplyCont							, "papplycont" 		}
, { GB_Ins_NOP									, "nop" 			}
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
