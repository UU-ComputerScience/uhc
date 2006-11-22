%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Register usage (adapted from lvm evaluator.c)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
/* register optimization:
   this makes a big difference, nfib is 1.5 times faster on a pentium
*/
#if defined(__GNUC__) && !defined(DEBUG)
#ifdef __i386__
# define PC_REG asm("%esi")
# define SP_REG asm("%edi")
# define FP_REG
#endif
#ifdef __x86_64__
#define PC_REG asm("%rsi")
#define SP_REG asm("%rdi")
#define FP_REG
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
register GB_BytePtr  pc PC_REG;
register GB_Ptr      sp SP_REG;

static GB_BytePtr gb_initPC ;

%%]

%%[8
#define GB_RegRel(r,o)			((r)+(o))
#define GB_PCRel(o)				GB_RegRel(pc,o)

#define GB_RegByteRel(ty,r,o)	Cast(ty*,((Cast(GB_BytePtr,r))+(o)))
#define GB_RegByteRelx(r,o)		GB_Deref(GB_RegByteRel(GB_Word,r,o))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Push(v)				{*(--sp) = ((GB_Word)(v)) ; }
#define GB_Push2(v)				{*(sp-1) = ((GB_Word)(v)) ; sp-- ;}		/* avoid predecrement */
#define GB_TOS					(*sp)
#define GB_Pop					(sp++)
#define GB_PopCastedIn(ty,v)	{(v) = Cast(ty,*GB_Pop) ;}
#define GB_PopIn(v)				GB_PopCastedIn(GB_Word,v)

#define GB_PopnUpdTOS(n,x)		{sp += (n) ; GB_TOS = (x) ; }

#define GB_SPRel(o)				GB_RegRel(sp,o)
#define GB_SPRelx(o)			GB_Deref(GB_SPRel(o))

#define GB_SPByteRel(ty,o)		GB_RegByteRel(ty,sp,o)
#define GB_SPByteRelx(o)		GB_Deref(GB_SPByteRel(GB_Word,o))

void gb_push( GB_Word x )
{
	GB_Push( x ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Immediate inlined constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_PCImm(ty)			(*(((ty*)pc)++))
#define GB_PCExt				GB_PCImm(GB_Byte)

#define GB_PCImmIn(sz,x)		{ switch (sz) { \
								    case GB_InsOp_ImmSz_08 : \
								      (x) = GB_PCImm(int8_t) ; break ; \
								    case GB_InsOp_ImmSz_16 : \
								      (x) = GB_PCImm(int16_t) ; break ; \
								    case GB_InsOp_ImmSz_32 : \
								      (x) = GB_PCImm(int32_t) ; break ; \
								    case GB_InsOp_ImmSz_64 : \
								      (x) = GB_PCImm(int64_t) ; break ; \
								  } \
								}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal chunks of bytecode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static GB_Byte gb_code_AfterEvalCall[] =
  { GB_Ins_Upd
  } ;

GB_Byte gb_code_Startup[] =
  { GB_Ins_Eval(GB_InsOp_LocB_TOS)
  , GB_Ins_Ext, GB_InsExt_Halt
  } ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
GB_Node* gb_MkCAF( GB_BytePtr pc )
{
  GB_NodeHeader h = {2, 1, GB_NodeTagCat_Fun, 0} ;
  GB_Node* n = Cast(GB_Node*,GB_HeapAlloc(2 * sizeof(GB_Word))) ;
  n->header = h ;
  n->fields[0] = Cast(GB_Word,pc) ;
  return n ;
}
%%]
GB_Node* gb_MkCAF( GB_BytePtr pc )
{
  GB_NodeHeader h = {2, 1, GB_NodeTagCat_Fun, 0} ;
  GB_Node* n = Cast(GB_Node*,GB_HeapAlloc(2 * sizeof(GB_Word))) ;
  n->header = h ;
  n->fields[0] = Cast(GB_Word,pc) ;
  return n ;
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if TRACE || DUMP_INTERNALS
extern char* gb_getMnem( GB_Byte c ) ;

void gb_prWord( GB_Word x )
{
	GB_Node* n = Cast(GB_Node*,x) ;
	printf( "Wd 0x%x: ", x ) ;
	if ( GB_Word_IsInt(x) || x < Cast(GB_Word,Heap) ) {
		printf( "int %d", GB_FromInt(int,x) ) ;
	} else {
		printf( "sz %d, ev %d, cat %d, tg %d:", n->header.size, n->header.needsEval, n->header.tagCateg, n->header.tag ) ;
		int i ;
		for ( i = 0 ; i < 5 && i < n->header.size-1 ; i++ )
		{
			printf( " 0x%x", n->fields[i] ) ;
		}
	}
	/* printf( "\n" ) ; */
}

void gb_prStack( int maxStkSz )
{
    int i ;
    
	for ( i = 0 ; i < maxStkSz && sp+i < Cast(GB_Ptr,StackEnd) ; i++ )
	{
		printf( "  %x: ", sp+i) ;
		gb_prWord( sp[i] ) ;
		printf( "\n" ) ;
	}
}

void gb_prState( char* msg, int maxStkSz )
{
	printf( "--------------------------------- %s ---------------------------------\n", msg ) ;
	printf( "PC 0x%x %d: 0x%0.2x '%s' 0x%0.8x, SP 0x%x: 0x%0.8x", pc, pc-gb_initPC, *pc, gb_getMnem(*pc), *Cast(uint32_t*,pc+1), sp, *sp ) ;
	printf( "\n" ) ;
	gb_prStack( maxStkSz ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interpreter loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void interpretLoopWith( GB_BytePtr initPC )
{
	pc = initPC ;
	gb_initPC = pc ;
	interpretLoop() ;
}

void interpretLoop()
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
		IF_TR_ON(1,gb_prState( "interpreter step", 10 ) ;)
		switch( *(pc++) )
		{
			/* load immediate constant on stack */
			/* l0ti08 */
			case GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_08) :
				GB_Push( GB_PCImm(int8_t) ) ;
				break ;

			/* l0ti16 */
			case GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_16) :
				GB_Push( GB_PCImm(int16_t) ) ;
				break ;

			/* l0ti32 */
			case GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_32) :
				GB_Push( GB_PCImm(int32_t) ) ;
				break ;

			/* l0ti64 */
			case GB_Ins_Ld(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_64) :
				GB_Push( GB_PCImm(int64_t) ) ;
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
				GB_Push( GB_ToInt( GB_PCImm(int8_t) ) ) ;
				break ;

			/* liti16 */
			case GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_16) :
				GB_Push( GB_ToInt( GB_PCImm(int16_t) ) ) ;
				break ;

			/* liti32 */
			case GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_32) :
				GB_Push( GB_ToInt( GB_PCImm(int32_t) ) ) ;
				break ;

			/* liti64 */
			case GB_Ins_Ld(GB_InsOp_DerefInt, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_64) :
				GB_Push( GB_ToInt( GB_PCImm(int64_t) ) ) ;
				break ;

			/* l0tt08 */
			/* l0tt16 */
			/* l0tt32 */
			/* l0tt64 */

			/* load TOS relative content on stack */
			/* l1tt08 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_08) :
				GB_Push2( GB_SPByteRelx(GB_PCImm(int8_t)) ) ;
				break ;

			/* l1tt16 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_16) :
				GB_Push2( GB_SPByteRelx(GB_PCImm(int16_t)) ) ;
				break ;

			/* l1tt32 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_32) :
				GB_Push2( GB_SPByteRelx(GB_PCImm(int32_t)) ) ;
				break ;

			/* l1tt64 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_64) :
				GB_Push2( GB_SPByteRelx(GB_PCImm(int64_t)) ) ;
				break ;

			/* l2tt08 */
			/* l2tt16 */
			/* l2tt32 */
			/* l2tt64 */
			
			/* ldg */
			case GB_Ins_Ldg :
				GB_Push( /*GB_Deref*/( GB_PCImm(GB_Word) ) ) ; /* linked in value */
				break ;
			
			/* calling, returning, case */
			/* callt */
			case GB_Ins_Call(GB_InsOp_LocB_TOS) :
				x = GB_TOS ;
				GB_TOS = Cast(GB_Word,pc) ;
				pc = Cast(GB_BytePtr,x) ;
				break ;
			
			/* callr */

			/* tailcallt */
#define GB_RetTailCallCode(getDst,jumpDst,getPrevRet,restorePrevRet)		/* share between tailcall & retcall */							\
				spSave = sp ;																												\
				x = GB_PCExt ;																												\
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,4,5),x3) ; 			/* nArgMine  , in bytes				*/							\
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,2,3),x4) ; 			/* nArgSurr  , in bytes				*/							\
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,0,1),x5) ; 			/* retOffSurr, in bytes 			*/							\
				IF_TR_ON(3,printf( "nArgMine %d nArgSurr %d retOffSurr %d\n", x3, x4, x5 );) \
				getPrevRet ; 												/* ret address of current function 	*/							\
				getDst ; 													/* destination of call 				*/							\
				IF_TR_ON(3,printf( "retSave %x dst %x\n", retSave, dst );) \
				p2 = GB_RegByteRel(GB_Word,spSave,x5+x4+sizeof(GB_Word)) ;	/* after last old arg is dst startpoint of backwards copy */	\
				p  = GB_SPByteRel(GB_Word,x3) ;								/* after last new arg is src startpoint of backwards copy */	\
				MemCopyBackward(p,sp,p2) ;									/* copy new args over old 			*/							\
				sp = GB_RegByteRel(GB_Word,spSave,x4+x5-x3) ;				/* sp points to return addr,		*/							\
				restorePrevRet ;											/* which is assigned here			*/							\
				jumpDst ;													/* jump to dst						*/

			case GB_Ins_TailCall(GB_InsOp_LocB_TOS) :
				GB_RetTailCallCode
					( GB_PopCastedIn(GB_BytePtr,dst)
					, pc = dst
					, retSave = GB_Deref(GB_RegByteRel(GB_Word,spSave,x5))
					, GB_TOS = retSave
					) ;
				break ;

			/* tailcallr */
			
			/* retcall */
			case GB_Ins_RetCall :
				GB_RetTailCallCode
					( dst = Cast(GB_BytePtr,GB_RegByteRelx(spSave,x5))
					, pc = dst
					, ;
					, GB_Pop
					) ;
				break ;

			/* casecall */ /* under construction 20061122 */
			case GB_Ins_CaseCall :
				x = GB_PCExt ;
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,0,1),x2) ; 			/* nr of following locations, in bytes 			*/
				p = Cast(GB_Ptr,x2) ;
				retSave = *(p++) ;											/* return address */
				n = Cast(GB_NodePtr,GB_TOS) ;								/* scrutinee is node */
				dst = Cast(GB_BytePtr,p[n->header.tag]) ;					/* destination from following table, indexed by tag */
				pc = dst ;													/* jump */
				break ;

			/* retcase */ /* under construction 20061122 */
			case GB_Ins_RetCase :
				x = GB_PCExt ;
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,2,3),x2) ; 			/* nRes		 , in bytes 			*/
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,0,1),x3) ; 			/* retOffSurr, in bytes 			*/
				dst = GB_PCImm(GB_BytePtr) ; 								/* destination 			*/
				p2 = GB_SPByteRel(GB_Word,x3) ;								/* after retOffSurr is dst startpoint of backwards copy */
				p  = GB_SPByteRel(GB_Word,x2) ;								/* after nRes is src startpoint of backwards copy */
				MemCopyBackward(p,sp,p2) ;									/* copy new args over old 			*/
				sp = GB_SPByteRel(GB_Word,x3-x2) ;							/* sp points to res		*/
				pc = dst ;													/* jump */
				break ;
						
			/* eval/apply */
			/* evalt */ /* under construction 20061122 */
			case GB_Ins_Eval(GB_InsOp_LocB_TOS) :
				x = GB_TOS ;
				if ( GB_Word_IsPtr(x) ) {
					n = Cast(GB_NodePtr,x) ;
					h = n->header ;
					if ( h.needsEval ) {
						switch( h.tagCateg ) {
							case GB_NodeTagCat_Fun :
								GB_Push(pc) ;									/* save ret for after eval 			*/
								p = Cast(GB_Ptr,&(n->fields[1])) ;				/* 1st arg 							*/
								p2 = Cast(GB_Ptr,&(n->fields[h.size-2])) ;		/* after last arg 					*/
								MemCopyBackward(p2,p,sp) ;						/* push args on stack 				*/
								pc = Cast(GB_BytePtr,n->fields[0]) ;			/* jump to function 				*/
								GB_Push(gb_code_AfterEvalCall) ;				/* ret addr is to special handling	*/
								break ;
							case GB_NodeTagCat_App :
							    
								break ;
							case GB_NodeTagCat_Ind :
							    GB_TOS = n->fields[0] ;							/* just follow the indirection		*/
								break ;
						}
					}
				}
				break ;

			/* evalr */
			
			/* update */
			case GB_Ins_Upd :
				GB_PopIn(x) ;													/* evaluation result 						*/
				GB_PopIn(retSave) ;												/* saved return address 					*/
				x2 = GB_TOS ;													/* node which was to be evaluated 			*/
				register GB_NodePtr nOld = Cast(GB_NodePtr,x2) ;
				if ( GB_Word_IsPtr(x) && ((n = Cast(GB_NodePtr,x))->header.size <= nOld->header.size) ) {
					p = Cast(GB_Ptr,&(n->fields[n->header.size-1])) ;		/* overwrite content of old with new 		*/
					p2 = Cast(GB_Ptr,nOld) ;
					p3 = Cast(GB_Ptr,n) ;
					MemCopyForward(p3,p,p2) ;	
				} else {
					nOld->header.tagCateg = GB_NodeTagCat_Ind ;					/* needsEval flag stays on, size unchanged 	*/
					nOld->fields[0] = x ;										/* assumption !!: memory is available !! 	*/
					GB_TOS = x ;
				}
				GB_TOS = x ;													/* update with new (??)						*/
				pc = Cast(GB_BytePtr,retSave) ;									/* jump to saved ret address				*/
				break ;

			/* applyt */ /* under construction 20061122 */
			case GB_Ins_Apply(GB_InsOp_LocB_TOS) :
				break ;

			/* applyr */

			/* heap allocation (+storing), heap retrieving (fetching) */
			/* allocstoret */
			case GB_Ins_AllocStore(GB_InsOp_LocB_TOS) :
				GB_PopIn(x) ;
				p = GB_HeapAlloc(x) ;
				p2 = p ;
				p3 = GB_SPByteRel(GB_Word,x) ;
				MemCopyForward(sp,p3,p) ;
				GB_Push(p2) ;
				break ;

			/* allocstorer */

			/* fetcht */
			case GB_Ins_Fetch(GB_InsOp_LocB_TOS) :
				GB_PopIn(x) ;
				n = Cast(GB_NodePtr,x) ;
				p = Cast(GB_Ptr,&(n->fields[n->header.size-1])) ;
				p2 = Cast(GB_Ptr,x) ;
				MemCopyBackward(p,p2,sp) ;
				break ;

			/* fetchr */

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
						panic( "extended instruction not implemented", *(pc-1) ) ;
						break ;
				}

				break ;

			default:
				panic( "instruction not implemented", *(pc-1) ) ;
				break ;

		}

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
	)
{
	int i ;
	GB_Ptr p ;

	for ( i = 0 ; i < cafEntriesSz ; i++ )
	{
		*(cafEntries[i]) = Cast(GB_BytePtr,gb_MkCAF( *(cafEntries[i]) )) ;
	}
	
	for ( i = 0 ; i < linkEntriesSz ; i++ )
	{
		p = Cast(GB_Word*,linkEntries[i].codeLoc) ;
		switch ( linkEntries[i].tblKind )
		{
			case GB_LinkTbl_EntryKind_Const :
				*p = consts[ linkEntries[i].inx ] ;
				break ;

			case GB_LinkTbl_EntryKind_CodeEntry :
				*p = Cast(GB_Word,globalEntries[ linkEntries[i].inx ]) ;
				break ;
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
	
}
%%]
		IF_TR_ON( 3, printf( "gb_InitTables caf1 %x\n", *(cafEntries[i]) ) )
		IF_TR_ON( 3, printf( "gb_InitTables caf2 %x\n", *(cafEntries[i]) ) )
				IF_TR_ON( 3, printf( "gb_InitTables const1 %x %x\n", linkEntries[i].codeLoc, *(Cast(GB_Word*,linkEntries[i].codeLoc)) ) )
				IF_TR_ON( 3, printf( "gb_InitTables const2 %x %x\n", linkEntries[i].codeLoc, *(Cast(GB_Word*,linkEntries[i].codeLoc)) ) )
				IF_TR_ON( 3, printf( "gb_InitTables code1 %x %x\n", linkEntries[i].codeLoc, *(Cast(GB_Word*,linkEntries[i].codeLoc)) ) )
				IF_TR_ON( 3, printf( "gb_InitTables code2 %x %x\n", linkEntries[i].codeLoc, *(Cast(GB_Word*,linkEntries[i].codeLoc)) ) )

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
		panic2( m, "size of word and pointer must be equal", 0 ) ;
	}
	
	if ( sizeof( GB_Word ) != sizeof(uint32_t) && sizeof( GB_Word ) != sizeof(uint64_t) ) {
		panic2( m, "size of word and pointer must be 32 or 64", 0 ) ;
	}
	
	x1 = Cast(GB_Word,GB_HeapAlloc( 8 )) ;
	if ( x1 & GB_Word_TagMask ) {
		panic2_2( m, "heap allocated pointers must have lower bits set to zero", GB_Word_TagSize, x1 ) ;
	}
	
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
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_08)
  , "l1tt08"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_16)
  , "l1tt16"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_32)
  , "l1tt32"
  }
, { GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_64)
  , "l1tt64"
  }
, { GB_Ins_Ldg									, "ldg" 			}
, { GB_Ins_Call(GB_InsOp_LocB_TOS)				, "callt" 			}
, { GB_Ins_TailCall(GB_InsOp_LocB_TOS)			, "tailcallt" 		}
, { GB_Ins_AllocStore(GB_InsOp_LocB_TOS)		, "allocstoret" 	}
, { GB_Ins_Fetch(GB_InsOp_LocB_TOS)				, "fetcht" 			}
, { GB_Ins_RetCall								, "retcall" 		}
, { GB_Ins_RetCase								, "retcase" 		}
, { GB_Ins_CaseCall								, "casecall" 		}
, { GB_Ins_Eval(GB_InsOp_LocB_TOS)				, "eval" 			}
, { GB_Ins_Upd									, "upd" 			}
, { GB_Ins_Apply(GB_InsOp_LocB_TOS)				, "applyt" 			}
, { GB_Ins_Ext									, "ext" 			}
, { GB_Ins_NOP									, "nop" 			}
, { 0											, "--" 				}	/* this must be the last one */
} ;

char* gb_getMnem( GB_Byte c )
{
	GB_Mnem *mn = gb_mnemTable ;
	for ( ; mn->code != 0 && mn->code != c ; mn++ ) ;
	return mn->mnem ;
}

#endif
%%]
