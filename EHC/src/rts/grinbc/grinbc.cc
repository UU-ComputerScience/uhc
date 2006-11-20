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
/* #define AC_REG asm("25") */
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

#ifdef /* DEBUG && */ AC_REG
#undef AC_REG
#endif

%%]

%%[8
register GB_BytePtr  pc PC_REG;
register GB_Ptr      sp SP_REG;

#ifdef AC_REG
register GB_Word   accu AC_REG;
#endif

%%]

%%[8
#define GB_RegRel(r,o)			((r)+(o))
#define GB_PCRel(o)				GB_RegRel(pc,o)

#define GB_RegByteRel(ty,r,o)	Cast(ty*,((Cast(GB_BytePtr,r))+(o)))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef AC_REG

#define GB_Push(v)				{*(--sp) = (accu) ; accu = ((GB_Word)(v)) ;}
#define GB_TOS					(accu)
#define GB_Pop					(accu = *(++sp))
#define GB_PopIn(v)				{(v) = accu ; GB_Pop ;}

#else

#define GB_Push(v)				{*(--sp) = ((GB_Word)(v)) ;}
#define GB_TOS					(*sp)
#define GB_Pop					(sp++)
#define GB_PopIn(v)				{(v) = *GB_Pop ;}

#define GB_PopnUpdTOS(n,x)		{sp += (n) ; GB_TOS = (x) ; }

#endif

#define GB_SPRel(o)				GB_RegRel(sp,o)
#define GB_SPRelx(o)			(*GB_SPRel(o))

#define GB_SPByteRel(ty,o)		GB_RegByteRel(ty,sp,o)
#define GB_SPByteRelx(ty,o)		(*GB_SPByteRel(ty,o))

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
static GB_Byte intBC_AfterEvalCall[] =
  { GB_Ins_Upd
  } ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interpreter loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void interpretLoop()
{
	/* scratch registers */
  	register GB_Word x, x2, x3 ;
  	register GB_Ptr  p, p2, p3 ;

	/* scratch */
  	GB_Word x4, x5, x6 ;
  	GB_Ptr  p4, p5, p6 ;
	
	while( 1 )
	{
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
				GB_Push( GB_SPByteRelx(GB_Word,GB_PCImm(int8_t)) ) ;
				break ;

			/* l1tt16 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_16) :
				GB_Push( GB_SPByteRelx(GB_Word,GB_PCImm(int16_t)) ) ;
				break ;

			/* l1tt32 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_32) :
				GB_Push( GB_SPByteRelx(GB_Word,GB_PCImm(int32_t)) ) ;
				break ;

			/* l1tt64 */
			case GB_Ins_Ld(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_64) :
				GB_Push( GB_SPByteRelx(GB_Word,GB_PCImm(int64_t)) ) ;
				break ;

			/* l2tt08 */
			/* l2tt16 */
			/* l2tt32 */
			/* l2tt64 */
			
			/* ldg */
			case GB_Ins_Ldg :
				GB_Push( GB_PCImm(GB_Word) ) ; /* linked in value */
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
			case GB_Ins_TailCall(GB_InsOp_LocB_TOS) :
				x = GB_PCExt ;
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,4,5),x3) ; 	/* nArgMine  , in bytes				*/
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,2,3),x4) ; 	/* nArgSurr  , in bytes				*/
				GB_PCImmIn(Bits_ExtrFromToSh(GB_Byte,x,0,1),x5) ; 	/* retOffSurr, in bytes 			*/
				x6 = GB_SPByteRelx(GB_Word,x5) ; 					/* ret address of current function 	*/
				GB_PopIn(x) ; 										/* destination of call 				*/
				p  = GB_SPByteRel(GB_Word,x3) ;						/* after last new arg is src startpoint of backwards copy */
				p2 = GB_SPByteRel(GB_Word,x5+x4+sizeof(GB_Word)) ;	/* after last old arg is dst startpoint of backwards copy */
				MemCopyBackward(GB_Word,p,sp,p2) ;					/* copy new args over old 			*/
				sp = GB_SPByteRel(GB_Word,x4+x5-x3) ;				/* sp points to return addr,		*/
				GB_TOS = x6 ;										/* which is assigned here			*/
				pc = Cast(GB_BytePtr,x) ;							/* jump to dst						*/
				break ;
			
			/* tailcallr */
			
			/* eval/apply */
			/* evalt */
			case GB_Ins_Eval(GB_InsOp_LocB_TOS) :
				x = GB_TOS ;
				if ( GB_Word_IsPtr(x) ) {
					register GB_NodePtr n = Cast(GB_NodePtr,x) ;
					register GB_NodeHeader h = n->header ;
					if ( h.needsEval ) {
						switch( h.tagCateg ) {
							case GB_NodeTagCat_Fun :
								GP_Push(pc) ;									/* save ret for after eval 			*/
								p = Cast(GB_Ptr,&(n->fields[1])) ;				/* 1st arg 							*/
								p2 = Cast(GB_Ptr,&(n->fields[h.size-1])) ;		/* after last arg 					*/
								MemCopyBackward(GB_Word,p2,p,sp) ;				/* push args on stack 				*/
								pc = Cast(GB_BytePtr,n->fields[0]) ;			/* jump to function 				*/
								GP_Push(intBC_AfterEvalCall) ;					/* ret addr is to special handling	*/
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
				GB_PopIn(x6) ;													/* saved return address 					*/
				x2 = GB_TOS ;													/* node which was to be evaluated 			*/
				register GB_NodePtr nOld = Cast(GB_NodePtr,x2) ;
				register GB_NodePtr nNew ;
				if ( GB_Word_IsPtr(x) && ((nNew = Cast(GB_NodePtr,x))->header.size <= nOld->header.size) ) {
					p = Cast(GB_Ptr,&(nNew->fields[nNew->header.size])) ;		/* overwrite content of old with new 		*/
					MemCopyForward(GB_Word,nNew,p,nOld) ;	
				} else {
					nOld->header.tagCateg = GB_NodeTagCat_Ind ;					/* needsEval flag stays on, size unchanged 	*/
					nOld->fields[0] = x ;										/* assumption !!: memory is available !! 	*/
					GB_TOS = x ;
				}
				GB_TOS = x ;													/* update with new (??)						*/
				pc = Cast(GB_BytePtr,x6) ;										/* jump to saved ret address				*/
				break ;

			/* applyt */
			case GB_Ins_Apply(GB_InsOp_LocB_TOS) :
				break ;

			/* applyr */

			/* heap allocation, fetching */
			/* allocstoret */
			case GB_Ins_AllocStore(GB_InsOp_LocB_TOS) :
				GB_PopIn(x) ;
				p = GB_HeapAlloc(x) ;
				p2 = p ;
				p3 = GB_SPRel(x) ;
				MemCopyForward(GB_Word,sp,p3,p) ;
				GB_Push(p3) ;
				break ;

			/* allocstorer */

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
void gb_InitTbls
	( int linkEntriesSz
	, GB_LinkEntry* linkEntries
	, GB_Word* consts
	)
{
	int i ;
	for ( i = 0 ; i < linkEntriesSz ; i++ )
	{
		*(linkEntries[i].codeLoc) = consts[ linkEntries[i].inx ] ;
	}
}
%%]
