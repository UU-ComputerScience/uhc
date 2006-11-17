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

#define GB_SPRel(o)				GB_RegRel(sp,o)

#endif

#define GB_SPRel(o)				GB_RegRel(sp,o)
#define GB_SPRelx(o)			(*GB_SPRel(o))

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Immediate inlined constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_PCImm(ty)			(*(((ty*)pc)++))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interpreter loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void interpretLoop()
{
  	register GB_Word x ;
	
	while( 1 )
	{
		switch( *(pc++) )
		{
			/* load immediate constant on stack */
			/* l0ti08 */
			case GB_InsLd(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_08) :
				GB_Push( GB_PCImm(int8_t) ) ;
				break ;

			/* l0ti16 */
			case GB_InsLd(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_16) :
				GB_Push( GB_PCImm(int16_t) ) ;
				break ;

			/* l0ti32 */
			case GB_InsLd(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_32) :
				GB_Push( GB_PCImm(int32_t) ) ;
				break ;

			/* l0ti64 */
			case GB_InsLd(GB_InsOp_Deref0, GB_InsOp_LocB_TOS, GB_InsOp_LocE_Imm, GB_InsOp_ImmSz_64) :
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

			/* l0tt08 */
			/* l0tt16 */
			/* l0tt32 */
			/* l0tt64 */

			/* load TOS relative content on stack */
			/* l1tt08 */
			case GB_InsLd(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_08) :
				GB_Push( GB_SPRelx(GB_PCImm(int8_t)) ) ;
				break ;

			/* l1tt16 */
			case GB_InsLd(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_16) :
				GB_Push( GB_SPRelx(GB_PCImm(int16_t)) ) ;
				break ;

			/* l1tt32 */
			case GB_InsLd(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_32) :
				GB_Push( GB_SPRelx(GB_PCImm(int32_t)) ) ;
				break ;

			/* l1tt64 */
			case GB_InsLd(GB_InsOp_Deref1, GB_InsOp_LocB_TOS, GB_InsOp_LocE_TOS, GB_InsOp_ImmSz_64) :
				GB_Push( GB_SPRelx(GB_PCImm(int64_t)) ) ;
				break ;

			/* l2tt08 */
			/* l2tt16 */
			/* l2tt32 */
			/* l2tt64 */
			
			/* callt */
			case GB_InsCall(GB_InsOp_LocB_TOS) :
				x = GB_TOS ;
				GB_TOS = Cast(GB_Word,pc) ;
				pc = Cast(GB_BytePtr,x) ;
				break ;
			
			/* callr */

			/* tailcallt */
			case GB_InsTailCall(GB_InsOp_LocB_TOS) :
				
				break ;
			
			/* tailcallr */

			default:
				panic( "instruction not implemented", *(pc-1) ) ;
				break ;

		}
	}
}

%%]