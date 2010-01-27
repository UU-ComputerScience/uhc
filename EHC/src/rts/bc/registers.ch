%%[8
#ifndef __BC_REGISTERS_H__
#define __BC_REGISTERS_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Registers internal config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if defined(__GNUC__) && !defined(DEBUG)

// add additional settings if/when it works on your platform
#ifdef __i386__
#define USE_REGS_FOR_SP 		1
#define USE_REGS_FOR_PC 		1
#define USE_REGS_FOR_BP 		0
#else
#define USE_REGS_FOR_SP 		0
#define USE_REGS_FOR_PC 		0
#define USE_REGS_FOR_BP 		0
#endif
#define USE_REGS_FOR_PC_SP 		USE_REGS_FOR_SP && USE_REGS_FOR_PC && USE_REGS_FOR_BP

#endif

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Register usage (adapted from lvm evaluator.c)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if defined(__GNUC__) && !defined(DEBUG)
#ifdef __i386__
# if USE_REGS_FOR_BP
#  define BP_REG asm("%esi")
#  undef USE_REGS_FOR_PC
# else
#  define PC_REG asm("%esi")
# endif
# define SP_REG asm("%edi")
# define FP_REG
#endif
#ifdef __x86_64__
#define PC_REG asm("6")
#define SP_REG asm("7")
#define FP_REG
// #undef USE_REGS_FOR_PC_SP
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Registers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Registers:
pc: program counter
sp: stack pointer (used for temporaries, locals, expression calculation)
bp: base pointer (used for exception handling)
rr: user available scratch register

%%[8
#if USE_REGS_FOR_BP
register Word*  	 bp BP_REG ;
#else
extern   Word*       bp ;
#endif

#if USE_REGS_FOR_PC
register Word8*  	 pc PC_REG ;
#else
extern   Word8*  	 pc ;
#endif

#if USE_REGS_FOR_SP
register Word*       sp SP_REG ;
#else
extern   Word*       sp ;
#endif


#if defined(RR_REG) && USE_REGS_FOR_SP && USE_REGS_FOR_PC
register Word     	rr RR_REG ;
#else
extern   Word     	rr ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BC_REGISTERS_H__ */
%%]
