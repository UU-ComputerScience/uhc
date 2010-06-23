%%[8
#ifndef __BASE_TYPES_H__
#define __BASE_TYPES_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GC specific information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Descriptor of what can and cannot be traced during GC, only known to the interpreter or other execution machine

%%[8
typedef struct GCStackInfo {
  Word16	sz ;	// size of stack fragment described by this info, in words
  Word8		nrDescrs ; 
  Word8*	descrs ; 
} __attribute__ ((__packed__)) GCStackInfo ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function specific information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Descriptor of info about functions

%%[8
typedef struct FunctionInfo {
  Word16	szStack ;	// size of stack required by function, in bytes
  Word8		flags ;
  Word8*	nm ; 		// name of function
} __attribute__ ((__packed__)) FunctionInfo ;

typedef SHalfWord		FunctionInfo_Inx ;

#define FunctionInfo_Inx_None						(-1)
%%]

%%[8
#define FunctionInfoFlag_None						0x0
#define FunctionInfoFlag_1stArgIsStackTrace			0x1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Call information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

After each call 'call information' is stored.
Invariant is that a return address points to the address immediately after this information.
The type and size used should agree with the code generation part.

%%[8
#ifdef __UHC_TARGET_BC__
typedef struct GB_CallInfo_CCall {
  char*		type ;
} GB_CallInfo_CCall ;
#endif

typedef struct CallInfo {
	Word8	 				kind ;
	Word8*   				name ;					// name of called function (to become obsolete when functionInfo works)
	// FunctionInfo*		functionInfo ;			// info about the called function (20100301 AD: under implementation)
	FunctionInfo_Inx		functionInfoModOff ;	// offset in imported module table, replaced at linking time with index into global module table
	FunctionInfo_Inx		functionInfoOff ;		// offset in per module FunctionInfo table
	GCStackInfo*			gcStackInfo ;
#if TRACE && defined(__UHC_TARGET_BC__)
	GB_CallInfo_CCall		ccall ;
#endif
} __attribute__ ((__packed__)) CallInfo ;

typedef CallInfo* CallInfoPtr ;

#define CallInfo_Inline						Word		// A CallInfoPtr, inlined after instruction, to be skipped by interpreter, used by exception handling & debugging

#if TRACE
#define MkCallInfoWith(k,n,mo,fo,gc,w)		{k,(BPtr)n,mo,fo,gc,w}		// make CallInfo
#else
#define MkCallInfoWith(k,n,mo,fo,gc,w)		{k,(BPtr)n,mo,fo,gc}		// make CallInfo
#endif
#define MkCallInfo(k,n)						MkCallInfoWith(k,n,-1,-1,NULL,NULL)

#define CallInfo_Fld_Kind(i)    			i

// the order must correspond to alternatives of CallInfoKind in ehc/GrinByteCode, extra ones may be at the end
#define CallInfo_Kind_Call    			0			// normal call
#define CallInfo_Kind_Tail    			1			// tail call
#define CallInfo_Kind_Eval    			2			// eval call
#define CallInfo_Kind_EvalWrap  		3			// eval call wrapper
#define CallInfo_Kind_TailEv  			4			// tail eval call
#define CallInfo_Kind_Apply   			5			// apply call
#define CallInfo_Kind_CCall   			6			// C call
#define CallInfo_Kind_EvCont  			7			// eval update continuation
#define CallInfo_Kind_ApCont  			8			// apply continuation
#define CallInfo_Kind_PApCont  			9			// partial apply continuation
#define CallInfo_Kind_Hdlr    			10			// exception handler installment
#define CallInfo_Kind_TailEval			11			// tail eval
// following may be defined freely
#define CallInfo_Kind_EvAppFunCont  	12			// apply fun eval update continuation
#define CallInfo_Kind_EvAppFunEvCont 	13			// apply fun eval update eval continuation
#define CallInfo_Kind_EvalTopWrap  		14			// top level eval call wrapper
#define CallInfo_Kind_TailEvalCont  	15			// return/cleanup after taileval
%%[[96
#define CallInfo_Kind_IntlCCall			16			// internal C call which must look like foreign function call (for exception handling)
%%]]
%%]

Flags

%%[8
%%]
#define CallInfo_Flag_None				0			// nothing, nada
#define CallInfo_Flag_ExplStackTrace		1			// this function takes as its first arg

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BASE_TYPES_H__ */
%%]


