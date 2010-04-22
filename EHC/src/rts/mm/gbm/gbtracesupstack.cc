%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: GBStack
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
#include "../../bc/interpreter.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupply_GBStack_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) {
	MM_TraceSupply_GBStack_Data* trsup = memmgt->malloc( sizeof(MM_TraceSupply_GBStack_Data) ) ;
	trsup->trace = mutator->trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trsup ;
}

void mm_traceSupply_GBStack_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) {
	MM_TraceSupply_GBStack_Data* trsup = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	trsup->gcStackInfo = (GB_GCStackInfo*)gcStackInfo ;
	// trsup->gcInfo = (GB_GCInfo*)gcStackInfo ;
}
%%]

%%[8
// deal with part covered/ruled by gc stack info
static inline WPtr mm_traceSupply_GBStack_RunWithStackInfo( MM_TraceSupply_GBStack_Data* trsup, GB_GCStackInfo* info, WPtr bptr ) {
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo BEF gcStackInfo=%p b=%p\n",info,bptr);}) ;
	if ( info != NULL ) {
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo A gcStackInfo=%p, sz=%x ndesc=%x\n",info,info->sz,info->nrDescrs);}) ;
		Word descrInx ;
		for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) {
			Word descr = info->descrs[ descrInx ] ;
			Word sz = descr >> 1 ;
			if ( descr & 1 ) {
				IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo B gcStackInfo=%p, descr[%x]=%x, tracesz=%x bptr=%x(-1=%x)\n",info,descrInx,descr,sz,bptr,*(bptr-1));}) ;
				for ( ; sz > 0 ; sz-- ) {
					bptr-- ;
					*bptr = mm_Trace_TraceObject( trsup->trace, *bptr ) ;
				}
			} else {
				IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo C gcStackInfo=%p, descr[%x]=%x, skipsz=%x bptr=%x(-1=%x)\n",info,descrInx,descr,sz,bptr,*(bptr-1));}) ;
				bptr -= sz ;
			}
		}
	}
	return bptr ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo AFT gcStackInfo=%p b=%p\n",info,bptr);}) ;
}
%%]

%%[8
static inline WPtr mm_traceSupply_GBStack_RunStack( MM_TraceSupply_GBStack_Data* trsup, WPtr s, WPtr bptr ) {
	for ( ; s < bptr ; s++ ) {
		*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
	}
	return s ;
}
%%]

%%[8
void mm_traceSupply_GBStack_Run4( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trsup = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	WPtr s = sp ;
	WPtr b = bp ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 BEF s=%p(lo=%p, hi=%p, used=%x, unused=%x) b=%p\n",s,(WPtr)StackAreaLow,(WPtr)StackAreaHigh,(BPtr)StackAreaHigh-(BPtr)s,(BPtr)s-(BPtr)StackAreaLow,b);}) ;
	// initial part, todo: check for b == NULL
	WPtr bptr = mm_traceSupply_GBStack_RunWithStackInfo( trsup, trsup->gcStackInfo, b ) ;
	s = mm_traceSupply_GBStack_RunStack( trsup, s, bptr ) ;
	for ( ; *b != NULL ; ) {
		GB_CallInfo* ci = GB_FromBPToCallInfo(b) ;
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 A callinfo=%p(%s) s=%p b=%p\n",ci,ci->name,s,b);}) ;
		// skip ret info
		// s = b + 2 ;
		bptr = (WPtr)*b ;
		bptr = mm_traceSupply_GBStack_RunWithStackInfo( trsup, ci->gcStackInfo, bptr ) ;
		// deal with remaining part of stackframe, on top of the stack
		s = mm_traceSupply_GBStack_RunStack( trsup, b+2, bptr ) ;
		// to next frame
		b = (WPtr)*b ;
	}
	// remaining part
	s = mm_traceSupply_GBStack_RunStack( trsup, b+2, (WPtr)StackAreaHigh ) ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 AFT\n");}) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_GBStack =
	{ NULL
	, &mm_traceSupply_GBStack_Init
	, MM_Undefined
	, &mm_traceSupply_GBStack_Reset
	, &mm_traceSupply_GBStack_Run4
	, MM_Undefined
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_GBStack_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_GBStack_Test() {
}
#endif
%%]

