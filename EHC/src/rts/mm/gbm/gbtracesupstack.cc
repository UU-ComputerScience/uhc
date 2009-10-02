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
void mm_traceSupply_GBStack_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Trace* trace ) {
	MM_TraceSupply_GBStack_Data* trgr = memmgt->malloc( sizeof(MM_TraceSupply_GBStack_Data) ) ;
	trgr->trace = trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trgr ;
}

void mm_traceSupply_GBStack_Reset( MM_TraceSupply* traceSupply, Word gcInfo ) {
	MM_TraceSupply_GBStack_Data* trgr = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	trgr->gcInfo = (GB_GCInfo*)gcInfo ;
}
%%]

void mm_traceSupply_GBStack_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trgr = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	Word off = (trgr->gcInfo ? trgr->gcInfo->nrOfTOS_No_GCTrace : 0) ;
	// stackpointer
	WPtr s = GB_RegRelCast(Word,sp,off) ;
	// basepointer
	WPtr b = bp ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run A nrOfTOS_No_GCTrace=%d (gcInfo=%p) sp=%p s=%p *s=%x\n",off,trgr->gcInfo,sp,s,*s);}) ;
	for ( ; s < (WPtr)StackAreaHigh ; s++ ) {
		if ( b == s ) {
			// meta info of stackframe needs to be skipped
			b = (WPtr)*b ;
			s += 2 ;
		}
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run B sp=%p *s=%x\n",s,*s);}) ;
		*s = mm_Trace_TraceObject( trgr->trace, *s, MM_Trace_Flg_All ) ;
	}
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run C\n");}) ;
}

%%[8
%%]
void mm_traceSupply_GBStack_Run2( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trgr = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	Word off = (trgr->gcInfo ? trgr->gcInfo->nrOfTOS_No_GCTrace : 0) ;
	WPtr s = GB_RegRelCast(Word,sp,off) ;
	WPtr b = bp ;
	for ( ; b != NULL && b < (WPtr)StackAreaHigh ; ) {
		GB_CallInfo* ci = GB_FromBPToCallInfo(b) ;
		WPtr bptr = b ;
		// deal with part covered/ruled by gc stack info
		if ( ci->gcStackInfo != NULL ) {
			GB_GCStackInfo* info = ci->gcStackInfo ;
			Word descrInx ;
			for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) {
				Word8 descr = info->descrs[ descrInx ] ;
				Bool mustGC = descr & 1 ;
				Word8 sz = (descr >> 1) + 1 ;
				if ( mustGC ) {
					for ( ; sz > 0 ; sz-- ) {
						bptr-- ;
						*bptr = mm_Trace_TraceObject( trgr->trace, *bptr, MM_Trace_Flg_All ) ;
					}
				} else {
					bptr -= sz ;
				}
			}
		} else {
		}
		// deal with remaining part of stackframe, on top of the stack
		for ( ; s < bptr ; s++ ) {
			*s = mm_Trace_TraceObject( trgr->trace, *s, MM_Trace_Flg_All ) ;
		}
		// to next frame, skipping ret info
		s = b + 2 ;
		b = (WPtr)*b ;
	}
	// remaining part
	for ( ; s < (WPtr)StackAreaHigh ; s++ ) {
		*s = mm_Trace_TraceObject( trgr->trace, *s, MM_Trace_Flg_All ) ;
	}
}

%%[8
void mm_traceSupply_GBStack_Run3( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trgr = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	Word off = (trgr->gcInfo ? trgr->gcInfo->nrOfTOS_No_GCTrace : 0) ;
	WPtr s = GB_RegRelCast(Word,sp,off) ;
	WPtr b = bp ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 BEF s=%p(lo=%p,hi=%p,diff=%x) b=%p\n",s,(WPtr)StackAreaLow,(WPtr)StackAreaHigh,(BPtr)StackAreaHigh-(BPtr)StackAreaLow,b);}) ;
	// initial part
	for ( ; s < b ; s++ ) {
		*s = mm_Trace_TraceObject( trgr->trace, *s, MM_Trace_Flg_All ) ;
	}
	for ( ; b != NULL && b < (WPtr)StackAreaHigh ; ) {
		GB_CallInfo* ci = GB_FromBPToCallInfo(b) ;
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 A callinfo=%p(%s) s=%p b=%p\n",ci,ci->name,s,b);}) ;
		// skip ret info
		s = b + 2 ;
		WPtr bptr = (WPtr)*b ;
		// deal with part covered/ruled by gc stack info
		if ( ci->gcStackInfo != NULL ) {
			GB_GCStackInfo* info = ci->gcStackInfo ;
			IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 B gcStackInfo=%p, sz=%x\n",info,info->sz);}) ;
			Word descrInx ;
			for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) {
				Word8 descr = info->descrs[ descrInx ] ;
				Bool mustGC = descr & 1 ;
				Word8 sz = (descr >> 1) + 1 ;
				if ( mustGC ) {
					for ( ; sz > 0 ; sz-- ) {
						bptr-- ;
						*bptr = mm_Trace_TraceObject( trgr->trace, *bptr, MM_Trace_Flg_All ) ;
					}
				} else {
					bptr -= sz ;
				}
			}
		} else {
		}
		// deal with remaining part of stackframe, on top of the stack
		for ( ; s < bptr ; s++ ) {
			*s = mm_Trace_TraceObject( trgr->trace, *s, MM_Trace_Flg_All ) ;
		}
		// to next frame
		b = (WPtr)*b ;
	}
	// remaining part
	for ( ; s < (WPtr)StackAreaHigh ; s++ ) {
		*s = mm_Trace_TraceObject( trgr->trace, *s, MM_Trace_Flg_All ) ;
	}
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
	, &mm_traceSupply_GBStack_Run3
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

