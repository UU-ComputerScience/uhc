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

void mm_traceSupply_GBStack_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trgr = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	Word off = (trgr->gcInfo ? trgr->gcInfo->nrOfTOS_No_GCTrace : 0) ;
	// stackpointer
	WPtr s = GB_RegRelCast(Word,sp,off) ;
	// basepointer
	WPtr b = bp ;
	// how many to skip
	int skip = 0 ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run A nrOfTOS_No_GCTrace=%d (gcInfo=%p) sp=%p s=%p *s=%x\n",off,trgr->gcInfo,sp,s,*s);}) ;
	for ( ; s < (WPtr)StackAreaHigh ; s++ ) {
		/*
		if ( skip <= 0 && GB_Word_IsGC(*s) ) {
			skip = GB_Word_UnTag(*s) ;
			s++ ;
		}
		*/
		if ( b == s ) {
			// meta info of stackframe needs to be skipped
			b = (WPtr)*b ;
			s += 2 ;
			// skip -= 2 ;
		}
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run B sp=%p *s=%x skip=%d\n",s,*s,skip);}) ;
		/*
		if ( skip > 0 ) {
			skip-- ;
		} else {
		*/
			*s = mm_Trace_TraceObject( trgr->trace, *s, MM_Trace_Flg_All ) ;
		/*
		}
		*/
	}
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run C\n");}) ;
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
	, &mm_traceSupply_GBStack_Run
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

