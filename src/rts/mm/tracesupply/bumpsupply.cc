%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: Bump
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupply_Bump_Reset( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Bump_Data* trgr = (MM_TraceSupply_Bump_Data*)traceSupply->data ;
	trgr->lastTraced = trgr->lastPushed = (Word*)trgr->trace->allocator->lastAllocLocation( trgr->trace->allocator ) ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_Reset lastPushed=%x lastTraced=%x\n",trgr->lastPushed,trgr->lastTraced);}) ;
}

void mm_traceSupply_Bump_Init( MM_TraceSupply* traceSupply, MM_Trace* trace ) {
	MM_TraceSupply_Bump_Data* trgr = mm_malloc_LOF.malloc( sizeof(MM_TraceSupply_Bump_Data) ) ;
	trgr->trace = trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trgr ;
	// mm_traceSupply_Bump_Reset( traceSupply ) ;
}

void mm_traceSupply_Bump_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Bump_Data* trgr = (MM_TraceSupply_Bump_Data*)traceSupply->data ;

	mm_traceSupply_Bump_Reset( traceSupply ) ;
	
	while ( trgr->lastPushed < trgr->lastTraced ) {
		// lastPushed + lastTraced may change as side effect of tracing
		Word* lastPushed = trgr->lastPushed ;
		Word* lastTraced = trgr->lastTraced ;
		Word* lastTracedNew = trgr->lastPushed ;
		for ( ; lastPushed < lastTraced ; ) {
			Word szWords = trgr->trace->objectNrWords( trgr->trace, (Word)lastPushed ) ;
			Word hdrSz = trgr->trace->objectHeaderNrWords ;
			IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_Run obj=%x sz=%x tr=%x last=%x lPush=%x lTrace=%x\n",lastPushed,szWords,lastPushed + hdrSz,lastTraced,trgr->lastPushed,trgr->lastTraced);}) ;
			trgr->trace->traceObjects( trgr->trace, lastPushed + hdrSz, szWords - hdrSz ) ;
			lastPushed += szWords ;
		}
		trgr->lastTraced = lastTracedNew ;
	}
}

// push ptr + size
void mm_traceSupply_Bump_PushWork( MM_TraceSupply* traceSupply, Word* work, Word nrWorkWords ) {
	MM_TraceSupply_Bump_Data* trgr = (MM_TraceSupply_Bump_Data*)traceSupply->data ;
	trgr->lastPushed = work ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_Bump =
	{ NULL
	, &mm_traceSupply_Bump_Init
	, MM_Undefined
	, &mm_traceSupply_Bump_Reset
	, &mm_traceSupply_Bump_Run
	, &mm_traceSupply_Bump_PushWork
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Bump_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Bump_Test() {
}
#endif
%%]

